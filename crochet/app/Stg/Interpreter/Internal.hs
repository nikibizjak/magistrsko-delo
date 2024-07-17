module Stg.Interpreter.Internal where

import Stg.Stg
import Stg.Interpreter.Types
import Stg.Interpreter.Utils
import Stg.Interpreter.Memory
import qualified Data.Map as Map
import qualified Data.Set as Set
import Stg.FreeVariables
import Stg.BoundVariables
import Stg.Pretty
import Data.Maybe (isJust)

-- Rule LET
evaluateExpression :: MachineState -> InterpreterResult
evaluateExpression state@MachineState {
    machineExpression = (LetIn name value body),
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment
} =
    let
        -- Which variables are actually needed inside of the value?
        -- We should **only add those to the environment** to reduce memory consumption.
        requiredVariables = freeVariables value
        closureEnvironment = Map.restrictKeys environment requiredVariables

        -- Allocate a new heap closure
        environment' = Map.insert name (MemoryAddress heapPointer) environment
        heapObject = HeapObject value closureEnvironment

        (heap', heapPointer') = allocate heap heapPointer heapObject
    in
        continue state {
            machineExpression = traceStep "LET" state body,
            machineHeap = heap',
            machineHeapPointer = heapPointer',
            machineEnvironment = environment'
        }

-- Rule CASECON
evaluateExpression state@MachineState {
    machineExpression = (CaseOf (Atom (Literal (Address address))) alternatives),
    machineHeap = heap,
    machineEnvironment = environment
} | case heapLookup (HeapAddress address) heap of
        Just (HeapObject (Constructor name _) _) ->
            isJust $ getAlgebraicAlternative name alternatives
        _ -> False
    =
        let
            -- Get the CON object and matching alternative. We are sure that
            -- they exist because it was matched in the guard condition.
            Just (HeapObject (Constructor name arguments) closureEnvironment) = heapLookup (HeapAddress address) heap
            Just (AlgebraicAlternative constructor parameters body) = getAlgebraicAlternative name alternatives

            environment' = Map.union closureEnvironment environment

            argumentValues = map (getEnvironmentValue environment') arguments
            parameterEnvironment = Map.fromList (zip parameters argumentValues)
            environment'' = Map.union parameterEnvironment environment'
        in
            continue state {
                machineExpression = traceStep "CASECON" state body,
                machineEnvironment = environment''
            }

-- Rule CASEANY where H[v] is a value
evaluateExpression state@MachineState {
    machineExpression = (CaseOf (Atom (Literal (Address address))) alternatives),
    machineHeap = heap,
    machineEnvironment = environment
} | isJustHeapObjectValue (heapLookup (HeapAddress address) heap) =
    case getDefaultAlternative alternatives of
        Just (DefaultAlternative variable body) ->
            -- Replace variable x in body with the literal value v (e.g.
            -- body[v / x]).
            let environment' = Map.insert variable (MemoryAddress (HeapAddress address)) environment in
                continue state {
                    machineExpression = traceStep "CASEANY" state body,
                    machineEnvironment = environment'
                }
        _ -> throw "No default alternative provided."

-- Rule CASEANY where v is a literal
evaluateExpression state@MachineState {
    machineExpression = (CaseOf (Atom (Literal (Integer n))) alternatives),
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment
} =
    case getDefaultAlternative alternatives of
        Just (DefaultAlternative variable body) ->
            -- Replace variable x in body with the literal value v (e.g. body[v
            -- / x]).
            let environment' = Map.insert variable (MemoryInteger n) environment in
                continue state {
                    machineExpression = traceStep "CASEANY" state body,
                    machineEnvironment = environment'
                }
        _ -> throw "No default alternative provided."

-- Rule CASE
evaluateExpression state@MachineState {
    machineExpression = (CaseOf scrutinee alternatives),
    machineStack = stack,
    machineEnvironment = environment
} =
    let
        requiredVariables = Set.unions (map freeVariables alternatives)
        closureEnvironment = Map.restrictKeys environment requiredVariables

        stack' = CaseContinuation alternatives closureEnvironment : stack
    in
        continue state {
            machineExpression = traceStep "CASE" state scrutinee,
            machineStack = stack'
        }

-- Custom rule INDIRECTION (follows an indirection)
evaluateExpression state@MachineState {
    machineExpression = (Atom (Literal (Address address))),
    machineHeap = heap
} | isJustIndirection (heapLookup (HeapAddress address) heap)
    =
    let
        Just (Indirection (HeapAddress otherAddress)) = heapLookup (HeapAddress address) heap
    in
        continue state {
            machineExpression = traceStep "INDIRECTION" state Atom (Literal (Address otherAddress))
        }

-- Rule RET where H[v] is a value
evaluateExpression state@MachineState {
    machineExpression = atom@(Atom (Literal (Address address))),
    machineStack = (CaseContinuation alternatives closureEnvironment) : stack',
    machineHeap = heap,
    machineEnvironment = environment
} | isJustHeapObjectValue (heapLookup (HeapAddress address) heap) =
    continue state {
        machineExpression = traceStep "RET" state CaseOf atom alternatives,
        machineStack = stack',
        machineEnvironment = Map.union closureEnvironment environment
    }

-- Rule RET where v is a literal
evaluateExpression state@MachineState {
    machineExpression = atom@(Atom literal@(Literal n)),
    machineStack = (CaseContinuation alternatives closureEnvironment) : stack',
    machineEnvironment = environment
} | isLiteral environment literal =
    continue state {
        machineExpression = traceStep "RET" state CaseOf atom alternatives,
        machineStack = stack',
        machineEnvironment = Map.union closureEnvironment environment 
    }

-- Rule THUNK
evaluateExpression state@MachineState {
    machineExpression = (Atom (Literal (Address address))),
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment,
    machineStep = i
} | isJustThunk (heapLookup (HeapAddress address) heap) =
    let
        Just (HeapObject (Thunk expression) closureEnvironment) = Map.lookup (HeapAddress address) heap
        stack' = UpdateContinuation (HeapAddress address) : stack
        heap' = Map.insert (HeapAddress address) (HeapObject BlackHole Map.empty) heap
    in
        continue state {
            machineExpression = traceStep "THUNK" state expression,
            machineStack = stack',
            machineHeap = heap',
            machineEnvironment = closureEnvironment
        }

-- Rule UPDATE
evaluateExpression state@MachineState {
    machineExpression = expression@(Atom (Literal (Address address))),
    machineStack = (UpdateContinuation updateAddress) : stack',
    machineHeap = heap,
    machineEnvironment = environment
} | isJustHeapObjectValue (heapLookup (HeapAddress address) heap) =
    let
        (Just heapObject) = heapLookup (HeapAddress address) heap
        heap' = Map.insert updateAddress (Indirection (HeapAddress address)) heap
    in
        continue state {
            machineExpression = traceStep "UPDATE" state expression,
            machineHeap = heap',
            machineStack = stack'
        }

-- Rule KNOWNCALL
evaluateExpression state@MachineState {
    machineExpression = FunctionApplication function (Known n) arguments,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment
} | case heapLookupVariable function environment heap of
        Just (HeapObject (Function parameters _) _) -> (length parameters == n) && (length arguments == n)
        _ -> False
    =
        let
            Just (HeapObject (Function parameters body) closureEnvironment) = heapLookupVariable function environment heap

            argumentValues = map (getEnvironmentValue environment) arguments
            parameterEnvironment = Map.fromList (zip parameters argumentValues)
            environment' = Map.union parameterEnvironment closureEnvironment
        in
            continue state {
                machineExpression = traceStep "KNOWNCALL" state body,
                machineEnvironment = environment'
            }

-- Rule PRIMOP
evaluateExpression state@MachineState {
    machineExpression = PrimitiveOperation operation arguments,
    machineEnvironment = environment,
    machineHeap = heap,
    machineHeapPointer = heapPointer
} =
    case arguments of

        -- Binary operations on integers
        [left, right] | operation `elem` [Addition, Subtraction, Multiplication, Division, Modulo] ->
            let
                (MemoryInteger leftValue) = getEnvironmentValue environment left
                (MemoryInteger rightValue) = getEnvironmentValue environment right

                f = case operation of
                    Addition -> (+)
                    Subtraction -> (-)
                    Multiplication -> (*)
                    Division -> div
                    Modulo -> mod
                
                result = f leftValue rightValue
                atomicResult = Atom (Literal (Integer result))
            in
                continue state { machineExpression = traceStep "PRIMOP" state atomicResult }
        
        -- Binary comparison operations
        [left, right] | operation `elem` [GreaterThanOrEqual, GreaterThan, Equal, LessThan, LessThanOrEqual, NotEqual] ->
            let
                (MemoryInteger leftValue) = getEnvironmentValue environment left
                (MemoryInteger rightValue) = getEnvironmentValue environment right

                f = case operation of
                    GreaterThanOrEqual -> (>=)
                    GreaterThan -> (>)
                    Equal -> (==)
                    LessThan -> (<)
                    LessThanOrEqual -> (<=)
                    NotEqual -> (/=)
                
                result = f leftValue rightValue

                -- Allocate a new CON object (True or False) based on the result
                -- of the comparison.
                object = Constructor (if result then "True" else "False") []
                heapObject = HeapObject object Map.empty
                (HeapAddress heapPointerAddress) = heapPointer
                (heap', heapPointer') = allocate heap heapPointer heapObject

                expression = Atom (Literal (Address heapPointerAddress))
            in
                continue state {
                    machineExpression = traceStep "PRIMOP" state expression,
                    machineHeap = heap',
                    machineHeapPointer = heapPointer'
                }
        
        _ -> throw "Invalid usage of primitive operation."

-- Rule EXACT
evaluateExpression state@MachineState {
    machineExpression = FunctionApplication function Unknown arguments,
    machineHeap = heap,
    machineEnvironment = environment
} | case heapLookupVariable function environment heap of
        Just (HeapObject (Function parameters _) _) -> length arguments == length parameters
        _ -> False
    =
    let
        Just (HeapObject (Function parameters body) closureEnvironment) = heapLookupVariable function environment heap

        -- TODO: Do we actually need to merge environments?
        environment' = Map.union environment closureEnvironment

        newVariables = Map.fromList (zip parameters (map (getEnvironmentValue environment') arguments))
        environment'' = Map.union newVariables environment'
    in
        continue state {
            machineExpression = traceStep "EXACT" state body,
            machineEnvironment = environment''
        }

-- Rule CALLK
evaluateExpression state@MachineState {
    machineExpression = FunctionApplication function (Known k) arguments,
    machineStack = stack,
    machineHeap = heap,
    machineEnvironment = environment
} | case heapLookupVariable function environment heap of
        Just (HeapObject (Function parameters _) _) -> length arguments > length parameters
        _ -> False
    =
        let
            Just (HeapObject (Function parameters body) closureEnvironment) = heapLookupVariable function environment heap

            -- Not all of the arguments are used. Only the first n = #
            -- parameters are used, the rest should be added to the stack as a
            -- continuation.
            argumentValues = map (getEnvironmentValue environment) arguments
            (usedArguments, remainingArguments) = splitAt (length parameters) argumentValues

            parameterEnvironment = Map.fromList (zip parameters usedArguments)
            environment' = Map.union parameterEnvironment closureEnvironment

            stack' = ApplyContinuation remainingArguments : stack
        in
            continue state {
                machineExpression = traceStep "CALLK" state body,
                machineStack = stack',
                machineEnvironment = environment'
            }

-- Rule PAP2
evaluateExpression state@MachineState {
    -- TODO: Must it always be
    machineExpression = FunctionApplication function (Known k) arguments,
    -- machineExpression = FunctionApplication function _ arguments,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment
} | case heapLookupVariable function environment heap of
        Just (HeapObject (Function parameters _) _) -> length arguments < length parameters
        _ -> False
    =
        let
            Just (HeapObject (Function parameters body) closureEnvironment) = heapLookupVariable function environment heap

            -- There are not enough parameters. Construct a new partial
            -- application object in the heap.
            partialApplication = HeapObject (PartialApplication function arguments) environment
            (HeapAddress partialApplicationAddress) = heapPointer
            (heap', heapPointer') = allocate heap heapPointer partialApplication
        in
            continue state {
                machineExpression = traceStep "PAP2" state Atom (Literal (Address partialApplicationAddress)),
                machineHeap = heap',
                machineHeapPointer = heapPointer'
            }

-- Rule TCALL
evaluateExpression state@MachineState {
    machineExpression = FunctionApplication function Unknown arguments,
    machineStack = stack,
    machineHeap = heap,
    machineEnvironment = environment
} | isJustThunk (heapLookupVariable function environment heap) =
    let
        Just (HeapObject (Thunk expression) closureEnvironment) = heapLookupVariable function environment heap

        stack' = ApplyContinuation (map (getEnvironmentValue environment) arguments) : stack
    in
        continue state {
            machineExpression = traceStep "TCALL" state Atom (Variable function),
            machineStack = stack'
        }

-- Rule PCALL
evaluateExpression state@MachineState {
    -- TODO: Must it always be
    machineExpression = FunctionApplication function (Known k) arguments,
    -- machineExpression = FunctionApplication function _ arguments,
    machineHeap = heap,
    machineEnvironment = environment
} | isJustPartialApplication (heapLookupVariable function environment heap) =
    let
        Just (HeapObject (PartialApplication g args) closureEnvironment) = heapLookupVariable function environment heap

        arguments' = args ++ arguments
        expression' = FunctionApplication g Unknown arguments'

        -- TODO: Do we actually need to merge environments?
        environment' = Map.union environment closureEnvironment
    in
        continue state {
            machineExpression = traceStep "PCALL" state expression',
            machineEnvironment = environment'
        }

-- Rule RETFUN
evaluateExpression state@MachineState {
    machineExpression = Atom (Variable function),
    machineStack = (ApplyContinuation arguments) : stack',
    machineHeap = heap,
    machineEnvironment = environment
} | case heapLookupVariable function environment heap of
        Just (HeapObject (Function _ _) _) -> True
        Just (HeapObject (PartialApplication _ _) _) -> True
        _ -> False
    =
    let
        atomicArguments = map memoryValueToAtom arguments
        expression' = FunctionApplication function Unknown atomicArguments
    in
        continue state {
            machineExpression = traceStep "RETFUN" state expression',
            machineStack = stack'
        }

-- Custom rule VAR (converts variables to their addresses)
evaluateExpression state@MachineState {
    machineExpression = (Atom (Variable name)),
    machineEnvironment = environment
} =
    case Map.lookup name environment of
        Nothing -> throw $ "Variable '" ++ name ++ "' not in scope."
        Just (MemoryInteger n) ->
            continue state {
                machineExpression = traceStep "VAR" state Atom (Literal (Integer n))
            }
        Just (MemoryAddress (HeapAddress address)) ->
            continue state {
                machineExpression = traceStep "VAR" state Atom (Literal (Address address))
            }

evaluateExpression state@MachineState {
    machineExpression = expression,
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment,
    machineStep = i
} =
    Done state