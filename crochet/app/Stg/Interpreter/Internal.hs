module Stg.Interpreter.Internal where

import Stg.Interpreter.Types
import Stg.Stg
import Stg.Interpreter.Utils
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace
import Stg.FreeVariables
import Stg.BoundVariables
import Stg.Pretty

throw :: String -> InterpreterResult
throw text = Failure $ InterpreterException text

todo :: InterpreterResult
todo = throw "Not implemented yet"

step expression stack heap heapPointer environment i =
    Step $ MachineState {
        machineExpression = expression,
        machineStack = stack,
        machineHeap = heap,
        machineHeapPointer = heapPointer,
        machineEnvironment = environment,
        machineStep = i
    }

resolveVariable environment variable =
    case Map.lookup variable environment of
        Just address -> address

getAddress :: Environment -> Atom -> HeapAddress
getAddress environment atom =
    case atom of
        Literal (Integer n) -> HeapAddress n
        Literal (Address address) -> HeapAddress address
        Variable name -> resolveVariable environment name

-- Memory management
allocate :: Heap -> HeapAddress -> HeapObject -> (Heap, HeapAddress)
allocate heap heapPointer@(HeapAddress heapPointerAddress) heapObject =
    let
        heap' = Map.insert heapPointer heapObject heap
        heapPointer' = HeapAddress (heapPointerAddress + 1)
    in
        (heap', heapPointer')

allocateMany :: Heap -> HeapAddress -> Map.Map String HeapAddress -> Program -> (Heap, HeapAddress, Map.Map String HeapAddress)
allocateMany initialHeap initialHeapPointer initialEnvironment =
    foldr (\(Binding name object) (heap, heapPointer, environment) ->
        let
            environment' = Map.insert name heapPointer environment
            (heap', heapPointer') = allocate heap heapPointer (HeapObject object Map.empty)
        in
            (heap', heapPointer', environment')
    ) (initialHeap, initialHeapPointer, initialEnvironment)

initializeTopLevelObjects :: Program -> (Heap, HeapAddress, Map.Map String HeapAddress)
initializeTopLevelObjects bindings =
    let
        topLevelNames = map (\(Binding name object) -> name) bindings
        topLevelEnvironment = Map.fromList [(name, HeapAddress 0) | name <- topLevelNames]

        (finalHeap, finalHeapPointer, finalEnvironment) = foldr (\(Binding name object) (heap, heapPointer, environment) ->
                let
                    environment' = Map.insert name heapPointer environment
                    heapObject = HeapObject object Map.empty
                    (heap', heapPointer') = allocate heap heapPointer heapObject
                in
                    (heap', heapPointer', environment')
            ) (Map.empty, HeapAddress 0, topLevelEnvironment) bindings
        
        finalHeap' = Map.map (\(HeapObject object _) ->
            let
                requiredVariables = freeVariables object
                freeVariablesEnvironment = Map.restrictKeys finalEnvironment requiredVariables

                definedVariables = Set.toList (boundVariables object)
                localVariablesEnvironment = Map.fromList [(key, HeapAddress 0) | key <- definedVariables]

                closureEnvironment = Map.union freeVariablesEnvironment localVariablesEnvironment
            in
                HeapObject object closureEnvironment
            ) finalHeap
    in
        (finalHeap', finalHeapPointer, finalEnvironment)

-- Rule LET
evaluateExpression :: MachineState -> InterpreterResult
evaluateExpression MachineState {
    machineExpression = e@(LetIn name value body),
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment,
    machineStep = i
} =
    let
        -- Which variables are actually needed inside of the value?
        -- We should **only add those to the environment** to reduce memory consumption.
        requiredVariables = freeVariables value
        freeVariablesEnvironment = Map.restrictKeys environment requiredVariables

        definedVariables = Set.toList (boundVariables value)
        localVariablesEnvironment = Map.fromList [(key, HeapAddress 0) | key <- definedVariables]

        closureEnvironment = Map.union freeVariablesEnvironment localVariablesEnvironment

        -- Allocate a new heap closure
        environment' = Map.insert name heapPointer environment
        heapObject = HeapObject value closureEnvironment

        (heap', heapPointer') = allocate heap heapPointer heapObject
    in
        step body stack heap' heapPointer' environment' (i + 1)

-- Rule CASECON / CASEANY
evaluateExpression MachineState {
    machineExpression = (CaseOf (Atom (Literal (Address address))) alternatives),
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment,
    machineStep = i
} = -- | Map.member (HeapAddress address) heap
    case Map.lookup (HeapAddress address) heap of
        Nothing -> throw "The address doesn't point to CON object."
        Just (HeapObject (Constructor name arguments) closureEnvironment) ->
            case getAlgebraicAlternative name alternatives of

                -- Rule CASECON
                Just (AlgebraicAlternative constructor parameters body) ->
                    -- There is an alternative with constructor [name].
                    let
                        environment' = Map.union environment closureEnvironment

                        newVariables = Map.fromList (zip parameters (map (getAddress environment') arguments))
                        environment'' = Map.union environment' newVariables
                    in
                        step body stack heap heapPointer environment'' (i + 1)

                -- Rule CASEANY
                _ ->
                    case getDefaultAlternative alternatives of
                        Just (DefaultAlternative variable body) ->
                            -- Replace variable x in body with the literal
                            -- value v (e.g. body[v / x]).
                            let environment' = Map.insert variable (HeapAddress address) environment in
                                step body stack heap heapPointer environment' (i + 1)
                        _ -> throw $ "No algebraic alternatives match constructor '" ++ name ++ "' and no default alternative provided."

-- Rule CASEANY
evaluateExpression MachineState {
    machineExpression = (CaseOf (Atom (Literal (Integer n))) alternatives),
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment,
    machineStep = i
} =
    case getDefaultAlternative alternatives of
        Just (DefaultAlternative variable body) ->
            -- Replace variable x in body with the literal
            -- value v (e.g. body[v / x]).
            let environment' = Map.insert variable (HeapAddress n) environment in
                step body stack heap heapPointer environment' (i + 1)
        _ -> throw "Case of expression has no default alternative"

-- Rule CASE
evaluateExpression MachineState {
    machineExpression = (CaseOf scrutinee alternatives),
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment,
    machineStep = i
} =
    let
        stack' = CaseContinuation alternatives : stack
    in
        step scrutinee stack' heap heapPointer environment (i + 1)

-- Rule RET
evaluateExpression MachineState {
    machineExpression = atom@(Atom (Literal (Address a))),
    machineStack = (CaseContinuation alternatives) : stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment,
    machineStep = i
} =
    -- TODO: Check if heap[address] is a value!
    step (CaseOf atom alternatives) stack heap heapPointer environment (i + 1)

-- Rule RET
evaluateExpression MachineState {
    machineExpression = atom@(Atom (Literal n)),
    machineStack = (CaseContinuation alternatives) : stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment,
    machineStep = i
} =
    step (CaseOf atom alternatives) stack heap heapPointer environment (i + 1)

-- Custom rule VAR
evaluateExpression MachineState {
    machineExpression = (Atom (Variable name)),
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment,
    machineStep = i
} =
    case Map.lookup name environment of
        Nothing -> throw $ "Variable '" ++ name ++ "' not in scope."
        Just (HeapAddress address) ->
            step (Atom (Literal (Address address))) stack heap heapPointer environment (i + 1)

-- Rule THUNK
evaluateExpression MachineState {
    machineExpression = (Atom (Literal (Address address))),
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment,
    machineStep = i
} | case Map.lookup (HeapAddress address) heap of
        Just (HeapObject (Thunk _) _) -> True
        _ -> False
    =
        let
            Just (HeapObject (Thunk expression) closureEnvironment) = Map.lookup (HeapAddress address) heap
            stack' = UpdateContinuation (HeapAddress address) : stack
            heap' = Map.insert (HeapAddress address) (HeapObject BlackHole Map.empty) heap
        in
            step expression stack' heap' heapPointer closureEnvironment (i + 1)

-- Custom rule INDIRECTION
evaluateExpression MachineState {
    machineExpression = (Atom (Literal (Address address))),
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment,
    machineStep = i
} | case Map.lookup (HeapAddress address) heap of
        Just (HeapObject (Indirection _) _) -> True
        _ -> False
    =
        let
            Just (HeapObject (Indirection otherAddress) _) = Map.lookup (HeapAddress address) heap
            expression' = Atom (Literal (Address otherAddress))
        in
            step expression' stack heap heapPointer environment (i + 1)

-- Rule UPDATE
evaluateExpression MachineState {
    machineExpression = expression@(Atom (Literal (Address address))),
    machineStack = (UpdateContinuation updateAddress) : stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment,
    machineStep = i
} =
    -- TODO: Check if H[address] is a value
    case Map.lookup (HeapAddress address) heap of
        Just heapObject ->
            let
                heap' = Map.insert updateAddress (HeapObject (Indirection address) Map.empty) heap
            in
                step expression stack heap' heapPointer environment (i + 1)

-- Rule KNOWNCALL
-- evaluateExpression MachineState {
--     machineExpression = expression@(FunctionApplication function (Known n) arguments),
--     machineStack = stack,
--     machineHeap = heap,
--     machineHeapPointer = heapPointer,
--     machineEnvironment = environment,
--     machineStep = i
-- } | case Map.lookup (getAddress environment function) heap of
--         Just (HeapObject (Function parameters _) _) -> length parameters == n
--         _ -> False
--     =
--         let
--             functionAddress = getAddress environment function
--             Just (HeapObject (Function parameters body) closureEnvironment) = Map.lookup functionAddress heap
--         in
--             todo

-- Rule PRIMOP

-- Rule EXACT
-- evaluateExpression MachineState {
--     machineExpression = expression@(FunctionApplication function Unknown arguments),
--     machineStack = stack,
--     machineHeap = heap,
--     machineHeapPointer = heapPointer,
--     machineEnvironment = environment,
--     machineStep = i
-- } | case Map.lookup (getAddress environment function) heap of
--         Just (HeapObject (Function parameters _) _) -> length parameters == length arguments
--         _ -> False
--     =
--         let
--             functionAddress = getAddress environment function
--             Just (HeapObject (Function parameters body) closureEnvironment) = Map.lookup functionAddress heap
--         in
--             let
--                 environment' = Map.union environment closureEnvironment

--                 newVariables = Map.fromList (zip parameters (map (getAddress environment') arguments))
--                 environment'' = Map.union environment' newVariables
--             in
--                 step body stack heap heapPointer environment'' (i + 1)

-- Rule TCALL
-- evaluateExpression MachineState {
--     machineExpression = expression@(FunctionApplication function Unknown arguments),
--     machineStack = stack,
--     machineHeap = heap,
--     machineHeapPointer = heapPointer,
--     machineEnvironment = environment,
--     machineStep = i
-- } | case Map.lookup (getAddress environment function) heap of
--         Just (HeapObject (Thunk _) _) -> True
--         _ -> False
--     =
--         let
--             expression' = Atom function
--             stack' = ApplyContinuation arguments : stack
--         in
--             step expression' stack' heap heapPointer environment (i + 1)

-- Rule PCALL
-- evaluateExpression MachineState {
--     machineExpression = expression@(FunctionApplication function Unknown arguments),
--     machineStack = stack,
--     machineHeap = heap,
--     machineHeapPointer = heapPointer,
--     machineEnvironment = environment,
--     machineStep = i
-- } | case Map.lookup (getAddress environment function) heap of
--         Just (HeapObject (Thunk _) _) -> True
--         _ -> False
--     =
--         todo

-- Rule RETFUN
-- evaluateExpression MachineState {
--     machineExpression = expression@(Atom (Literal (Address functionAddress))),
--     machineStack = (ApplyContinuation arguments) : stack,
--     machineHeap = heap,
--     machineHeapPointer = heapPointer,
--     machineEnvironment = environment,
--     machineStep = i
-- } | case Map.lookup (HeapAddress functionAddress) heap of
--         Just (HeapObject (Function _ _) _) -> True
--         Just (HeapObject (PartialApplication _ _) _) -> True
--         _ -> False
--     =
--         let
--             expression' = FunctionApplication (Literal (Address functionAddress)) Unknown arguments
--         in
--             step expression' stack heap heapPointer environment (i + 1)

evaluateExpression state@MachineState {
    machineExpression = expression,
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment,
    machineStep = i
} =
    Done state