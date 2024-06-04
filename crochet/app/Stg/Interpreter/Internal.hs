module Stg.Interpreter.Internal where

import Stg.Interpreter.Types
import Stg.Stg
import Stg.Interpreter.Utils
import qualified Data.Map as Map

throw :: String -> InterpreterResult
throw text = Failure $ InterpreterException text

todo :: InterpreterResult
todo = throw "Not implemented yet"

step expression stack heap heapPointer environment =
    Step $ MachineState {
        machineExpression = expression,
        machineStack = stack,
        machineHeap = heap,
        machineHeapPointer = heapPointer,
        machineEnvironment = environment
    }

allocate :: Heap -> HeapAddress -> HeapObject -> (Heap, HeapAddress)
allocate heap heapPointer@(HeapAddress heapPointerAddress) heapObject =
    let
        heap' = Map.insert heapPointer heapObject heap
        heapPointer' = HeapAddress (heapPointerAddress + 1)
    in
        (heap', heapPointer')

-- Rule LET
evaluateExpression :: MachineState -> InterpreterResult
evaluateExpression MachineState {
    machineExpression = (LetIn name value body),
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment
} =
    let
        -- Allocate a new heap closure
        environment' = Map.insert name heapPointer environment
        heapObject = HeapObject value environment'

        (heap', heapPointer') = allocate heap heapPointer heapObject
    in
        step body stack heap' heapPointer' environment'

-- Rule CASECON / CASEANY
evaluateExpression MachineState {
    machineExpression = (CaseOf (Atom (Literal (Address address))) alternatives),
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment
} = -- | Map.member (HeapAddress address) heap
    case Map.lookup (HeapAddress address) heap of
        Nothing -> throw "The address doesn't point to CON object."
        Just (HeapObject (Constructor name arguments) closureEnvironment) ->
            -- Rule CASECON
            case getAlgebraicAlternative name alternatives of
                Just (AlgebraicAlternative constructor arguments body) ->
                    -- Is there an alternative with constructor with name [name]?
                    todo
                _ ->
                    -- Rule CASEANY
                    case getDefaultAlternative alternatives of
                        Just (DefaultAlternative variable body) ->
                            -- Replace variable x in body with the literal
                            -- value v (e.g. body[v / x]).
                            let environment' = Map.insert variable (HeapAddress address) environment in
                                step body stack heap heapPointer environment'
                        _ -> throw $ "No algebraic alternatives match constructor '" ++ name ++ "' and no default alternative provided."

-- Rule CASEANY
evaluateExpression MachineState {
    machineExpression = (CaseOf (Atom (Literal (Integer n))) alternatives),
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment
} =
    case getDefaultAlternative alternatives of
        Just (DefaultAlternative variable body) ->
            -- Replace variable x in body with the literal
            -- value v (e.g. body[v / x]).
            let environment' = Map.insert variable (HeapAddress n) environment in
                step body stack heap heapPointer environment'
        _ -> throw "Case of expression has no default alternative"

-- Rule CASE
evaluateExpression MachineState {
    machineExpression = (CaseOf scrutinee alternatives),
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment
} =
    let
        stack' = CaseContinuation alternatives : stack
    in
        step scrutinee stack' heap heapPointer environment

-- Rule RET
evaluateExpression MachineState {
    machineExpression = atom@(Atom (Literal (Address a))),
    machineStack = (CaseContinuation alternatives) : stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment
} =
    -- TODO: Check if heap[address] is a value!
    step (CaseOf atom alternatives) stack heap heapPointer environment

-- Rule RET
evaluateExpression MachineState {
    machineExpression = atom@(Atom (Literal n)),
    machineStack = (CaseContinuation alternatives) : stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment
} =
    step (CaseOf atom alternatives) stack heap heapPointer environment

-- Rule RET
-- evaluateExpression MachineState {
--     machineExpression = atom@(Atom (Variable name)),
--     machineStack = (CaseContinuation alternatives) : stack,
--     machineHeap = heap,
--     machineHeapPointer = heapPointer,
--     machineEnvironment = environment
-- } =
--     case lookup name environment of
--         Nothing -> throw $ "Variable '" ++ name ++ "' not in scope."
--         Just (HeapAddress address) ->
--             step (CaseOf (Atom $ Literal $ Address address) alternatives) stack heap heapPointer environment

-- Custom rule VAR
evaluateExpression MachineState {
    machineExpression = (Atom (Variable name)),
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment
} =
    case Map.lookup name environment of
        Nothing -> throw $ "Variable '" ++ name ++ "' not in scope."
        Just (HeapAddress address) ->
            step (Atom (Literal (Address address))) stack heap heapPointer environment

-- Rule THUNK
evaluateExpression MachineState {
    machineExpression = (Atom (Literal (Address address))),
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment
} | case Map.lookup (HeapAddress address) heap of
        Just (HeapObject (Thunk _) _) -> True
        _ -> False
    =
        let
            Just (HeapObject (Thunk expression) closureEnvironment) = Map.lookup (HeapAddress address) heap
            stack' = UpdateContinuation (HeapAddress address) : stack
            heap' = Map.insert (HeapAddress address) (HeapObject BlackHole Map.empty) heap
        in
            step expression stack' heap' heapPointer environment

-- Rule UPDATE
evaluateExpression MachineState {
    machineExpression = expression@(Atom (Literal (Address address))),
    machineStack = (UpdateContinuation updateAddress) : stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment
} =
    -- TODO: Check if H[address] is a value
    case Map.lookup (HeapAddress address) heap of
        Just heapObject ->
            let
                heap' = Map.insert updateAddress heapObject heap
            in
                step expression stack heap' heapPointer environment

evaluateExpression state@MachineState {
    machineExpression = expression,
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment
} =
    Done state