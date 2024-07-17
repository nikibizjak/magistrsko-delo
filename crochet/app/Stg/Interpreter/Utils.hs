module Stg.Interpreter.Utils where

import Stg.Stg
import Stg.Interpreter.Types
import Debug.Trace
import Stg.Pretty
import Stg.Interpreter.Memory (getAddress)

throw :: String -> InterpreterResult
throw text = Failure $ InterpreterException text

todo :: InterpreterResult
todo = throw "Not implemented yet"

continue :: MachineState -> InterpreterResult
continue = Step . next

isDefaultAlternative (DefaultAlternative _ _) = True
isDefaultAlternative _ = False

isAlgebraicAlternative constructor (AlgebraicAlternative name _ _) | constructor == name = True
isAlgebraicAlternative _ _ = False

getDefaultAlternative alternatives =
    case filter isDefaultAlternative alternatives of
        [ alternative ] -> Just alternative
        _ -> Nothing

getAlgebraicAlternative constructor alternatives =
    case filter (isAlgebraicAlternative constructor) alternatives of
        [ alternative ] -> Just alternative
        _ -> Nothing

step expression stack heap heapPointer environment i =
    Step $ MachineState {
        machineExpression = expression,
        machineStack = stack,
        machineHeap = heap,
        machineHeapPointer = heapPointer,
        machineEnvironment = environment,
        machineStep = i
    }

next :: MachineState -> MachineState
next state@MachineState { machineStep = i } =
    state { machineStep = i + 1 }

isObjectValue :: Object -> Bool
isObjectValue object =
    case object of
        Function _ _ -> True
        PartialApplication _ _ -> True
        Constructor _ _ -> True
        _ -> False

isHeapObjectValue :: HeapObject -> Bool
isHeapObjectValue (HeapObject object _) = isObjectValue object
isHeapObjectValue _ = False

isJustHeapObjectValue :: Maybe HeapObject -> Bool
isJustHeapObjectValue = maybe False isHeapObjectValue

isConstructor :: HeapObject -> Bool
isConstructor (HeapObject (Constructor _ _) _) = True
isConstructor _ = False

isThunk :: HeapObject -> Bool
isThunk (HeapObject (Thunk _) _) = True
isThunk _ = False

isFunction :: HeapObject -> Bool
isFunction (HeapObject (Function _ _) _) = True
isFunction _ = False

isPartialApplication :: HeapObject -> Bool
isPartialApplication (HeapObject (PartialApplication _ _) _) = True
isPartialApplication _ = False

isIndirection :: HeapObject -> Bool
isIndirection (Indirection _) = True
isIndirection _ = False

isJustConstructor :: Maybe HeapObject -> Bool
isJustConstructor = maybe False isConstructor

isJustThunk :: Maybe HeapObject -> Bool
isJustThunk = maybe False isThunk

isJustFunction :: Maybe HeapObject -> Bool
isJustFunction = maybe False isFunction

isJustPartialApplication :: Maybe HeapObject -> Bool
isJustPartialApplication = maybe False isPartialApplication

isJustIndirection :: Maybe HeapObject -> Bool
isJustIndirection = maybe False isIndirection

isLiteral :: Environment -> Atom -> Bool
isLiteral environment atom =
    case getAddress environment atom of
        EnvironmentLiteral _ -> True
        _ -> False

traceStep rule MachineState {
    machineExpression = expression,
    machineStep = i
} =
    trace $ show i ++ " (" ++ rule ++ "): " ++ pretty expression