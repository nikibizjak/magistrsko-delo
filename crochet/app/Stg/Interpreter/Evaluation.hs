module Stg.Interpreter.Evaluation where

-- The small step operational semantics is described in
-- [Stg.Interpreter.Internal]. This module is responsible for the execution of
-- multiple steps in succession.

-- We provide the [evaluate] function, which executes computation steps until
-- the graph is fully reduced or until the evaluation fails. Function [run] is
-- responsible for execution of the whole program. It tries to find the main
-- function and starts evaluating it until it reaches normal form.

-- Similarly, two more functions are provided: [evaluateDebug] and [runDebug],
-- which also print all the small step in operational semantics. They accept a
-- function [debug], which should print the required debug info.

import Stg.Stg
import Stg.Interpreter.Internal
import Stg.Pretty
import qualified Data.Map as Map
import GHC.IO.Handle (hPutStr)
import GHC.IO.Handle.Text (hPutStrLn)
import Stg.Interpreter.Debug
import Stg.Interpreter.Types

evaluate :: MachineState -> Either InterpreterException MachineState
evaluate machineState =
    case evaluateExpression machineState of
        Failure exception -> Left exception
        Done machineState' -> Right machineState'
        Step machineState' ->
            evaluate machineState'

evaluateDebug :: (MachineState -> IO ()) -> MachineState -> IO (Either InterpreterException MachineState)
evaluateDebug debug machineState =
    case evaluateExpression machineState of
        Failure exception -> return $ Left exception
        Done machineState' -> return $ Right machineState'
        Step machineState' -> do
            debug machineState'
            evaluateDebug debug machineState'

run :: Program -> Either InterpreterException MachineState
run program =
    let
        (initialHeap, initialHeapPointer, initialEnvironment) =
            allocateMany Map.empty (HeapAddress 0) Map.empty program
    in
        case Map.lookup "main" initialEnvironment of
            Nothing -> Left $ InterpreterException "Undefined function 'main'."
            Just (HeapAddress address) ->
                let
                    initialState = MachineState {
                        machineExpression = Atom (Literal (Address address)),
                        machineStack = [],
                        machineHeap = initialHeap,
                        machineHeapPointer = initialHeapPointer,
                        machineEnvironment = initialEnvironment
                    }
                in
                    evaluate initialState

allocateMany initialHeap initialHeapPointer initialEnvironment =
    foldr (\(Binding name object) (heap, heapPointer, environment) ->
        let
            environment' = Map.insert name heapPointer environment
            (heap', heapPointer') = allocate heap heapPointer (HeapObject object Map.empty)
        in
            (heap', heapPointer', environment')
    ) (initialHeap, initialHeapPointer, initialEnvironment)

runDebug :: (MachineState -> IO ()) -> Program -> IO (Either InterpreterException MachineState)
runDebug debug program =
    let
        (initialHeap, initialHeapPointer, initialEnvironment) =
            allocateMany Map.empty (HeapAddress 0) Map.empty program
    in
        case Map.lookup "main" initialEnvironment of
            Nothing -> return $ Left $ InterpreterException "Undefined function 'main'."
            Just (HeapAddress address) ->
                let
                    initialState = MachineState {
                        machineExpression = Atom (Literal (Address address)),
                        machineStack = [],
                        machineHeap = initialHeap,
                        machineHeapPointer = initialHeapPointer,
                        machineEnvironment = initialEnvironment
                    }
                in do
                    debug initialState
                    evaluateDebug debug initialState

-- UTILITY FUNCTIONS
printExpression :: MachineState -> IO ()
printExpression MachineState { machineExpression = expression } = do
    putStr "Expression: "
    putStrLn $ pretty expression

printEnvironment :: MachineState -> IO ()
printEnvironment MachineState { machineEnvironment = environment } = do
    putStrLn "Environment: "
    putStrLn $ showMap environment

printStack :: MachineState -> IO ()
printStack MachineState { machineStack = stack } = do
    putStrLn "Stack: "
    putStrLn (unlines (map show stack))

showMap mapping =
    let
        showItem (address, value) =
            show address ++ " -> " ++ show value

        items = Map.toList mapping
        lines = map showItem items
    in
        unlines lines

printHeap :: MachineState -> IO ()
printHeap MachineState { machineHeap = heap } = do
    putStrLn "Heap: "
    putStrLn $ showMap heap

printSeparator =
    putStrLn "------------------------------------------------------------"

printAll :: MachineState -> IO ()
printAll state = do
    printSeparator
    printExpression state
    printEnvironment state
    printStack state
    printHeap state

writeAllToFile handle state = do
    hPutStr handle $ machineStateToHtml state

-- Print only the expression at each step of the evaluation.
runDebugExpression :: Program -> IO (Either InterpreterException MachineState)
runDebugExpression = runDebug printExpression

-- Print all debug information (expression, stack and heap) at each step of the
-- evaluation.
runDebugAll :: Program -> IO (Either InterpreterException MachineState)
runDebugAll = runDebug printAll

runDebugWriteAllToFile handle = runDebug (writeAllToFile handle)