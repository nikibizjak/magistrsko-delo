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
import qualified Data.Map as Map
import Stg.Interpreter.Types

evaluate :: Monad m => (MachineState -> m a) -> MachineState -> m (Either InterpreterException MachineState)
evaluate debug machineState =
    case evaluateExpression machineState of
        Failure exception -> return $ Left exception
        Done machineState' -> return $ Right machineState'
        Step machineState' -> do
            _ <- debug machineState'
            evaluate debug machineState'

initializeState :: Program -> Either InterpreterException MachineState
initializeState program =
    let
        (initialHeap, initialHeapPointer, initialEnvironment) =
            initializeTopLevelObjects program
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
                        machineEnvironment = initialEnvironment,
                        machineStep = 1
                    }
                in
                    Right initialState

run :: Monad m => (MachineState -> m a) -> Program -> m (Either InterpreterException MachineState)
run debug program =
    case initializeState program of
        Left exception -> return $ Left exception
        Right initialState -> do
            debug initialState
            evaluate debug initialState

noDebug :: MachineState -> IO MachineState
noDebug = return