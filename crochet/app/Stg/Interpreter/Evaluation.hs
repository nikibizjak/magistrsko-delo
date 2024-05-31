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

evaluate :: Expression -> Stack -> Heap -> Either InterpreterException Expression
evaluate expression stack heap =
    case step expression stack heap of
        Failure exception -> Left exception
        Done result _ _ -> Right result
        Step expression' stack' heap' ->
            evaluate expression' stack' heap'

evaluateDebug :: (Expression -> Stack -> Heap -> IO a) -> Expression -> Stack -> Heap -> IO (Either InterpreterException Expression)
evaluateDebug debug expression stack heap =
    case step expression stack heap of
        Failure exception -> return $ Left exception
        Done result _ _ -> return $ Right result
        Step expression' stack' heap' -> do
            debug expression' stack' heap'
            evaluateDebug debug expression' stack' heap'

run :: Program -> Either InterpreterException Expression
run program =
    evaluate (Atom $ Literal $ Integer 1) [] []

runDebug :: (Expression -> Stack -> Heap -> IO a) -> Program -> IO (Either InterpreterException Expression)
runDebug debug program =
    evaluateDebug debug (Atom $ Literal $ Integer 1) [] []

-- UTILITY FUNCTIONS
printExpression :: Expression -> Stack -> Heap -> IO ()
printExpression expression stack heap = do
    putStr "Expression: "
    print expression

printStack :: Expression -> Stack -> Heap -> IO ()
printStack expression stack heap = do
    putStr "Stack: "
    print stack

printHeap :: Expression -> Stack -> Heap -> IO ()
printHeap expression stack heap = do
    putStr "Heap: "
    print heap

-- Print only the expression at each step of the evaluation.
runDebugExpression :: Program -> IO (Either InterpreterException Expression)
runDebugExpression = runDebug printExpression

-- Print all debug information (expression, stack and heap) at each step of the
-- evaluation.
runDebugAll :: Program -> IO (Either InterpreterException Expression)
runDebugAll = runDebug (printExpression >> printStack >> printHeap)
