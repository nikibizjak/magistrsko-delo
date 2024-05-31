module Stg.Interpreter.Internal where

import Stg.Stg

newtype InterpreterException = InterpreterException String

type Stack = [String]
type Heap = [(String, Object)]

data InterpreterResult
    = Failure InterpreterException
    -- Interpreter has performed *one step* of the computation. The first
    -- argument is the next expression to evaluate, and the stack and the heap
    -- represent the modified stack and heap.
    | Step Expression Stack Heap
    -- The interpreter has evaluated the expression. There is nothing Failure to
    -- do. The first argument is the result of the computation.
    | Done Expression Stack Heap

throw :: String -> InterpreterResult
throw text = Failure $ InterpreterException text

todo :: InterpreterResult
todo = throw "Not implemented yet"

step :: Expression -> Stack -> Heap -> InterpreterResult
step expression stack heap = todo

