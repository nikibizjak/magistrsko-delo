module Stg.Interpreter.Types where

import qualified Data.Map as Map
import Stg.Stg
import Stg.Pretty
import Text.Printf (printf)
import Data.List (intercalate)

newtype InterpreterException = InterpreterException String

newtype HeapAddress
    = HeapAddress Int
    deriving (Eq, Ord)

type Stack = [ Continuation ]
type Heap = Map.Map HeapAddress HeapObject
type Environment = Map.Map String HeapAddress

data InterpreterResult
    = Failure InterpreterException
    -- Interpreter has performed *one step* of the computation. The first
    -- argument is the next expression to evaluate, and the stack and the heap
    -- represent the modified stack and heap.
    | Step MachineState
    -- The interpreter has evaluated the expression. There is nothing Failure to
    -- do. The first argument is the result of the computation.
    | Done MachineState

data Continuation
    = CaseContinuation [Alternative]
    | UpdateContinuation HeapAddress
    | ApplyContinuation [Atom]

data HeapObject =
    HeapObject Object Environment

data MachineState = MachineState
    { machineExpression :: Expression
    , machineStack :: Stack
    , machineHeap :: Heap
    , machineHeapPointer :: HeapAddress
    , machineEnvironment :: Environment
    , machineStep :: Int
    }

-- Show implementation
instance Show HeapAddress where
    show (HeapAddress a) = printf "0x%04x" a

instance Show Continuation where
    show (CaseContinuation alternatives) =
        "case . of " ++ unwords (map pretty alternatives)
    show (UpdateContinuation thunk) =
        "Update " ++ show thunk
    show (ApplyContinuation arguments) =
        "Apply " ++ unwords (map pretty arguments)

instance Show HeapObject where
    show (HeapObject object environment) =
        pretty object ++ " | " ++ showEnvironment environment

showEnvironment environment =
    let
        showItem (name, address) =
            show name ++ " -> " ++ show address

        items = Map.toList environment
    in
        '{' : intercalate ", " (map showItem items) ++ "}"