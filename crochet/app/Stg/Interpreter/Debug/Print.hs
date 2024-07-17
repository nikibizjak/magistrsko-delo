module Stg.Interpreter.Debug.Print (debugPrint) where

import Stg.Interpreter.Types (MachineState(..), HeapAddress, HeapObject)
import Stg.Pretty
import qualified Data.Map as Map

printExpression :: MachineState -> IO MachineState
printExpression state@MachineState { machineExpression = expression } = do
    putStr "Expression: "
    putStrLn $ pretty expression
    return state

printEnvironment :: MachineState -> IO ()
printEnvironment MachineState { machineEnvironment = environment } = do
    putStrLn "Environment: "
    putStrLn $ showMap environment

printStack :: MachineState -> IO ()
printStack MachineState { machineStack = stack } = do
    putStrLn "Stack: "
    putStrLn (unlines (map show stack))

showMap :: (Show k, Show a) => Map.Map k a -> String
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

debugPrint :: MachineState -> IO MachineState
debugPrint state = do
    printSeparator
    printExpression state
    printEnvironment state
    printStack state
    printHeap state
    return state