{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad (when)
import Stg.BorrowCheck
import Stg.FreeVariables
import Stg.Interpreter
import Stg.MoveCheck
import Stg.NameResolution
import Stg.Ownership
import Stg.Parser
import Stg.Pretty
import Stg.Stg
import System.Console.Docopt
import System.Environment (getArgs)
import System.IO (openFile, hClose)
import GHC.IO.IOMode (IOMode(..))

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  arguments <- parseArgsOrExit patterns =<< getArgs

  when (arguments `isPresent` argument "file") $ do
    filename <- arguments `getArgOrExit` argument "file"
    contents <- readFile filename

    let debugFile = getArg arguments (longOption "debug-interpreter")

    -- Execute the program
    execute contents debugFile

execute contents debugInterpreter =
  case parse contents of
    Left (ParserException exception) ->
      putStrLn $ "[ ] Parsing AST tree exception: " ++ exception
    Right (program, typeHints) -> do
      -- putStrLn "[x] Parsing AST tree"
      -- print $ map pretty program
      -- print typeHints

      -- Perform name resolution - find undefined variables.
      case nameResolutionProgram program of
        Left (NameResolutionException exception) ->
          putStrLn $ "[ ] Name resolution exception: " ++ exception
        Right _ -> do
          -- putStrLn "[x] Name resolution"

          -- Perform borrow checking - find use after free errors.
          case borrowCheckProgram program of
            Left (BorrowCheckException exception) ->
              putStrLn $ "[ ] Borrow check exception: " ++ exception
            Right (lifetimes, equations) -> do
              -- putStrLn "[x] Borrow check"
              -- print lifetimes
              -- print equations

              -- Perform move checking - find invalid moves
              case moveCheckProgram program of
                Left (MoveCheckException exception) ->
                  putStrLn $ "[ ] Move check exception: " ++ exception
                Right _ -> do
                  -- putStrLn "[x] Move check"
                  -- case debugInterpreter of

                    case debugInterpreter of

                      Nothing -> do
                        case run program of
                          Left (InterpreterException exception) ->
                            putStrLn $ "[ ] Interpreter exception: " ++ exception
                          Right (MachineState {machineExpression = result}) -> do
                            putStrLn $ pretty result
                        
                      Just debugFile -> do
                        handle <- openFile debugFile WriteMode
                        
                        interpreterResult <- runDebugWriteAllToFile handle program
                        case interpreterResult of
                          Left (InterpreterException exception) ->
                            putStrLn $ "[ ] Interpreter exception: " ++ exception
                          Right (MachineState {machineExpression = result}) -> do
                            -- putStr "[x] Interpreter result: "
                            putStrLn $ pretty result
                        
                        hClose handle