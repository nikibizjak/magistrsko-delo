module Main where

import ArgumentParser
import Control.Monad (when, (>=>))
import Stg.BorrowCheck
import Stg.FreeVariables
import Stg.Interpreter
import Stg.MoveCheck
import Stg.NameResolution
import Stg.Ownership
import Stg.Parser
import Stg.Pretty
import Stg.Stg

main = do
  arguments <- parseArguments
  runInterpreter arguments

runInterpreter
  arguments@Options
    { debugInterpreter = debugInterpreter,
      debugDirectory = debugDirectory,
      inputFile = inputFile
    } = do
    let debugFunction = noDebug

    let debugFunction' =
          if debugInterpreter
            then debugFunction >=> debugPrint
            else debugFunction

    let debugFunction'' = case debugDirectory of
          Just directory -> debugFunction' >=> debugHtml directory
          Nothing -> debugFunction'

    contents <- readFile inputFile
    execute debugFunction'' contents

execute debugFunction contents =
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

                  result <- run debugFunction program
                  case result of
                    Left (InterpreterException exception) ->
                      putStrLn $ "[ ] Interpreter exception: " ++ exception
                    Right (MachineState {machineExpression = result}) -> do
                      putStrLn $ pretty result