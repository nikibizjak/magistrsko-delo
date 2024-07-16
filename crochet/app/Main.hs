module Main where

import ArgumentParser
import Control.Monad (when, (>=>))
import Stg.FreeVariables
import Stg.Interpreter
import Stg.NameResolution
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
    Right program -> do

      -- Perform name resolution - find undefined variables.
      case nameResolutionProgram program of
        Left (NameResolutionException exception) ->
          putStrLn $ "[ ] Name resolution exception: " ++ exception
        Right _ -> do

          result <- run debugFunction program
          case result of
            Left (InterpreterException exception) ->
              putStrLn $ "[ ] Interpreter exception: " ++ exception
            Right (MachineState {machineExpression = result}) -> do
              putStrLn $ pretty result