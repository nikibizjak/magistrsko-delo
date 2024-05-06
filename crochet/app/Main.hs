module Main where

import System.Directory.Internal.Prelude ( getArgs, readFile )

import Stg.Stg
import Stg.Parser.Internal
import Stg.Pretty
import Stg.Ownership
import Stg.FreeVariables
import Stg.NameResolution
import Stg.MoveCheck
import Stg.BorrowCheck

main = do
    arguments <- getArgs
    case arguments of
        [ filename ] -> do
            contents <- readFile filename
            execute contents
        _ ->
            putStrLn "Usage: cabal run crochet [input_file]"

execute contents =
    case parse contents of
        Left (ParserException exception) ->
            putStrLn $ "[ ] Parsing AST tree exception: " ++ exception
        Right program -> do
            putStrLn "[x] Parsing AST tree"
            print $ map pretty program
        
            -- Perform name resolution - find undefined variables.
            case nameResolutionProgram program of
                Left (NameResolutionException exception) ->
                    putStrLn $ "[ ] Name resolution exception: " ++ exception
                Right _ -> do
                    putStrLn "[x] Name resolution"

                    -- Perform borrow checking - find use after free errors.
                    case borrowCheckProgram program of
                        Left (BorrowCheckException exception) ->
                            putStrLn $ "[ ] Borrow check exception: " ++ exception
                        Right (lifetimes, equations) -> do
                            putStrLn "[x] Borrow check"
                            print lifetimes
                            print equations

                            -- Perform move checking - find invalid moves
                            case moveCheckProgram program of
                                Left (MoveCheckException exception) ->
                                    putStrLn $ "[ ] Move check exception: " ++ exception
                                Right _ -> do

                                    putStrLn "[x] Move check"

                            