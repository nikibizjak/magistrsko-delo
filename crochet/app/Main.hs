module Main where

import System.Directory.Internal.Prelude ( getArgs )
import Language.Language ( Expression, Program )
import Language.Parser ( parse, parseExpression )
import Language.Printer
import Language.TypeChecker ( infer )

execute contents =
    let
        abstractSyntaxTree = parse contents
        --result = abstractSyntaxTree
        result = infer abstractSyntaxTree
        -- result = evaluate abstractSyntaxTree
    in result

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        [] -> do
            contents <- getContents
            print $ execute contents
        _ ->
            putStrLn "Usage: cabal run crochet < [input_file]"
