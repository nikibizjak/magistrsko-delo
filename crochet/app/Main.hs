module Main where

import System.Directory.Internal.Prelude ( getArgs )
import Language.Language ( Expression, Program )
import Language.Parser ( parse, parseExpression )
import Language.Printer
import Language.Converter ( toStg )
import Stg.Stg (prettify)
import Language.Optimizer ( removeUnnecessaryBinds, freeVariables )

execute contents =
    removeUnnecessaryBinds $
    parse contents

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        [] -> do
            contents <- getContents
            print $ execute contents
        _ ->
            putStrLn "Usage: cabal run crochet < [input_file]"
