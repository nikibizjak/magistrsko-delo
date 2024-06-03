module Stg.Interpreter.Utils where

import Stg.Stg

isDefaultAlternative (DefaultAlternative _ _) = True
isDefaultAlternative _ = False

isAlgebraicAlternative constructor (AlgebraicAlternative name _ _) | constructor == name = True
isAlgebraicAlternative _ _ = False

getDefaultAlternative alternatives =
    case filter isDefaultAlternative alternatives of
        [ alternative ] -> Just alternative
        _ -> Nothing

getAlgebraicAlternative constructor alternatives =
    case filter (isAlgebraicAlternative constructor) alternatives of
        [ alternative ] -> Just alternative
        _ -> Nothing
