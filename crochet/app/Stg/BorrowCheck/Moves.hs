module Stg.BorrowCheck.Moves where

import Stg.Stg
import qualified Data.Set as Set

class Moves a where
    moves :: a -> Set.Set Variable

instance Moves Binding where
    moves (Binding name object) = moves object

instance Moves Object where
    -- TODO: Write moves function for the Function object.
    -- moves (Function parameters body) =
    moves (PartialApplication function arguments) =
        -- TODO: Currently partial application only supports moving functions.
        -- The first argument to the PartialApplication constructor should be an
        -- Atom (but not a Literal). To get rid of literals, a specialized
        -- analysis should be performed.
        -- PartialApplication :: Atom -> [ Atom ] -> Object
        Set.insert function (movesMany arguments)
    moves (Constructor name arguments) = movesMany arguments
    moves (Thunk expression) = moves expression
    moves BlackHole = Set.empty

instance Moves Expression where
    moves (Atom atom) = moves atom
    moves (FunctionApplication function _ arguments) =
        Set.insert function (movesMany arguments)
    moves (PrimitiveOperation _ arguments) = movesMany arguments
    moves (LetIn name object body) =
        Set.union (moves object) (moves body)
    -- moves (CaseOf Expression [Alternative]) = Set.empty

instance Moves Atom where
    moves (Variable name) = Set.singleton name
    moves (Literal _) = Set.empty


movesMany :: Foldable t => Moves a => t a -> Set.Set Variable
movesMany = foldr (Set.union . moves) Set.empty