module Stg.BorrowCheck.Borrows where

import Stg.Stg
import qualified Data.Set as Set

class Borrows a where
    borrows :: a -> Set.Set Variable

instance Borrows Binding where
    borrows (Binding name object) = borrows object

instance Borrows Object where
    -- TODO: Write moves function for the Function object.
    -- moves (Function parameters body) =
    borrows (PartialApplication function arguments) =
        -- TODO: Currently partial application only supports moving functions.
        -- The first argument to the PartialApplication constructor should be an
        -- Atom (but not a Literal). To get rid of literals, a specialized
        -- analysis should be performed.
        -- PartialApplication :: Atom -> [ Atom ] -> Object
        borrowsMany arguments
    borrows (Constructor name arguments) = borrowsMany arguments
    borrows (Thunk expression) = borrows expression
    borrows BlackHole = Set.empty

instance Borrows Expression where
    borrows (Atom atom) = borrows atom
    borrows (FunctionApplication function _ arguments) = borrowsMany arguments
    borrows (PrimitiveOperation _ arguments) = borrowsMany arguments
    borrows (LetIn name object body) =
        -- Set.union (borrows object) (borrows body)
        borrows body
    -- borrows (CaseOf Expression [Alternative]) = Set.empty

instance Borrows Atom where
    borrows (Variable _) = Set.empty
    borrows (Literal _) = Set.empty


borrowsMany :: Foldable t => Borrows a => t a -> Set.Set Variable
borrowsMany = foldr (Set.union . borrows) Set.empty