module Stg.BoundVariables where

import qualified Data.Set as Set
import Stg.Stg
import Stg.NameResolution

namesSet :: [Variable] -> Set.Set String
namesSet variables = Set.fromList $ names variables

class BoundVariables a where
  boundVariables :: a -> Set.Set String

instance BoundVariables Literal where
  boundVariables (Integer _) = Set.empty

instance BoundVariables Atom where
  boundVariables (Variable v) = Set.empty
  boundVariables (Literal l) = boundVariables l

instance BoundVariables Expression where
  boundVariables (Atom atom) = boundVariables atom
  boundVariables (FunctionApplication function arity arguments) =
    Set.unions (map boundVariables arguments)
  boundVariables (PrimitiveOperation operation arguments) =
    Set.unions (map boundVariables arguments)
  boundVariables (LetIn name value body) =
    Set.insert name (boundVariables body)
  boundVariables (CaseOf scrutinee alternatives) =
    Set.union (boundVariables scrutinee) (Set.unions (map boundVariables alternatives))

instance BoundVariables Alternative where
  boundVariables (AlgebraicAlternative constructor variables expression) =
    Set.union (boundVariables expression) (namesSet variables)
  boundVariables (DefaultAlternative variable expression) =
    Set.insert (name variable) (boundVariables expression)

instance BoundVariables Object where
  boundVariables (Function variables expression) =
    Set.union (boundVariables expression) (namesSet variables)
  boundVariables (PartialApplication function arguments) =
    -- Set.insert function (Set.unions (map boundVariables arguments))
    Set.empty
  boundVariables (Constructor constructor arguments) =
    Set.unions (map boundVariables arguments)
  boundVariables (Thunk expression) = boundVariables expression
  boundVariables BlackHole = Set.empty

instance BoundVariables Binding where
  boundVariables (Binding name value) = boundVariables value