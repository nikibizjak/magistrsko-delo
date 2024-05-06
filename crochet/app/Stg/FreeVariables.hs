module Stg.FreeVariables where

import qualified Data.Set as Set
import Stg.Stg
import Stg.NameResolution

namesSet :: [Variable] -> Set.Set String
namesSet variables = Set.fromList $ names variables

class FreeVariables a where
  freeVariables :: a -> Set.Set String

instance FreeVariables Literal where
  freeVariables (Integer _) = Set.empty

instance FreeVariables Atom where
  freeVariables (Variable v) =
    Set.singleton v
  freeVariables (Literal l) = freeVariables l
  freeVariables (Borrow variable) = freeVariables variable

instance FreeVariables Expression where
  freeVariables (Atom atom) = freeVariables atom
  freeVariables (FunctionApplication function arity arguments) =
    Set.insert function (Set.unions (map freeVariables arguments))
  freeVariables (PrimitiveOperation operation arguments) =
    Set.unions (map freeVariables arguments)
  freeVariables (LetIn name value body) =
    Set.delete name (Set.union (freeVariables value) (freeVariables body))
  freeVariables (CaseOf scrutinee alternatives) =
    Set.union (freeVariables scrutinee) (Set.unions (map freeVariables alternatives))

instance FreeVariables Alternative where
  freeVariables (AlgebraicAlternative constructor variables expression) =
    Set.difference (freeVariables expression) (namesSet variables)
  freeVariables (DefaultAlternative variable expression) =
    Set.delete (name variable) (freeVariables expression)

instance FreeVariables Object where
  freeVariables (Function variables expression) =
    Set.difference (freeVariables expression) (namesSet variables)
  freeVariables (PartialApplication function arguments) =
    Set.insert function (Set.unions (map freeVariables arguments))
  freeVariables (Constructor constructor arguments) =
    Set.unions (map freeVariables arguments)
  freeVariables (Thunk expression) = freeVariables expression
  freeVariables BlackHole = Set.empty

instance FreeVariables Binding where
  freeVariables (Binding name value) = freeVariables value