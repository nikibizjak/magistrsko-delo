module Stg.Pretty where

import Stg.Stg
import Data.List (intercalate)

class Pretty a where
   pretty :: a -> String

instance Pretty Literal where
   pretty (Integer n) = show n

instance Pretty Variable where
   pretty (MovedVariable v) = v
   pretty (BorrowedVariable v) = '&' : v

instance Pretty Atom where
   pretty (Variable v) = pretty v
   pretty (Literal l) = pretty l

instance Pretty FunctionArity where
   pretty (Known n) = '^' : show n
   pretty Unknown = "^?"

instance Pretty Expression where
   pretty (Atom atom) = pretty atom
   pretty (FunctionApplication function arity arguments) =
      -- The fast curry paper assumes that the number of arguments is > 0.
      pretty function ++ pretty arity ++ (' ' : unwords (map pretty arguments))
   pretty (PrimitiveOperation operation arguments) =
      -- The fast curry paper assumes that the number of arguments is > 0.
      pretty operation ++ " " ++ unwords (map pretty arguments)
   pretty (LetIn name value body) =
      "let " ++ name ++ " = " ++ pretty value ++ " in " ++ pretty body
   pretty (LetRec bindings body) =
      let
         binds = map (\(name, value) -> name ++ " = " ++ pretty value) bindings
      in
         "letrec " ++ intercalate "; " binds ++ " in " ++ pretty body
   pretty (CaseOf scrutinee alternatives) =
      -- The fast curry paper assumes that the number of alternatives is > 0.
      "case "  ++ pretty scrutinee ++ " of " ++ unwords (map pretty alternatives)

instance Pretty Alternative where
   pretty (AlgebraicAlternative constructor variables expression) =
      if null variables
      then "| " ++ constructor ++ " -> " ++ pretty expression
      else "| " ++ constructor ++ " " ++ unwords (map pretty variables) ++ " -> " ++ pretty expression
   pretty (DefaultAlternative variable expression) =
      "| " ++ pretty variable ++ " -> " ++ pretty expression

instance Pretty Object where
   pretty (Function variables expression) =
      "FUN(" ++ unwords (map pretty variables) ++ " -> " ++ pretty expression ++ ")"
   pretty (PartialApplication function arguments) =
      "PAP(" ++ pretty function ++ unwords (map pretty arguments) ++ ")"
   pretty (Constructor constructor arguments) =
      if null arguments
      then "CON(" ++ constructor ++ ")"
      else "CON(" ++ constructor ++ " " ++ unwords (map pretty arguments) ++ ")"
   pretty (Thunk expression) = "THUNK(" ++ pretty expression ++ ")"
   pretty BlackHole = "BLACKHOLE"

instance Pretty Binding where
   pretty (Binding name value) = name ++ " = " ++ pretty value

instance Pretty PrimitiveOperation where
   pretty Addition = "add"
   pretty Multiplication = "multiply"