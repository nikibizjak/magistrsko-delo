module Stg.Stg where

newtype Literal
    = Integer Int
    -- Missing: Double, which is implemented in the original paper
    deriving (Show, Eq)

data Variable
    = MovedVariable String
    | BorrowedVariable String
    deriving (Show, Eq)

data Atom
    = Variable Variable
    | Literal Literal
    deriving (Show, Eq)

data FunctionArity
    = Known Int
    | Unknown
    deriving (Show, Eq)

data Expression
    = Atom Atom
    | FunctionApplication Variable FunctionArity [Atom]
    | PrimitiveOperation PrimitiveOperation [Atom]
    | LetIn String Object Expression
    -- Missing: LetRec, which is not implemented in the original paper
    | CaseOf Expression [Alternative]
    deriving (Show, Eq)

data PrimitiveOperation
    = Addition
    | Multiplication
    deriving (Show, Eq)

data Alternative
    = AlgebraicAlternative String [Variable] Expression
    | DefaultAlternative Variable Expression
    deriving (Show, Eq)

data Object
    = Function [Variable] Expression
    | PartialApplication Variable [Atom]
    | Constructor String [Atom]
    | Thunk Expression
    | BlackHole
    deriving (Show, Eq)

data Binding =
    Binding String Object
    deriving (Show, Eq)

type Program = [Binding]