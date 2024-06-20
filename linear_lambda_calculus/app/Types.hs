module Types where

data Qualifier
    = Linear
    | Unrestricted
    deriving (Show, Eq)

data Boolean
    = BTrue
    | BFalse
    deriving (Show)

data Term
    = Variable String
    | Boolean Qualifier Boolean
    | Conditional Term Term Term
    | Pair Qualifier Term Term
    | Split Term String String Term
    | Abstraction Qualifier String Type Term
    | Application Term Term
    deriving (Show)

data Pretype
    = TBoolean
    | TPair Type Type
    | TFunction Type Type
    deriving (Show, Eq)

data Type
    = Type Qualifier Pretype
    deriving (Show, Eq)

type Context = [ (String, Type) ]

newtype TypeCheckException
    = TypeCheckException String
    deriving (Show)