module Stg.Types where

import Data.List (intercalate)

data Lifetime
    = StaticLifetime
    | NamedLifetime String
    deriving (Eq)

data StgType
    = NamedType String
    | ReferencedType Lifetime StgType
    | ProductType [ StgType ]
    | ArrowType StgType StgType
    deriving (Eq)

data TypeHint
    = TypeHint String StgType
    deriving (Eq)

instance Show Lifetime where
    show (NamedLifetime name) = '\'' : name
    show StaticLifetime = "'static"

instance Show StgType where
    show (NamedType name) = name
    show (ReferencedType lifetime subtype) = '&' : show lifetime ++ ' ' : show subtype
    show (ProductType types) =
        if null types
            then "()"
            else
                '(' : intercalate ", " (map show types) ++ ")"
    show (ArrowType left right) = "(" ++ show left ++ " -> " ++ show right ++ ")"

instance Show TypeHint where
    show (TypeHint name stgType) = name ++ " :: " ++ show stgType