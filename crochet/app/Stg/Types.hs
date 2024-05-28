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

showGroup :: Show a => a -> String
showGroup item = '(' : show item ++ ")"

instance Show StgType where
    show (NamedType name) = name
    show (ReferencedType lifetime subtype) =
        case subtype of
            (ReferencedType _ _) ->
                '&' : show lifetime ++ ' ' : showGroup subtype
            (ArrowType _ _) ->
                '&' : show lifetime ++ ' ' : showGroup subtype
            _ -> '&' : show lifetime ++ ' ' : show subtype
    show (ProductType types) =
        '(' : intercalate ", " (map show types) ++ ")"
    show (ArrowType left right) =
        case left of
            (ArrowType _ _) -> showGroup left ++ " -> " ++ show right
            _ -> show left ++ " -> " ++ show right

instance Show TypeHint where
    show (TypeHint name stgType) = name ++ " :: " ++ show stgType