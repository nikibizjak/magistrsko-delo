module Language.TypeChecker.Types where

data Type
    = TypeVariable Int
    | TypeFunction String [Type]
    | Quantifier Int Type
    deriving Eq

type Context = [(String, Type)]

instance Show Type where
    show ty = case ty of
        TypeVariable value -> '\'' : showTypeVariable value
        TypeFunction name types ->
            let
                showSubType t = case t of
                    TypeFunction "int" [] -> show t
                    TypeFunction _ _ -> "(" ++ show t ++ ")"
                    _ -> show t
            in
                case (name, types) of
                    ("arrow", [left, right]) ->
                        showSubType left ++ " -> " ++ show right
                    ("int", []) -> "int"
                    (name, []) -> name
                    _ -> name ++ " " ++ unwords (map showSubType types)
        Quantifier name typ ->
            "forall '" ++ showTypeVariable name ++ " . " ++ show typ

typeVariableNames :: [String]
typeVariableNames = [replicate cnt v | cnt <- [1..], v <- ['a'..'z']]

showTypeVariable value = typeVariableNames !! value

integer :: Type
integer = TypeFunction "int" []

arrow :: Type -> Type -> Type
arrow first second = TypeFunction "arrow" [first, second]