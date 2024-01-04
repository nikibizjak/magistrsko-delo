{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Use bimap" #-}
module Language.TypeChecker.Unify where

import Language.TypeChecker.Types
import qualified Data.Maybe
import Control.Exception (throw)
import Exception (CompilerException(TypeCheckException))

mapsVariable :: Int -> (Type, Type) -> Bool
mapsVariable variable (from, _) =
    case from of
        TypeVariable x | x == variable -> True
        _ -> False

substitute :: [(Type, Type)] -> Type -> Type
substitute substitution typ =
    case typ of
        TypeVariable _ ->
            case lookup typ substitution of
                Nothing -> typ
                Just value -> value
        TypeFunction name types ->
            TypeFunction name (map (substitute substitution) types)
        Quantifier variable body ->
            -- Since the quantifier introduces a new type variable, this
            -- variable should *not* be substituted in its body. To do this, the
            -- mapping (variable -> ...) should be removed from the substitution
            -- at this point.
            let
                modifiedSubstitution = filter (not . mapsVariable variable) substitution
            in
                substitute modifiedSubstitution body

composeSubstitution :: [(Type, Type)] -> [(Type, Type)] -> [(Type, Type)]
composeSubstitution first second =
    first ++ map (\(from, to) -> (from, substitute first to)) second

substituteEquations :: [(Type, Type)] -> [(Type, Type)] -> [(Type, Type)]
substituteEquations substitution =
    map (\(a, b) -> (substitute substitution a, substitute substitution b))

remove value = filter (/= value)

freeTypeVariables :: Type -> [Int]
freeTypeVariables typ =
    case typ of
        TypeVariable v -> [ v ]
        TypeFunction _ types ->
            concatMap freeTypeVariables types
        Quantifier argument typ ->
            remove argument (freeTypeVariables typ)

isBound :: Int -> Type -> Bool
isBound typeVariable ty =
    typeVariable `notElem` freeTypeVariables ty

unify :: [(Type, Type)] -> [(Type, Type)]
unify equations =
    case equations of
        [] -> []
        (TypeFunction name args, TypeFunction name' args') : rest | name == name' && length args == length args' ->
            unify (zip args args' ++ rest)
        (TypeVariable v1, ty) : rest | isBound v1 ty ->
            let substitution = unify (substituteEquations [ (TypeVariable v1, ty) ] equations)
            in composeSubstitution [(TypeVariable v1, substitute substitution ty)] substitution
        (ty, TypeVariable v1) : rest | isBound v1 ty ->
            let substitution = unify (substituteEquations [ (TypeVariable v1, ty) ] equations)
            in composeSubstitution [(TypeVariable v1, substitute substitution ty)] substitution
        (first, second) : rest | first == second -> unify rest
        -- TODO: Add unification for quantifiers
        (first, second) : _ ->
            throw $ TypeCheckException $ "Types '" ++ show first ++ "' and '" ++ show second ++ "' can't be unified."