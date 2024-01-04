{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use second" #-}
module Language.TypeChecker.Internal where

import Control.Monad.State
import Control.Exception ( throw )
import Data.Maybe
import Exception ( CompilerException(..) )
import Language.Language ( Declaration(..), Expression(..), Alternative (..), TypeDefinition(..), isTypeDeclaration, isValueDeclaration )
import Control.Monad (foldM)
import Language.TypeChecker.Types
import Language.TypeChecker.Unify ( substitute, unify )


-- Generate a new unique type variable. 
fresh :: State Int Type
fresh = do
    n <- get
    put (n + 1)
    return (TypeVariable n)

resolve :: [(String, Type)] -> TypeDefinition -> Type
resolve context typ =
    case typ of
        IntegerTypeDefinition -> integer
        ParametricTypeDefinition name ->
            case lookup name context of
                Nothing -> throw $ TypeCheckException $ "Unknown type variable " ++ name ++ "."
                Just value -> value
        NamedType name parameters ->
            TypeFunction name (map (resolve context) parameters)

generateConstructorFunctions :: Declaration -> State Int [(String, Type)]
generateConstructorFunctions (TypeDeclaration name parameters constructors) = do
    -- data List 'a = ...

    -- 1. context = [ 'a -> alpha ]
    context <- mapM (\parameter -> do
            alpha <- fresh
            return (parameter, alpha)) parameters

    let parameterTypes = map snd context

    let pt = map (\(TypeVariable i) -> i) parameterTypes
    let quantified items = foldr Quantifier items pt

    let resolveConstructorTypes (name, constructorParameters) = (name, map (resolve context) constructorParameters)
    let resolvedConstructors = map resolveConstructorTypes constructors

    -- Type of the return result
    let result = TypeFunction name parameterTypes

    mapM (\(constructorName, constructorParameters) -> do
        return (constructorName, quantified $ foldr arrow result constructorParameters)
        ) resolvedConstructors

-- generateConstructorFunctionsAll :: [Declaration] -> State Int [(String, Type)]
generateConstructorFunctionsAll :: [Declaration] -> State Int [(String, Type)]
generateConstructorFunctionsAll typeDeclarations = do
    foldM (\acc x -> do
        newItems <- generateConstructorFunctions x
        return $ newItems ++ acc) [] typeDeclarations

inferDeclarations :: Context -> [Declaration] -> State Int ([(String, Type)], [(Type, Type)])
inferDeclarations context = foldM (\(types, equations) declaration -> do
    (inferredType, inferredEquations) <- inferDeclaration context declaration
    return (inferredType : types, inferredEquations ++ equations)
    ) ([], [])

-- inferDeclaration :: Declaration -> State Int ((String, LanguageType), [(LanguageType, LanguageType)])
inferDeclaration context declaration = do
    case declaration of
        ValueDeclaration name expression -> do
            (inferredType, inferredEquations) <- inferExpression context expression
            return ((name, inferredType), inferredEquations)
        _ -> throw NotImplementedException

inferStateful program = do
    
    let typeDeclarations = filter isTypeDeclaration program
        valueDeclarations = filter isValueDeclaration program

    -- CONSTRUCTORS FOR USER DEFINED TYPES
    -- For user defined types like the following
    --
    -- data Tree 'a = | Leaf 'a | Branch (Tree 'a) (Tree 'a) ;;
    --
    -- we need to generate types for constructors. Each type is a function
    -- that accepts arguments and returns the user defined type.
    --
    -- Leaf :: 'a -> Tree 'a
    -- Branch :: (Tree 'a) -> (Tree 'a) -> (Tree 'a)
    constructors <- generateConstructorFunctionsAll typeDeclarations
    
    -- TYPES FOR FUNCTIONS DEFINED AT TOP-LEVEL
    topLevelFunctions <- mapM (\(ValueDeclaration name _) -> do
        alpha <- fresh
        return (name, alpha)) valueDeclarations

    let context = constructors

    (names, equations) <- inferDeclarations context valueDeclarations
    let substitution = unify equations

    return $ (map (\(name, typ) -> (name, substitute substitution typ)) names, equations)

infer program =
    evalState (inferStateful program) 0


inferExpression :: Context -> Expression -> State Int (Type, [(Type, Type)])
inferExpression context (Integer _) = return (integer, [])

inferExpression context (Variable name) =
    case lookup name context of
        Just value -> return (value, [])
        Nothing -> throw (TypeCheckException ("Unknown type of variable '"++ name ++ "'."))

inferExpression context (Binary _ left right) = do
    (typeLeft, equationsLeft) <- inferExpression context left
    (typeRight, equationsRight) <- inferExpression context right
    return (integer, (typeLeft, integer) : (typeRight, integer) : equationsLeft ++ equationsRight)

inferExpression context (Lambda parameter expression) = do
    alpha <- fresh
    (ty, equations) <- inferExpression ((parameter, alpha) : context) expression
    return (arrow alpha ty, equations)

inferExpression context (Application left right) = do
    alpha <- fresh
    (typeLeft, equationsLeft) <- inferExpression context left
    (typeRight, equationsRight) <- inferExpression context right
    return (alpha, (typeLeft, arrow typeRight alpha) : equationsLeft ++ equationsRight)

-- TODO: Check if this is actually correct, we might have implemented type
-- inference for the letrec instead.
inferExpression context (LetIn binds body) = do
    typesBinds <- mapM (\(name, expression) -> do
        alpha <- fresh
        return (name, alpha, expression)) binds

    let types = map (\(name, alpha, _) -> (name, alpha)) typesBinds
    let context' = types ++ context

    equationsBinds <- foldM (\acc (name, alpha, expression) -> do
        (typeExpression, equationsExpression) <- inferExpression context' expression
        return ((alpha, typeExpression) : equationsExpression ++ acc)
        ) [] typesBinds

    (typeBody, equationsBody) <- inferExpression context' body
    return (typeBody, equationsBinds ++ equationsBody)

inferExpression context (LetRec binds body) = do
    typesBinds <- mapM (\(name, expression) -> do
        alpha <- fresh
        return (name, alpha, expression)) binds

    let types = map (\(name, alpha, _) -> (name, alpha)) typesBinds
    let context' = types ++ context

    equationsBinds <- foldM (\acc (name, alpha, expression) -> do
        (typeExpression, equationsExpression) <- inferExpression context' expression
        return ((alpha, typeExpression) : equationsExpression ++ acc)
        ) [] typesBinds

    (typeBody, equationsBody) <- inferExpression context' body
    return (typeBody, equationsBinds ++ equationsBody)

inferExpression context (IfThenElse condition thenBranch elseBranch) =
    throw NotImplementedException

inferExpression context (CaseOf expression alternatives) =
    -- Types of all alternative's bodies must match
    let
        body alternative =
            case alternative of
                AlgebraicAlternative constructor variables body -> body
                PrimitiveAlternative value body -> body
                NamedDefaultAlternative variable body -> body
                DefaultAlternative body -> body

        bodyExpressions = map body alternatives
    in
        throw NotImplementedException

-- The last one