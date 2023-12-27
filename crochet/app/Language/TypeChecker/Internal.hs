module Language.TypeChecker.Internal where

import Control.Monad.State
import Control.Exception ( throw )
import Data.Maybe
import Exception ( CompilerException(..) )
import Language.Language ( Declaration(..), Expression(..) )
import Control.Monad (foldM)

data LanguageType
    = IntegerType
    | TypeVariable Int
    | FunctionType LanguageType LanguageType
    deriving Eq

typeVariableNames :: [String]
typeVariableNames = [replicate cnt v | cnt <- [1..], v <- ['a'..'z']]

instance Show LanguageType where
    show ty = case ty of
        IntegerType -> "int"
        TypeVariable value -> typeVariableNames !! value
        FunctionType left right ->
            case left of
                FunctionType _ _ -> "(" ++ show left ++ ") -> " ++ show right
                _ -> show left ++ " -> " ++ show right

-- Generate a new unique type variable. 
fresh :: State Int LanguageType
fresh = do
    n <- get
    put (n + 1)
    return (TypeVariable n)

substitute :: [(LanguageType, LanguageType)] -> LanguageType -> LanguageType
substitute substitution typ =
    case typ of
        TypeVariable _ ->
            Data.Maybe.fromMaybe typ (lookup typ substitution)
        IntegerType -> IntegerType
        FunctionType left right ->
            FunctionType (substitute substitution left) (substitute substitution right)

composeSubstitution :: [(LanguageType, LanguageType)] -> [(LanguageType, LanguageType)] -> [(LanguageType, LanguageType)]
composeSubstitution first second =
    first ++ map (\(from, to) -> (from, substitute first to)) second

substituteEquations :: [(LanguageType, LanguageType)] -> [(LanguageType, LanguageType)] -> [(LanguageType, LanguageType)]
substituteEquations substitution =
    map (\(a, b) -> (substitute substitution a, substitute substitution b))

freeTypeVariables :: LanguageType -> [Int]
freeTypeVariables typ =
    case typ of
        IntegerType -> []
        TypeVariable v -> [ v ]
        FunctionType left right ->
            freeTypeVariables left ++ freeTypeVariables right

isBound :: Int -> LanguageType -> Bool
isBound typeVariable ty =
    typeVariable `notElem` freeTypeVariables ty

unify :: [(LanguageType, LanguageType)] -> [(LanguageType, LanguageType)]
unify equations =
    case equations of
        [] -> []
        (FunctionType a b, FunctionType a' b') : rest ->
            unify ((a, a') : (b, b') : rest)
        (TypeVariable v1, ty) : rest | isBound v1 ty ->
            let substitution = unify (substituteEquations [ (TypeVariable v1, ty) ] equations)
            in composeSubstitution [(TypeVariable v1, substitute substitution ty)] substitution
        (ty, TypeVariable v1) : rest | isBound v1 ty ->
            let substitution = unify (substituteEquations [ (TypeVariable v1, ty) ] equations)
            in composeSubstitution [(TypeVariable v1, substitute substitution ty)] substitution
        (first, second) : rest | first == second -> unify rest
        (first, second) : _ ->
            throw $ TypeCheckException $ "Types '" ++ show first ++ "' and '" ++ show second ++ "' can't be unified."

-- infer :: [Declaration] -> [([(String, LanguageType)], [(LanguageType, LanguageType)])]
infer program =
    let
        (names, equations) = evalState (inferDeclarations program) 0
        substitution = unify equations
    in
        --let (typ, equations) = evalState (inferExpression [] expression) 0
        --in substitute (unify equations) typ
        map (\(name, typ) -> (name, substitute substitution typ)) names

inferDeclarations :: [Declaration] -> State Int ([(String, LanguageType)], [(LanguageType, LanguageType)])
inferDeclarations = foldM (\(types, equations) declaration -> do
    (inferredType, inferredEquations) <- inferDeclaration declaration
    return (inferredType : types, inferredEquations ++ equations)
    ) ([], [])

inferDeclaration :: Declaration -> State Int ((String, LanguageType), [(LanguageType, LanguageType)])
inferDeclaration declaration = do
    case declaration of
        ValueDeclaration name expression -> do
            (inferredType, inferredEquations) <- inferExpression [] expression
            return ((name, inferredType), inferredEquations)
        _ -> throw NotImplementedException

inferProgram :: [Declaration] -> State Int (LanguageType, [(LanguageType, LanguageType)])
inferProgram program = do
    case program of
        [] -> return (IntegerType, [(IntegerType, IntegerType)])
        (ValueDeclaration name expression) : rest ->
            inferExpression [] expression >> inferProgram rest

inferExpression :: [(String, LanguageType)] -> Expression -> State Int (LanguageType, [(LanguageType, LanguageType)])

inferExpression context (Integer _) = return (IntegerType, [])

inferExpression context (Variable name) =
    case lookup name context of
        Just value -> return (value, [])
        Nothing -> throw (TypeCheckException ("Unknown type of variable '"++ name ++ "'."))

inferExpression context (Binary _ left right) = do
    (typeLeft, equationsLeft) <- inferExpression context left
    (typeRight, equationsRight) <- inferExpression context right
    return (IntegerType, (typeLeft, IntegerType) : (typeRight, IntegerType) : equationsLeft ++ equationsRight)

inferExpression context (Lambda parameter expression) = do
    alpha <- fresh
    (ty, equations) <- inferExpression ((parameter, alpha) : context) expression
    return (FunctionType alpha ty, equations)

inferExpression context (Application left right) = do
    alpha <- fresh
    (typeLeft, equationsLeft) <- inferExpression context left
    (typeRight, equationsRight) <- inferExpression context right
    return (alpha, (typeLeft, FunctionType typeRight alpha) : equationsLeft ++ equationsRight)

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
    throw NotImplementedException