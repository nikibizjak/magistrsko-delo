module Language.Optimizer where

import Language.Language ( Program, Declaration(..), Expression(..), Alternative(..) )
import Data.Set as Set hiding (map)

deleteMore :: Ord a => [a] -> Set a -> Set a
deleteMore toDelete = Set.filter (`Set.notMember` Set.fromList toDelete)

freeVariables :: Expression -> Set String
freeVariables expression =
    case expression of
        Integer _ -> Set.empty
        Variable name -> Set.singleton name
        Binary _ left right -> Set.union (freeVariables left) (freeVariables right)
        IfThenElse condition thenExpression elseExpression ->
            Set.union (Set.union (freeVariables condition) (freeVariables thenExpression)) (freeVariables elseExpression)
        Application function argument ->
            Set.union (freeVariables function) (freeVariables argument)
        Lambda parameter body ->
            deleteMore [parameter] (freeVariables body)
        LetIn binds body ->
            let
                definedNames = map fst binds

                -- Names used in the head of the expression (LET)
                usedNamesLet = Set.unions $ map (freeVariables . snd) binds
                -- Names used in the body of the expression (IN)
                usedNamesIn = freeVariables body
                -- bodies = body : map snd binds
            in
                -- deleteMore definedNames $ Set.unions (map freeVariables bodies)
                deleteMore definedNames (Set.union usedNamesLet usedNamesIn)
        LetRec binds body ->
            let
                definedNames = map fst binds

                -- Names used in the head of the expression (LET)
                usedNamesLet = Set.unions $ map (freeVariables . snd) binds
                -- Names used in the body of the expression (IN)
                usedNamesIn = freeVariables body
                -- bodies = body : map snd binds
            in
                -- deleteMore definedNames $ Set.unions (map freeVariables bodies)
                deleteMore definedNames (Set.union usedNamesLet usedNamesIn)
        CaseOf expression alternatives ->
            Set.union (freeVariables expression) (Set.unions (map freeVariablesAlternative alternatives))

freeVariablesAlternative :: Alternative -> Set String
freeVariablesAlternative alternative =
    case alternative of
        AlgebraicAlternative _ parameters body ->
            deleteMore parameters $ freeVariables body
        PrimitiveAlternative _ body -> freeVariables body
        NamedDefaultAlternative name body ->
            Set.delete name $ freeVariables body
        DefaultAlternative body -> freeVariables body


removeUnnecessaryBinds :: Program -> Program
removeUnnecessaryBinds = map removeUnnecessaryBindsDeclaration

removeUnnecessaryBindsDeclaration :: Declaration -> Declaration
removeUnnecessaryBindsDeclaration declaration =
    case declaration of
        ValueDeclaration name expression ->
            ValueDeclaration name (removeUnnecessaryBindsExpression expression)
        -- TODO
        TypeDeclaration a b c -> TypeDeclaration a b c
        TypeHintDeclaration a b -> TypeHintDeclaration a b

removeUnnecessaryBindsExpression :: Expression -> Expression
removeUnnecessaryBindsExpression expression =
    case expression of
        Integer n -> Integer n
        Variable name -> Variable name
        Binary operator left right ->
            Binary operator (removeUnnecessaryBindsExpression left) (removeUnnecessaryBindsExpression right)
        IfThenElse condition thenExpression elseExpression ->
            IfThenElse (removeUnnecessaryBindsExpression condition) (removeUnnecessaryBindsExpression thenExpression) (removeUnnecessaryBindsExpression elseExpression)
        Application function argument ->
            Application (removeUnnecessaryBindsExpression function) (removeUnnecessaryBindsExpression argument)
        Lambda parameters body ->
            Lambda parameters (removeUnnecessaryBindsExpression body)
        LetIn binds body ->
            let
                body' = removeUnnecessaryBindsExpression body

                usedNamesLet = Set.unions (map (freeVariables . snd) binds)
                usedNamesIn = freeVariables body'
                usedNames = Set.union usedNamesLet usedNamesIn
                necessaryBinds = Prelude.filter (\(name, _) -> Set.member name usedNames) binds
            in
                LetIn necessaryBinds body'
        LetRec binds body ->
            let
                body' = removeUnnecessaryBindsExpression body

                usedNamesLet = Set.unions (map (freeVariables . snd) binds)
                usedNamesIn = freeVariables body'
                usedNames = Set.union usedNamesLet usedNamesIn
                necessaryBinds = Prelude.filter (\(name, _) -> Set.member name usedNames) binds
            in
                LetRec necessaryBinds body'
        CaseOf expression alternatives ->
            CaseOf (removeUnnecessaryBindsExpression expression) (map removeUnnecessaryBindsAlternative alternatives)

removeUnnecessaryBindsAlternative :: Alternative -> Alternative
removeUnnecessaryBindsAlternative alternative =
    case alternative of
        AlgebraicAlternative constructor parameters body ->
            AlgebraicAlternative constructor parameters (removeUnnecessaryBindsExpression body)
        PrimitiveAlternative n body ->
            PrimitiveAlternative n (removeUnnecessaryBindsExpression body)
        NamedDefaultAlternative name body ->
            NamedDefaultAlternative name (removeUnnecessaryBindsExpression body)
        DefaultAlternative body ->
            DefaultAlternative (removeUnnecessaryBindsExpression body)