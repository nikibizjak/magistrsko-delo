module Language.Converter (toStg) where

import qualified Language.Language as Language
import qualified Stg.Stg as Stg
import Data.Maybe (mapMaybe)

toStg :: Language.Program -> Stg.Program
toStg = mapMaybe declarationToStg

declarationToStg :: Language.Declaration -> Maybe Stg.Binding
declarationToStg declaration =
    case declaration of
        Language.ValueDeclaration name body ->
            Just (name, Stg.Lambda [] Stg.Updatable [] (expressionToStg body))
        -- TODO: Translate type declaration into STG.
        Language.TypeDeclaration _ _ _ -> Nothing

expressionToStg :: Language.Expression -> Stg.Expression
expressionToStg expression =
    case expression of
        Language.Integer n -> Stg.LiteralExpression n
        Language.Variable name -> Stg.Application name []
        -- Language.Binary BinaryOperator Expression Expression
        -- Language.Application function argument -> Stg.Application function argument
        -- Language.IfThenElse Expression Expression Expression
        -- Language.Lambda [String] Expression ->
        -- Language.LetIn bindings body ->
        -- Language.LetRec [(String, Expression)] Expression
        -- Language.CaseOf Expression [Alternative]
        _ -> Stg.LiteralExpression 404