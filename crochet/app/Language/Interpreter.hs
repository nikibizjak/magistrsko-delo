module Language.Interpreter where
import Language.Language
import Exception (CompilerException(InterpreterException, NotImplementedException))
import Control.Exception ( throw )
import Data.Foldable (find)



evaluate :: Program -> Expression
evaluate program =
    let
        isMain declaration =
            case declaration of
                ValueDeclaration "main" _ -> True
                _ -> False

        main = find isMain program
    in
        case main of
            Just (ValueDeclaration _ body) -> evalExpression [] body
            _ -> throw $ InterpreterException "Undefined function 'main'."

evalExpression :: [(String, Expression)] -> Expression -> Expression
evalExpression context expression =
    case expression of
        Integer _ -> expression
        Variable name ->
            case lookup name context of
                Just exp -> evalExpression context exp
                Nothing -> throw $ InterpreterException ("Undefined variable '" ++ name ++ "'.")
        Binary operator left right ->
            let
                leftValue = evalExpression context left
                rightValue = evalExpression context right
            in
                case (leftValue, rightValue) of
                    (Integer n, Integer m) ->
                        case operator of
                            Add -> Integer $ n + m
                            Subtract -> Integer $ n - m
                            Multiply -> Integer $ n * m
                            Divide -> Integer $ div n m
                            Modulo -> Integer $ mod n m
                    _ -> throw $ InterpreterException "Operands not integers"
        LetIn binds body ->
            let context' = binds ++ context
            in evalExpression context' body
        LetRec binds body ->
            let context' = binds ++ context
            in evalExpression context' body
        -- Application Expression Expression
        -- Lambda String Expression
        _ -> throw NotImplementedException