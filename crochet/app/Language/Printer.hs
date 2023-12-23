module Language.Printer where

import Language.Language
    ( Alternative(..),
      BinaryOperator(..),
      Constructor,
      Declaration(..),
      Expression(..),
      Type(..),
      TypeHint(..) )
import Control.Arrow (Arrow(second))

-- PRINTING FUNCTIONS
unseparators :: [Char] -> [String] -> String
unseparators _ [] = ""
unseparators _ [last] = last
unseparators separator (current : next : rest) = current ++ separator ++ uncommas (next : rest)

uncommas :: [String] -> String
uncommas = unseparators ", "

instance Show BinaryOperator where
    show operator = case operator of
        Add -> "+"
        Subtract -> "-"
        Multiply -> "*"
        Divide -> "/"
        Modulo -> "%"

instance Show Expression where
  show expression = case expression of
    Integer n -> show n
    Variable name -> name
    Binary operator left right ->
        "(" ++ show left ++ " " ++ show operator ++ " " ++ show right ++ ")"
    IfThenElse condition thenBranch elseBranch ->
        "if " ++ show condition ++ " then " ++ show thenBranch ++ " else " ++ show elseBranch
    Lambda parameters body ->
        "fun " ++ show parameters ++ " -> " ++ show body
    Application left right -> "(" ++ show left ++ " " ++ show right ++ ")"
    LetIn binds body ->
        "let " ++ uncommas (map (\(name, value) -> name ++ " = " ++ show value) binds) ++ " in " ++ show body
    LetRec binds body ->
        "let* " ++ uncommas (map (\(name, value) -> name ++ " = " ++ show value) binds) ++ " in " ++ show body
    CaseOf expression alternatives ->
        "case " ++ show expression ++ " of \n" ++ unwords (map show alternatives)

instance Show Alternative where
    show alternative = case alternative of
        AlgebraicAlternative constructor names body ->
            "| " ++ constructor ++ " " ++ unwords names ++ " -> " ++ show body
        PrimitiveAlternative value body ->
            "| " ++ show value ++ " -> " ++ show body
        NamedDefaultAlternative name body ->
            "| " ++ name ++ " -> " ++ show body
        DefaultAlternative body ->
            "| _ -> " ++ show body

instance Show Declaration where
    show declaration = case declaration of
        ValueDeclaration name arguments body ->
            if not (null arguments)
                then name ++ " " ++ unwords arguments ++ " = " ++ show body
                else name ++ " = " ++ show body
        TypeDeclaration name parameters t ->
            let
                parametersClean = if null parameters then "" else " " ++ parametersString
                parametersString = unwords (map show parameters)
            in
                "type " ++ name ++ parametersClean ++ " = " ++ show t
        TypeHintDeclaration name typeHint ->
            name ++ " :: " ++ show typeHint

showConstructor :: Constructor -> String
showConstructor (name, t) =
    let
        valueString = show t
        valueClean = if null valueString then "" else ' ' : valueString
    in
        name ++ valueClean

instance Show Type where
    show t = case t of
        IntegerType -> "int"
        NamedType name parameters ->
            let
                parametersClean = if null parameters then "" else " " ++ parametersString
                parametersString = unwords (map show parameters)
            in
                name ++ parametersClean
        ParametricType name -> '\'' : name
        SumType types -> unseparators " | " (map showConstructor types)
        ProductType types -> unseparators " * " (map show types)

instance Show TypeHint where
    show t = case t of
        IntegerTypeHint -> "int"
        ParametricTypeHint name -> '\'' : name
        NamedTypeHint name parameters ->
            let
                showParameter parameter =
                    case parameter of
                        NamedTypeHint _ _ -> '(' : show parameter ++ ")"
                        FunctionTypeHint _ _ -> '(' : show parameter ++ ")"
                        _ -> show parameter
            in
                name ++ " " ++ unwords (map showParameter parameters)
        FunctionTypeHint first second ->
            let
                wrapFunctionTypeHint hint =
                    case hint of
                        FunctionTypeHint _ _ -> '(' : show hint ++ ")"
                        _ -> show hint
            in
                wrapFunctionTypeHint first ++ " -> " ++ show second