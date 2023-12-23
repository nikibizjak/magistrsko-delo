module Language.Language (
    BinaryOperator(..),
    Expression(..),
    Alternative(..),
    Declaration(..),
    Program(..),
    Type(..),
    Constructor,
    TypeHint(..),
) where

type Program = [ Declaration ]

data Declaration
    = TypeDeclaration String [Type] Type
    | ValueDeclaration String [String] Expression
    | TypeHintDeclaration String TypeHint

data TypeHint
    = IntegerTypeHint
    | ParametricTypeHint String
    | NamedTypeHint String [TypeHint]
    | FunctionTypeHint TypeHint TypeHint

data Type
    = IntegerType
    | NamedType String [Type]
    | ParametricType String
    | SumType [(String, Type)]
    | ProductType [Type]

type Constructor = (String, Type)

data Expression
    = Integer Int
    | Variable String
    | Binary BinaryOperator Expression Expression
    | Application Expression Expression
    | IfThenElse Expression Expression Expression
    | Lambda [String] Expression
    | LetIn [(String, Expression)] Expression
    | LetRec [(String, Expression)] Expression
    | CaseOf Expression [Alternative]

data BinaryOperator
    = Add
    | Subtract
    | Multiply
    | Divide
    | Modulo

data Alternative
    = AlgebraicAlternative String [String] Expression
    | PrimitiveAlternative Int Expression
    | NamedDefaultAlternative String Expression
    | DefaultAlternative Expression
