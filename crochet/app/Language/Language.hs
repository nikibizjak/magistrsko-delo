module Language.Language (
    BinaryOperator(..),
    Expression(..),
    Alternative(..),
    Declaration(..),
    Program(..),
    TypeDefinition(..),
    TypeHint(..),
    Constructor,
    isValueDeclaration,
    isTypeDeclaration,
    isTypeHintDeclaration,
) where

type Program = [ Declaration ]

type Constructor = (String, [TypeDefinition])

data Declaration
    -- = TypeDeclaration String [Type] Type
    = TypeDeclaration String [String] [Constructor]
    | ValueDeclaration String Expression
    | TypeHintDeclaration String TypeHint

data TypeHint
    = IntegerTypeHint
    | ParametricTypeHint String
    | NamedTypeHint String [TypeHint]
    | FunctionTypeHint TypeHint TypeHint

data TypeDefinition
    = IntegerTypeDefinition
    | ParametricTypeDefinition String
    | NamedType String [TypeDefinition]

data Expression
    = Integer Int
    | Variable String
    | Binary BinaryOperator Expression Expression
    | Application Expression Expression
    | IfThenElse Expression Expression Expression
    | Lambda String Expression
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

-- Utility functions
isValueDeclaration :: Declaration -> Bool
isValueDeclaration declaration =
    case declaration of
        ValueDeclaration {} -> True
        _ -> False
        
isTypeDeclaration :: Declaration -> Bool
isTypeDeclaration declaration =
    case declaration of
        TypeDeclaration {} -> True
        _ -> False

isTypeHintDeclaration :: Declaration -> Bool
isTypeHintDeclaration declaration =
    case declaration of
        TypeHintDeclaration {} -> True
        _ -> False