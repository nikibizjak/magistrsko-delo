{-# LANGUAGE InstanceSigs #-}
module Stg.Stg where

type Program = [ Binding ]

type Binding = (Variable, LambdaForm)

data UpdateFlag
    = Updatable
    | NotUpdatable

data LambdaForm
    = Lambda [ Variable ] UpdateFlag [ Variable ] Expression

freeVariables :: LambdaForm -> [Variable]
freeVariables (Lambda vars _ _ _) = vars

arguments :: LambdaForm -> [Variable]
arguments (Lambda _ _ args _) = args

data Expression
    = LetIn [ Binding ] Expression
    | LetRec [ Binding ] Expression
    | CaseOf Expression [ Alternative ]
    | Application Variable [ Atom ]
    | ConstructorExpression Constructor [ Atom ]
    | BuiltInOperation PrimitiveOperation [ Atom ]
    | LiteralExpression Literal

data Alternative
    = PrimitiveAlternative Literal Expression
    | AlgebraicAlternative Constructor [Variable] Expression
    | NamedDefaultAlternative Variable Expression
    | DefaultAlternative Expression
    deriving (Show)

isDefaultAlternative :: Alternative -> Bool
isDefaultAlternative alternative =
    case alternative of
    NamedDefaultAlternative _ _ -> True
    DefaultAlternative _ -> True
    _ -> False

data PrimitiveOperation
    = Addition
    | Subtraction
    | Multiplication
    | Division

data Atom
    = V Variable
    | L Literal

type Constructor = String
type Literal = Int
type Variable = String

prettify :: Program -> String
prettify = programToString

programToString :: Program -> String
programToString = unlines . map bindingToString

bindingToString :: (Variable, LambdaForm) -> String
bindingToString (name, lambda) = name ++ " = " ++ show lambda

sequenceToString :: [String] -> String
sequenceToString items =
    let
        inner xs =
            case xs of
                [] -> ""
                [last] -> last
                head : next : rest -> head ++ ", " ++ sequenceToString (next : rest)
    in
        "{" ++ inner items ++ "}"

atomsToString :: Show a => [a] -> String
atomsToString = sequenceToString . map show

instance Show Expression where
    show :: Expression -> String
    show expression = case expression of
        LetIn bindings body ->
            "let " ++ unwords (map (\(name, value) -> name ++ " = " ++ show value) bindings) ++ " in " ++ show body
        LiteralExpression value -> show value
        Application f arguments ->
            "(" ++ f ++ " " ++ atomsToString arguments ++ ")"
        BuiltInOperation operation arguments ->
            show operation ++ " " ++ atomsToString arguments
        _ -> "TODO"

instance Show PrimitiveOperation where
    show :: PrimitiveOperation -> String
    show operation = case operation of
        Addition -> "+#"
        Subtraction -> "-#"
        Multiplication -> "*#"
        Division -> "/#"

instance Show Atom where
    show :: Atom -> String
    show atom = case atom of
        V name -> name
        L value -> show value ++ "#"

instance Show LambdaForm where
    show :: LambdaForm -> String
    show lambda = case lambda of
        Lambda freeVariables updatable arguments body ->
            sequenceToString freeVariables ++ " " ++ show updatable ++ " " ++ sequenceToString arguments ++ " -> " ++ show body

instance Show UpdateFlag where
    show :: UpdateFlag -> String
    show flag = case flag of
        Updatable -> "\\u"
        NotUpdatable -> "\\n"
