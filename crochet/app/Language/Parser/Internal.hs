module Language.Parser.Internal (
    parse, parseExpression
) where

import Language.Parser.Utils
import Prelude hiding (lines, (>>), (||), return, fail, (>>=))
import Data.Char ( isDigit, isSpace, isAsciiLower, isAsciiUpper )
import Exception ( CompilerException(ParserException) )
import Control.Exception ( throw )
import Language.Language
    ( Alternative(..),
      BinaryOperator(Modulo, Add, Subtract, Multiply, Divide),
      Constructor,
      Declaration(..),
      Expression(..),
      Program,
      Type(..),
      TypeHint(..) )

symbolToOperator symbol =
    case symbol of
        '+' -> Add
        '-' -> Subtract
        '*' -> Multiply
        '/' -> Divide
        '%' -> Modulo

underscore = exactly '_'

-- identifier = [_a-zA-Z][_a-zA-Z0-9]*
identifier :: Parser String
identifier =
    oneOf [ underscore, alphaAny ] >>= \first ->
        many (oneOf [ underscore, alphaAny, digit ]) >>= \rest ->
            return (first : rest)

reservedWords :: [String]
reservedWords = [ "if", "then", "else", "case", "of", "fun", "let", "let*", "in", "data" ]

variable :: Parser Expression
variable =
    identifier >>= \name ->
        -- Must not be a reserved word!
        if name `elem` reservedWords
            then fail
            else return (Variable name)

primitiveInteger :: Parser Int
primitiveInteger =
    many1 digit >>= \value ->
        return (read value)

integer :: Parser Expression
integer =
    primitiveInteger >>= \value ->
        return (Integer value)

application :: Parser Expression
application =
    expression0 >>= \first ->
    many (spaces1 >> expression0) >>= \others ->
        return (foldl Application first others)

expression0 :: Parser Expression
expression0 = oneOf [ integer, variable, grouping expression ]

expression1 :: Parser Expression
expression1 = application

leftAssociativeOperator :: Parser Expression -> Parser Char -> Parser Expression -> Parser Expression
leftAssociativeOperator leftParser operatorParser rightParser =
    leftParser >>= \first ->
    many (spaces >> operatorParser >>= \operator ->
    spaces >> rightParser >>= \next -> return (operator, next)) >>= \others ->
    return (foldl (\current (symbol, next) -> Binary (symbolToOperator symbol) current next) first others)

multiplicativeExpression :: Parser Expression
multiplicativeExpression =
    leftAssociativeOperator expression1 (exactly '*' || exactly '/' || exactly '%') expression1

expression2 :: Parser Expression
expression2 = multiplicativeExpression

additiveExpression :: Parser Expression
additiveExpression =
    leftAssociativeOperator expression2 (exactly '+' || exactly '-') expression2

expression3 :: Parser Expression
expression3 = additiveExpression

ifThenElse :: Parser Expression
ifThenElse =
    word "if" >> spaces1 >> expression3 >>= \condition ->
    spaces1 >> word "then" >> spaces1 >> expression3 >>= \thenBranch ->
    spaces1 >> word "else" >> spaces1 >> expression3 >>= \elseBranch ->
    return (IfThenElse condition thenBranch elseBranch)

lambda :: Parser Expression
lambda =
    word "fun" >> many (spaces1 >> identifier) >>= \parameters ->
    spaces1 >> word "->" >> spaces1 >> expression >>= \body ->
    return (foldr Lambda body parameters)

bind :: Parser (String, Expression)
bind =
    identifier >>= \name ->
    spaces1 >> arguments >>= \args ->
    spaces >> exactly '=' >> spaces >> expression >>= \body ->
    return (name, foldr Lambda body args)

binds :: Parser [(String, Expression)]
binds = bind >>= \first ->
    many (spaces >> exactly ';' >> spaces >> bind) >>= \others ->
        return (first : others)

letIn :: Parser Expression
letIn =
    word "let" >> spaces1 >> binds >>= \binds ->
        spaces >> word "in" >> spaces >> expression >>= \body ->
            return (LetIn binds body)

letRec :: Parser Expression
letRec =
    word "let*" >> spaces1 >> binds >>= \binds ->
    spaces >> word "in" >> spaces >>expression >>= \body ->
    return (LetRec binds body)

defaultAlternative :: Parser Alternative
defaultAlternative =
    exactly '_' >> spaces1 >> word "->" >> spaces >> expression2 >>= \expression ->
        return (DefaultAlternative expression)

namedDefaultAlternative :: Parser Alternative
namedDefaultAlternative =
    identifier >>= \name ->
    spaces1 >> word "->" >> spaces >> expression2 >>= \expression ->
    return (NamedDefaultAlternative name expression)

algebraicAlternative :: Parser Alternative
algebraicAlternative =
    identifier >>= \constructor ->
    many (spaces1 >> identifier) >>= \names ->
    spaces >> word "->" >> spaces >> expression2 >>= \body ->
    return (AlgebraicAlternative constructor names body)

primitiveAlternative :: Parser Alternative
primitiveAlternative =
    primitiveInteger >>= \value ->
    spaces1 >> word "->" >> spaces >> expression2 >>= \expression ->
    return (PrimitiveAlternative value expression)

alternative :: Parser Alternative
alternative =
    exactly '|' >> spaces >>
    oneOf [ primitiveAlternative, algebraicAlternative, defaultAlternative, namedDefaultAlternative ]

caseOf :: Parser Expression
caseOf =
    word "case" >> spaces1 >> expression >>= \condition ->
    spaces1 >> word "of" >> spaces1 >> many1 (spaces >> alternative) >>= \alternatives ->
    return (CaseOf condition alternatives)

expression4 :: Parser Expression
expression4 = oneOf [ lambda, letIn, letRec, ifThenElse, caseOf, expression3 ]

expression :: Parser Expression
expression = expression4

arguments :: Parser [String]
arguments = many (spaces >> identifier)

valueDeclaration :: Parser Declaration
valueDeclaration =
    identifier >>= \name ->
    spaces1 >> arguments >>= \args ->
    spaces >> exactly '=' >> spaces >> expression >>= \body ->
    return (ValueDeclaration name (foldr Lambda body args))

parametricType :: Parser Type
parametricType =
    exactly '\'' >> identifier >>= \name ->
    return (ParametricType name)

namedType :: Parser Type
namedType =
    identifier >>= \name ->
    many (spaces1 >> parametricType) >>= \parameters ->
    return (NamedType name parameters)

parseType :: Parser Type
parseType =
    oneOf [
        word "int" >> return IntegerType,
        namedType,
        parametricType,
        grouping parseType
    ] >>= \t ->
    spaces >> return t

constructor :: Parser Constructor
constructor =
    exactly '|' >> spaces >> identifier >>= \name ->
    spaces >> many parseType >>= \types ->
    return (name, ProductType types)

typeDeclaration :: Parser Declaration
typeDeclaration =
    word "data" >> spaces1 >> identifier >>= \name ->
    many (spaces1 >> parametricType) >>= \parameters ->
    spaces >> exactly '=' >> spaces >>
    many1 constructor >>= \constructors ->
    return (TypeDeclaration name parameters (SumType constructors))

integerTypeHint :: Parser TypeHint
integerTypeHint =
    word "int" >> exactlySpaces >> return IntegerTypeHint

parametricTypeHint :: Parser TypeHint
parametricTypeHint =
    exactly '\'' >> identifier >>= \name ->
    exactlySpaces >> return (ParametricTypeHint name)

namedTypeHint :: Parser TypeHint
namedTypeHint =
    identifier >>= \name ->
    many (exactlySpaces >> typeHint0) >>= \parameters ->
    exactlySpaces >>
    return (NamedTypeHint name parameters)

groupingOnlySpaces :: Parser b -> Parser b
groupingOnlySpaces parser =
    exactly '(' >> exactlySpaces >> parser >>= \subexpression ->
        exactlySpaces >> exactly ')' >> return subexpression

typeHint0 :: Parser TypeHint
typeHint0 =
    oneOf [
        integerTypeHint,
        namedTypeHint,
        parametricTypeHint,
        groupingOnlySpaces typeHint
    ]

functionTypeHint :: Parser TypeHint
functionTypeHint =
    typeHint0 >>= \first ->
    exactlySpaces >>
    many1 (
        word "->" >>
        exactlySpaces >>
        typeHint >>= \hint ->
        exactlySpaces >> return hint
    ) >>= \others ->
        return (foldl FunctionTypeHint first others)

typeHint :: Parser TypeHint
typeHint =
    oneOf [
        functionTypeHint,
        typeHint0
    ]

typeHintDeclaration :: Parser Declaration
typeHintDeclaration =
    identifier >>= \name ->
    exactlySpaces >> word "::" >> exactlySpaces >> typeHint >>= \hint ->
    exactlySpaces >> lines >> exactlySpaces >>
    return (TypeHintDeclaration name hint)

twoSemicolon :: Parser b -> Parser b
twoSemicolon parser =
    parser >>= \result ->
    spaces >> word ";;" >> spaces >>
    return result

declaration :: Parser Declaration
declaration = spaces >> oneOf [
    twoSemicolon typeDeclaration ,
    twoSemicolon valueDeclaration,
    typeHintDeclaration ]

program :: Parser Program
program = many declaration

parse :: String -> Program
parse input = case program input of
    Just (result, []) -> result
    _ -> throw (ParserException "Could not parse input")

parseExpression :: String -> Expression
parseExpression input = case expression input of
    Just (result, []) -> result
    _ -> throw (ParserException "Could not parse input")
