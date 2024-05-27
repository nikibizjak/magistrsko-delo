module Stg.Parser.Internal (parse, ParserException(..)) where

import Stg.Parser.Utils
import Stg.Stg
import Prelude hiding (fail, return, (>>), (>>=), (||))
import Stg.Types (TypeHint(TypeHint), StgType (..), Lifetime (NamedLifetime, StaticLifetime))

newtype ParserException = ParserException String

underscore = exactly '_'

-- Comment: starts with -- and ends with a newline
comment :: Parser ()
comment = word "--" >> many notLineSeparator >> return ()

spaces :: Parser ()
spaces = many (oneOf [space, comment]) >> return ()

spaces1 :: Parser ()
spaces1 = many1 (oneOf [space, comment]) >> return ()

-- identifier = [_a-zA-Z][_a-zA-Z0-9]*
identifier :: Parser String
identifier =
  oneOf [underscore, alphaAny] >>= \first ->
    many (oneOf [underscore, alphaAny, digit]) >>= \rest ->
      return (first : rest)

reservedWords :: [String]
reservedWords = ["case", "of", "let", "in", "THUNK", "FUN", "PAP", "CON", "BLACKHOLE"]

variable :: Parser Variable
variable =
  identifier >>= \name ->
    -- Must not be a reserved word!
    if name `elem` reservedWords
      then fail
      else return name

borrow :: Parser Atom
borrow =
  exactly '&' >> spaces >> variable >>= \name ->
    return $ Borrow name

primitiveInteger :: Parser Int
primitiveInteger =
  many1 digit >>= \value ->
    return (read value)

integer :: Parser Literal
integer =
  primitiveInteger >>= \value ->
    return $ Integer value

literal :: Parser Literal
literal = integer

atom :: Parser Atom
atom =
  oneOf
    [
      borrow,
      variable >>= \value -> return $ Variable value,
      literal >>= \value -> return $ Literal value
    ]

atomicExpression :: Parser Expression
atomicExpression =
  atom >>= \value -> return $ Atom value

letIn :: Parser Expression
letIn =
  word "let" >> spaces1 >> many1 binding >>= \binds ->
    spaces >> word "in" >> spaces >> expression >>= \body ->
      return $ foldr (\(Binding name value) current -> LetIn name value current) body binds

defaultAlternative :: Parser Alternative
defaultAlternative =
    variable >>= \name ->
    spaces >> word "->" >> spaces >> expression >>= \expression ->
    return (DefaultAlternative name expression)

algebraicAlternative :: Parser Alternative
algebraicAlternative =
    identifier >>= \constructor ->
    many (spaces1 >> variable) >>= \parameters ->
    spaces >> word "->" >> spaces >> expression >>= \body ->
    return (AlgebraicAlternative constructor parameters body)

alternative :: Parser Alternative
alternative =
    exactly '|' >> spaces >>
    oneOf [ algebraicAlternative, defaultAlternative ]

caseOf :: Parser Expression
caseOf =
  word "case" >> spaces1 >> expression >>= \scrutinee ->
    spaces1 >> word "of" >> spaces >>
    many1 (spaces >> alternative) >>= \alternatives ->
    return (CaseOf scrutinee alternatives)

-- If only one atom, then this is an atomic expression, otherwise, this is a
-- function application.
application :: Parser Expression
application =
  variable >>= \function ->
    many (spaces1 >> atom) >>= \arguments ->
      if not (null arguments)
        then return (FunctionApplication function Unknown arguments)
        else return (Atom $ Variable function)

grouping :: Parser b -> Parser b
grouping parser =
    exactly '(' >> spaces >> parser >>= \subexpression ->
        spaces >> exactly ')' >> return subexpression

expression :: Parser Expression
expression =
  oneOf
    [ letIn,
      caseOf,
      grouping expression,
      application,
      atomicExpression
    ]

function :: Parser Object
function =
  word "FUN"
    >> spaces
    >> exactly '('
    >> many1 (spaces >> variable)
    >>= \parameters ->
      spaces
        >> word "->"
        >> spaces
        >> expression
        >>= \body ->
          spaces
            >> exactly ')'
            >> spaces
            >> return (Function parameters body)

partialApplication :: Parser Object
partialApplication =
  word "PAP"
    >> spaces
    >> exactly '('
    >> spaces
    >> variable
    >>= \function ->
      many (spaces >> atom) >>= \arguments ->
        spaces
          >> exactly ')'
          >> spaces
          >> return (PartialApplication function arguments)

constructor :: Parser Object
constructor =
  word "CON"
    >> spaces
    >> exactly '('
    >> spaces
    >> identifier
    >>= \name ->
      many (spaces >> atom) >>= \arguments ->
        spaces
          >> exactly ')'
          >> spaces
          >> return (Constructor name arguments)

thunk :: Parser Object
thunk =
  word "THUNK" >> spaces >> exactly '(' >> spaces >> expression >>= \body ->
    spaces >> exactly ')' >> spaces >> return (Thunk body)

blackhole :: Parser Object
blackhole =
  word "BLACKHOLE" >> return BlackHole

object :: Parser Object
object =
  oneOf
    [ function,
      partialApplication,
      constructor,
      thunk,
      blackhole
    ]

binding :: Parser Binding
binding =
  identifier >>= \name ->
    spaces >> exactly '=' >> spaces >> object >>= \body ->
      return (Binding name body)

namedType :: Parser StgType
namedType =
  identifier >>= \name ->
    return $ NamedType name

lifetime :: Parser Lifetime
lifetime =
  exactly '\'' >> identifier >>= \name ->
    case name of
      "static" -> return StaticLifetime
      _ -> return $ NamedLifetime name

referenceType :: Parser StgType
referenceType =
 exactly '&' >> spaces >> lifetime >>= \lifetime ->
  spaces >> stgType >>= \subtype ->
    return $ ReferencedType lifetime subtype

productType :: Parser StgType
productType =
  exactly '(' >> spaces >>
    oneOf [
      -- An empty product type () - Unit type
      exactly ')' >> return (ProductType []),
      -- Non-empty product type (T_1, T_2, ..., T_n)
      stgType >>= \first ->
        many (exactly ',' >> spaces >> stgType) >>= \rest ->
          spaces >> exactly ')' >>
            return (ProductType $ first : rest)
    ]

atomicType :: Parser StgType
atomicType =
  oneOf [ productType, namedType, referenceType, grouping stgType ]

arrowType :: Parser StgType
arrowType =
  atomicType >>= \first ->
    oneOf [
      spaces >> word "->" >> spaces >> arrowType >>= \next ->
        return $ ArrowType first next,
      return first
    ]

stgType :: Parser StgType
stgType = arrowType

typeHint :: Parser TypeHint
typeHint =
  identifier >>= \name ->
    spaces >> word "::" >> spaces >> stgType >>= \stgType ->
      return (TypeHint name stgType)

declaration :: Parser Declaration
declaration = spaces >> oneOf [
  binding >>= \result -> return $ BindingDeclaration result,
  typeHint >>= \result -> return $ TypeHintDeclaration result
  ]

declarations :: Parser [Declaration]
declarations = many declaration

data Declaration
  = BindingDeclaration Binding
  | TypeHintDeclaration TypeHint

splitDeclarations :: [Declaration] -> ([Binding], [TypeHint])
splitDeclarations declarations =
  case declarations of
    [] -> ([], [])
    head : rest ->
      let
        (bindings, typeHints) = splitDeclarations rest
      in
        case head of
          (BindingDeclaration binding) -> (binding : bindings, typeHints)
          (TypeHintDeclaration hint) -> (bindings, hint : typeHints)

parse :: String -> Either ParserException (Program, [TypeHint])
parse input =
  case declarations input of
    Just (result, []) ->
      let (bindings, typeHints) = splitDeclarations result in
        Right (bindings, typeHints)
    _ -> Left $ ParserException "Invalid input"