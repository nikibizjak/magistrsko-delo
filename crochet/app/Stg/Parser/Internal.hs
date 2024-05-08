module Stg.Parser.Internal (parse, ParserException(..)) where

import Stg.Parser.Utils
import Stg.Stg
import Prelude hiding (fail, return, (>>), (>>=), (||))

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
  word "let" >> spaces1 >> binding >>= \(Binding name value) ->
    spaces >> word "in" >> spaces >> expression >>= \body ->
      return (LetIn name value body)

defaultAlternative :: Parser Alternative
defaultAlternative =
    variable >>= \name ->
    spaces1 >> word "->" >> spaces >> expression >>= \expression ->
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
    spaces1 >> word "of" >> spaces1 >>
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
    spaces1 >> exactly '=' >> spaces >> object >>= \body ->
      return (Binding name body)

declaration :: Parser Binding
declaration = spaces >> binding

program :: Parser Program
program = many declaration

parse :: String -> Either ParserException Program
parse input =
  case program input of
    Just (result, []) -> Right result
    _ -> Left $ ParserException "Invalid input"