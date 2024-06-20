module Parser.Utils where

import Prelude hiding (lines, (>>), (||), return, fail, (>>=))
import Data.Char ( isDigit, isSpace, isAsciiLower, isAsciiUpper )

-- PARSER TYPE DEFINITION
type Parser a = String -> Maybe (a, String)

-- BASIC PARSERS
fail :: Parser a
fail _ = Nothing

return :: a -> Parser a
return value input = Just (value, input)

item :: Parser Char
item input =
    case input of
        [] -> Nothing
        first : rest -> Just (first, rest)

(||) :: Parser a -> Parser a -> Parser a
(||) parser1 parser2 input =
    case parser1 input of
        Nothing -> parser2 input
        Just (value, input') -> Just (value, input')

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
(>>=) parser1 parser2 = \input ->
    case parser1 input of
        Nothing -> Nothing
        Just (value, input') -> parser2 value input'

-- DERIVED PARSERS
(>>) :: Parser a -> Parser b -> Parser b
(>>) parser1 parser2 =
    parser1 >>= \_ -> parser2

satisfy :: (a -> Bool) -> Parser a -> Parser a
satisfy condition parser =
    parser >>= (\x -> if condition x then return x else fail)

character :: Parser Char
character = item

digit :: Parser Char
digit = satisfy isDigit character

alpha :: Parser Char
alpha = satisfy isAsciiLower character

alphaUpper :: Parser Char
alphaUpper = satisfy isAsciiUpper character

alphaAny :: Parser Char
alphaAny = oneOf [ alpha, alphaUpper ]

isLineSeparator :: Char -> Bool
isLineSeparator x = x `elem` "\n\r"

lineSeparator :: Parser Char
lineSeparator = satisfy isLineSeparator character

notLineSeparator :: Parser Char
notLineSeparator = satisfy (not . isLineSeparator) character

space :: Parser ()
space = satisfy isSpace character >> return ()

exactly :: Char -> Parser Char
exactly expected = satisfy (== expected) character

oneOf :: [ Parser a ] -> Parser a
oneOf = foldr (||) fail

word :: String -> Parser ()
word = foldr (\c parser -> exactly c >> parser) (return ())

many :: Parser a -> Parser [a]
many parser = many1 parser || return []

many1 :: Parser a -> Parser [a]
many1 parser =
    parser >>= \first ->
        many parser >>= \rest -> return (first : rest)

spaces :: Parser ()
spaces = many space >> return ()

spaces1 :: Parser ()
spaces1 = many1 space >> return ()

underscore :: Parser Char
underscore = exactly '_'