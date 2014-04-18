module Combinators
    ( module Parser
    , many, many1
    , char, anyChar, string, oneOf
    , digit, natural, integer
    , spaces
    , try
    , endBy, endBy1
    , sepBy, sepBy1
    , foldr1P, foldl1P
    , between, brackets, parens, braces, angles
    ) where

import Parser
import Data.Char

-- (0.5 балла)
char :: Char -> Parser ()
char ch = () <$ satisfy (== ch)

-- (0.5 балла)
anyChar :: Parser Char
anyChar = satisfy (const True)

-- (0.5 балла)
digit :: Parser Int
digit = read . (:[]) <$> satisfy isDigit

-- (0.5 балла)
string :: String -> Parser ()
string = foldr1 (*>) . map char

charDigit :: Parser Char
charDigit = oneOf "0123456789"

minus :: Parser Char
minus = oneOf "-"

-- (0.5 балла)
oneOf :: String -> Parser Char
oneOf set = satisfy (`elem` set)

-- (0.5 балла)
many :: Parser a -> Parser [a]
many p = ((:) <$> p <*> (many p <|> pure [])) <|> pure []

-- (0.5 балла)
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> (many p <|> pure [])

-- (0.5 балла)
natural :: Parser Integer
natural = read <$> many1 charDigit

-- (0.5 балла)
integer :: Parser Integer
integer = natural <|> read <$> ((:) <$> minus <*> many1 charDigit)

-- (0.5 балла)
spaces :: Parser ()
spaces = (fmap (const ()) <$> many1 $ char ' ') <|> pure ()

-- (0.5 балла)
try :: Parser a -> Parser (Maybe a)
try p = Just <$> p <|> pure Nothing

-- (0.5 балла)
endBy :: Parser a -> Parser b -> Parser [a]
endBy p1 p2 = many $ p1 <* p2

-- (0.5 балла)
endBy1 :: Parser a -> Parser b -> Parser [a]
endBy1 p1 p2 = many1 $ p1 <* p2

invEndBy :: Parser a -> Parser b -> Parser [b]
invEndBy p1 p2 = many $ p1 *> p2

invEndBy1 :: Parser a -> Parser b -> Parser [b]
invEndBy1 p1 p2 = many1 $ p1 *> p2

-- (0.5 балла)
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p1 p2 = (:) <$> p1 <*> invEndBy p2 p1 <|> pure []

-- (0.5 балла)
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p1 p2 = (:) <$> p1 <*> invEndBy1 p2 p1

-- (0.1 балла)
between :: Parser a -> Parser b -> Parser c -> Parser c
between a b c = a *> c <* b

-- (0.1 балла)
brackets :: Parser a -> Parser a
brackets p = between (char '[') (char ']') p

-- (0.1 балла)
parens :: Parser a -> Parser a
parens p = between (char '(') (char ')') p

-- (0.1 балла)
braces :: Parser a -> Parser a
braces p = between (char '{') (char '}') p

-- (0.1 балла)
angles :: Parser a -> Parser a
angles p = between (char '<') (char '>') p

-- (1 балл)
foldr1P :: (a -> b -> a -> a) -> Parser a -> Parser b -> Parser a
foldr1P f p1 p2 = undefined

-- (1 балл)
foldl1P :: (a -> b -> a -> a) -> Parser a -> Parser b -> Parser a
foldl1P = undefined
