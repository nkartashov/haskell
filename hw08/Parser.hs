-- Список экспорта менять нельзя!
module Parser
    ( Parser
    , pure, (<$>), (<$), (<*>), (<*), (*>)
    , empty, (<|>)
    , satisfy, eof
    , evalParser
    , parserTest
    ) where

import Control.Applicative
import Test.HUnit

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

evalParser :: Parser a -> String -> Maybe a
evalParser p s = fst <$> runParser p s

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser (\s -> case s of 
	[] -> Nothing
	(x : xs) -> case p x of
		True -> Just (x, xs)
		_ -> Nothing)

eof :: Parser ()
eof = Parser (\s -> case s of 
	[] -> Just ((), [])
	otherwise -> Nothing)

instance Functor Parser where
	fmap f p = Parser (\s ->  case runParser p s of
		Nothing -> Nothing
		Just (x, s') -> Just (f x, s'))

instance Applicative Parser where
	pure a = Parser (\s -> Just (a, s))
	lp <*> rp = Parser (\s -> case runParser lp s of
															Nothing -> Nothing
															Just (res, s') -> case runParser rp s' of
																									Nothing -> Nothing
																									Just (res', s'') -> Just (res res', s''))

instance Alternative Parser where
-- (0.5 балла)
	empty = Parser (\s -> Nothing)
-- (0.5 балла)
	lp <|> rp = Parser (\s -> case runParser lp s of
		Nothing -> runParser rp s
		Just v -> Just v)

parserTest :: (Eq a, Show a) => Parser a -> String -> Maybe (a,String) -> Test
parserTest (Parser p) s e = p s ~?= e
