
module ParserUtils where

import Data.Void
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

consumeLine :: Parser String
consumeLine = do
  str <- many (satisfy (/= '\n'))
  char '\n'
  return str

consumeUntil :: (Char -> Bool) -> Parser String
consumeUntil cond = do
  str <- many (satisfy cond)
  return str

sc :: Parser ()
sc = L.space hspace1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

signedInteger :: Parser Int
signedInteger = L.signed sc integer

symbol :: String -> Parser String
symbol = L.symbol sc

comma :: Parser String
comma = symbol ","

semicolon :: Parser String
semicolon = symbol ";"

colon :: Parser String
colon = symbol ":"

dot :: Parser String
dot = symbol "."

pipe :: Parser String
pipe = symbol "|"

isEmpty :: String -> Bool
isEmpty = all isSpace

trim :: String -> String
trim input = reverse flippedTrimmed
  where
    trimStart = dropWhile isSpace input
    flipped = reverse trimStart
    flippedTrimmed = dropWhile isSpace flipped
