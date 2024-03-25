module Token where

import Data.Char (isAlpha, isAlphaNum, isNumber, isSpace)
import Data.Word

data Token = UnknownChar Char | Ident String | Num Word64 | Comma | Colon | Plus | Newline

instance Show Token where
  show (UnknownChar c) = "UnknownChar" ++ show c
  show (Ident s) = "Ident " ++ show s
  show (Num x) = "Num " ++ show x
  show (Comma) = "Comma"
  show (Colon) = "Colon"
  show (Plus) = "Plus"
  show (Newline) = "Newline"

data Lexer = Lexer
  { lineCount :: Int,
    source :: String
  }

lexerNew :: String -> Lexer
lexerNew source' = Lexer {lineCount = 0, source = source'}

lexerNextLine :: Lexer -> [Token] -> Maybe (Lexer, (Int, [Token]))
lexerNextLine lexer' tokens = case lexerNext lexer' of
  Just (lexer'', Newline) -> case tokens of
    [] -> lexerNextLine lexer'' (tokens) -- empty line
    tokens' -> Just (lexer'', (lineCount lexer'', tokens'))
  Just (lexer'', token) -> lexerNextLine lexer'' (tokens ++ [token])
  Nothing | length tokens == 0 -> Nothing
  Nothing -> Just (lexer', (lineCount lexer', tokens))

lexerNext :: Lexer -> Maybe (Lexer, Token)
lexerNext Lexer {source = ""} = Nothing
lexerNext lexer@Lexer {source = (',' : s)} = Just (lexer {source = s}, Comma)
lexerNext lexer@Lexer {source = (':' : s)} = Just (lexer {source = s}, Colon)
lexerNext lexer@Lexer {source = ('+' : s)} = Just (lexer {source = s}, Plus)
lexerNext lexer@Lexer {source = ('\n' : s)} = Just (lexer {lineCount = lineCount lexer + 1, source = s}, Newline)
lexerNext lexer@Lexer {source = (c : s)} | isSpace c = lexerNext $ lexer {source = s}
lexerNext lexer@Lexer {source = (c : s)}
  | isAlpha c || c == '_' = Just (lexer {source = s'}, Ident ident)
  where
    (ident, s') = takeFor (\c' -> isAlphaNum c' || c' == '_') [c] s
lexerNext lexer@Lexer {source = (c : s)}
  | isNumber c || c == '-' = Just (lexer {source = s'}, Num num)
  where
    (numStr, s') = takeFor (\c' -> isNumber c') [c] s
    num = read numStr
lexerNext lexer@Lexer {source = (c : s)} = Just (lexer {source = s}, UnknownChar c)

takeFor :: (a -> Bool) -> [a] -> [a] -> ([a], [a])
takeFor _ xs [] = (xs, [])
takeFor f xs (y : zs) =
  if (f y)
    then takeFor f (xs ++ [y]) zs
    else (xs, y : zs)
