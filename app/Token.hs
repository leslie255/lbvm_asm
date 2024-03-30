module Token where

import Data.Char (isAlpha, isAlphaNum, isNumber, isSpace)
import Data.Word

data LexerError = InvalidNumberLiteral | InvalidCharLiteral
  deriving (Show)

data Token = UnknownChar Char | Ident String | Num Word64 | Comma | Colon | Plus | Newline
  deriving (Show)

data Lexer = Lexer
  { lineCount :: Int,
    source :: String
  }

lexerNew :: String -> Lexer
lexerNew source' = Lexer {lineCount = 0, source = source'}

lexerNextLine :: Lexer -> [Token] -> Maybe (Lexer, (Int, Either LexerError [Token]))
lexerNextLine lexer' tokens = case lexerNext lexer' of
  Just (lexer'', Right Newline) -> case tokens of
    [] -> lexerNextLine lexer'' (tokens) -- empty line
    tokens' -> Just (lexer'', (lineCount lexer'', Right tokens'))
  Just (lexer'', Right token) -> lexerNextLine lexer'' (tokens ++ [token])
  Just (lexer'', Left e) -> Just (lexer'', (lineCount lexer'', Left e))
  Nothing -> case tokens of
    [] -> Nothing
    tokens' -> Just (lexer', (lineCount lexer', Right tokens'))

lexerNext :: Lexer -> Maybe (Lexer, Either LexerError Token)
lexerNext Lexer {source = ""} = Nothing
lexerNext lexer@Lexer {source = (',' : s)} = Just (lexer {source = s}, Right Comma)
lexerNext lexer@Lexer {source = (':' : s)} = Just (lexer {source = s}, Right Colon)
lexerNext lexer@Lexer {source = ('+' : s)} = Just (lexer {source = s}, Right Plus)
lexerNext lexer@Lexer {source = (';' : s)} = skipComment lexer {source = s}
lexerNext lexer@Lexer {source = ('\n' : s)} = Just (lexer {lineCount = lineCount lexer + 1, source = s}, Right Newline)
lexerNext lexer@Lexer {source = ('\'' : '\\' : '\\' : '\'' : s)} = Just (lexer {source = s}, Right $ Num $ fromIntegral $ fromEnum '\\')
lexerNext lexer@Lexer {source = ('\'' : '\\' : '\'' : '\'' : s)} = Just (lexer {source = s}, Right $ Num $ fromIntegral $ fromEnum '\'')
lexerNext lexer@Lexer {source = ('\'' : '\\' : '\"' : '\'' : s)} = Just (lexer {source = s}, Right $ Num $ fromIntegral $ fromEnum '\"')
lexerNext lexer@Lexer {source = ('\'' : '\\' : '\n' : '\'' : s)} = Just (lexer {source = s}, Right $ Num $ fromIntegral $ fromEnum '\n')
lexerNext lexer@Lexer {source = ('\'' : '\\' : '\t' : '\'' : s)} = Just (lexer {source = s}, Right $ Num $ fromIntegral $ fromEnum '\t')
lexerNext lexer@Lexer {source = ('\'' : c : '\'' : s)} = Just (lexer {source = s}, Right $ Num $ fromIntegral $ fromEnum c)
lexerNext lexer@Lexer {source = (c : s)}
  | isSpace c = lexerNext $ lexer {source = s}
  | isAlpha c || c == '_' =
      let (ident, s') = takeFor (\c' -> isAlphaNum c' || c' == '_') [c] s
       in Just (lexer {source = s'}, Right $ Ident ident)
  | isNumber c || c == '-' =
      let (numStr, s') = takeFor (\c' -> isAlphaNum c') [c] s
       in case parseNumber numStr of
            Right num -> Just (lexer {source = s'}, Right $ Num num)
            Left e -> Just (lexer {source = s'}, Left e)
lexerNext lexer@Lexer {source = (c : s)} = Just (lexer {source = s}, Right $ UnknownChar c)

parseNumber :: String -> Either LexerError Word64
parseNumber ('0' : 'x' : s) = parseHexNumber s 0
parseNumber ('0' : 'd' : s) | all isNumber s = Right $ read s
parseNumber ('0' : 'o' : s) = parseOctNumber s 0
parseNumber ('0' : 'b' : s) = parseBinNumber s 0
parseNumber s | all isNumber s = Right $ read s
parseNumber _ = Left InvalidNumberLiteral

parseHexNumber :: String -> Word64 -> Either LexerError Word64
parseHexNumber (c : s) i
  | c == '0' = parseHexNumber s $ i * 0x10 + 0x0
  | c == '1' = parseHexNumber s $ i * 0x10 + 0x1
  | c == '2' = parseHexNumber s $ i * 0x10 + 0x2
  | c == '3' = parseHexNumber s $ i * 0x10 + 0x3
  | c == '4' = parseHexNumber s $ i * 0x10 + 0x4
  | c == '5' = parseHexNumber s $ i * 0x10 + 0x5
  | c == '6' = parseHexNumber s $ i * 0x10 + 0x6
  | c == '7' = parseHexNumber s $ i * 0x10 + 0x7
  | c == '8' = parseHexNumber s $ i * 0x10 + 0x8
  | c == '9' = parseHexNumber s $ i * 0x10 + 0x9
  | c == 'A' || c == 'a' = parseHexNumber s $ i * 0x10 + 0xA
  | c == 'B' || c == 'b' = parseHexNumber s $ i * 0x10 + 0xB
  | c == 'C' || c == 'c' = parseHexNumber s $ i * 0x10 + 0xC
  | c == 'D' || c == 'd' = parseHexNumber s $ i * 0x10 + 0xD
  | c == 'E' || c == 'e' = parseHexNumber s $ i * 0x10 + 0xE
  | c == 'F' || c == 'f' = parseHexNumber s $ i * 0x10 + 0xF
  | True = Left InvalidNumberLiteral
parseHexNumber "" i = Right i

parseOctNumber :: String -> Word64 -> Either LexerError Word64
parseOctNumber (c : s) i
  | c == '0' = parseOctNumber s $ i * 8 + 0
  | c == '1' = parseOctNumber s $ i * 8 + 1
  | c == '2' = parseOctNumber s $ i * 8 + 2
  | c == '3' = parseOctNumber s $ i * 8 + 3
  | c == '4' = parseOctNumber s $ i * 8 + 4
  | c == '5' = parseOctNumber s $ i * 8 + 5
  | c == '6' = parseOctNumber s $ i * 8 + 6
  | c == '7' = parseOctNumber s $ i * 8 + 7
  | True = Left InvalidNumberLiteral
parseOctNumber "" i = Right i

parseBinNumber :: String -> Word64 -> Either LexerError Word64
parseBinNumber (c : s) i
  | c == '0' = parseBinNumber s $ i * 2 + 0
  | c == '1' = parseBinNumber s $ i * 2 + 1
  | True = Left $ InvalidNumberLiteral
parseBinNumber "" i = Right i

skipComment :: Lexer -> Maybe (Lexer, Either LexerError Token)
skipComment lexer@Lexer {source = ('\n' : s)} = Just (lexer {source = s}, Right Newline)
skipComment lexer@Lexer {source = (_ : s)} = skipComment lexer {source = s}
skipComment Lexer {source = []} = Nothing

takeFor :: (a -> Bool) -> [a] -> [a] -> ([a], [a])
takeFor _ xs [] = (xs, [])
takeFor f xs (y : zs) =
  if (f y)
    then takeFor f (xs ++ [y]) zs
    else (xs, y : zs)
