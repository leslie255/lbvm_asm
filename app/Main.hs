module Main where

import Parser
import System.IO
import Token

printTokenLines :: Lexer -> IO ()
printTokenLines lexer = case lexerNextLine lexer [] of
  Just (lexer', (line, tokens)) -> do
    putStrLn ("line" ++ show line ++ ":" ++ show tokens)
    printTokenLines lexer'
  Nothing -> putStrLn "EOF"

parseLines :: Lexer -> [Item] -> Either ([Item], ParseError) [Item]
parseLines lexer items = case lexerNextLine lexer [] of
  Just (lexer', (line, tokens)) -> case parseLine tokens line of
    Right item -> parseLines lexer' (items ++ [item])
    Left e -> Left (items, e)
  Nothing -> Right items

printParsed :: String -> IO ()
printParsed s = case parseLines (lexerNew s) [] of
  Left x -> print x
  Right x -> print x

main :: IO ()
main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  printParsed contents
  hClose handle
