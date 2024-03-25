module Main where

import Assembler
import Data.Vector (Vector)
import Inst.Inst (Inst)
import Parser
import System.Environment (getArgs)
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

assembleAll :: Lexer -> Assembler -> IO (Vector Inst)
assembleAll lexer assembler = case lexerNextLine lexer [] of
  Just (lexer', (line, tokens)) -> do
    let item = case parseLine tokens line of
          Right item' -> item'
          Left e -> error $ show e
    assembler' <- assembler `feedItem` item
    assembleAll lexer' assembler'
  Nothing -> do
    assembleResult <- finish assembler
    case assembleResult of
      Right insts' -> return insts'
      Left e -> error $ show e

main :: IO ()
main = do
  args <- getArgs
  if length args > 0
    then do
      handle <- openFile (args !! 0) ReadMode
      contents <- hGetContents handle
      assembledInsts <- assembleAll (lexerNew contents) assemblerNew
      print assembledInsts
      hClose handle
    else
      putStrLn "Expect at one argument"
