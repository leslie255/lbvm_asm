module Main where

import Assembler
import Data.Vector (Vector, forM_)
import Inst.Inst (Inst)
import Parser
import System.Environment (getArgs)
import System.IO
import Token

printTokenLines :: Lexer -> IO ()
printTokenLines lexer = case lexerNextLine lexer [] of
  Just (lexer', (line, Right tokens)) -> do
    putStrLn ("line" ++ show line ++ ":" ++ show tokens)
    printTokenLines lexer'
  Just (lexer', (line, Left e)) -> do
    putStrLn ("line" ++ show line ++ ":" ++ show e)
    printTokenLines lexer'
  Nothing -> putStrLn "EOF"

-- Errors on parser or lexer error
parseLines :: Lexer -> [Item] -> [Item]
parseLines lexer items = case lexerNextLine lexer [] of
  Just (lexer', (line, Right tokens)) -> case parseLine tokens line of
    Right item -> parseLines lexer' (items ++ [item])
    Left e -> error $ show e
  Just (_, (line, Left e)) -> error $ show e ++ "@" ++ show line
  Nothing -> items

-- Errors on parser or lexer error
assembleAll :: Assembler -> Lexer -> IO (Vector Inst)
assembleAll assembler lexer = case lexerNextLine lexer [] of
  Just (lexer', (line, Right tokens)) -> do
    let item = case parseLine tokens line of
          Right item' -> item'
          Left e -> error $ show e
    assembler' <- assembler `feedItem` item
    assembleAll assembler' lexer'
  Just (_', (line, Left e)) -> error $ show e ++ "@" ++ show line
  Nothing -> do
    assembleResult <- finish assembler
    case assembleResult of
      Right insts' -> return insts'
      Left e -> error $ show e

data Settings = Settings
  { file :: Maybe String,
    dbg :: Bool
  }
  deriving (Show)

settingsDefault :: Settings
settingsDefault =
  Settings
    { file = Nothing,
      dbg = False
    }

readArgs :: IO Settings
readArgs = do
  args <- getArgs
  return $ readArgsInner settingsDefault args

readArgsInner :: Settings -> [String] -> Settings
readArgsInner settings ("--dbg" : args) = readArgsInner settings {dbg = True} args
readArgsInner settings@Settings {file = Nothing} (file' : args) = readArgsInner settings {file = Just file'} args
readArgsInner settings [] = settings
readArgsInner Settings {file = Just _} _ = error "multiple source files are not supported"

main :: IO ()
main = do
  settings <- readArgs
  case file settings of
    Nothing -> putStrLn "Expect input file name"
    Just file' -> do
      handle <- openFile file' ReadMode
      contents <- hGetContents handle
      assembledInsts <- assembleAll assemblerNew $ lexerNew contents
      hClose handle
      if dbg settings
        then forM_ assembledInsts (\inst -> print inst)
        else return ()
      putStrLn $ emitCArray assembledInsts
