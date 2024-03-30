module Main where

import Assembler
import Data.List (isSuffixOf)
import Data.Vector (Vector, forM_)
import Inst.Inst (Inst)
import Parser
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.Exit
import System.IO
import Token

errorAndExit :: String -> IO a
errorAndExit s = do
  putStrLn s
  exitFailure

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
parseLines :: Lexer -> [Item] -> IO [Item]
parseLines lexer items = case lexerNextLine lexer [] of
  Just (lexer', (line, Right tokens)) -> case parseLine tokens line of
    Right item -> parseLines lexer' (items ++ [item])
    Left e -> errorAndExit $ show e
  Just (_, (line, Left e)) -> errorAndExit $ show e ++ "@" ++ show line
  Nothing -> return items

-- Errors on parser or lexer error
assembleAll :: Assembler -> Lexer -> IO (Vector Inst)
assembleAll assembler lexer = case lexerNextLine lexer [] of
  Just (lexer', (line, Right tokens)) ->
    case parseLine tokens line of
      Right item -> do
        assembler' <- assembler `feedItem` item
        assembleAll assembler' lexer'
      Left e -> errorAndExit $ show e
  Just (_', (line, Left e)) -> errorAndExit $ show e ++ "@" ++ show line
  Nothing -> do
    assembleResult <- finish assembler
    case assembleResult of
      Right insts' -> return insts'
      Left e -> errorAndExit $ show e

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
  readArgsInner settingsDefault args

mightBeSourceFile :: String -> Bool
mightBeSourceFile s = (".asm" `isSuffixOf` s) || (".s" `isSuffixOf` s)

readArgsInner :: Settings -> [String] -> IO Settings
readArgsInner settings ("--dbg" : args) = readArgsInner settings {dbg = True} args
readArgsInner settings@Settings {file = Nothing} (file' : args) = readArgsInner settings {file = Just file'} args
readArgsInner Settings {file = Just file'} (arg : _) -- guess where the error is
  | arg == "dbg" || file' == "dbg" || arg == "-dbg" || file' == "-dbg" = errorAndExit "did you mean `--dbg`?"
  | mightBeSourceFile arg && mightBeSourceFile file' = errorAndExit "multiple source files are not supported"
  | mightBeSourceFile arg = errorAndExit $ "invalid argument " ++ show file'
  | mightBeSourceFile file' = errorAndExit $ "invalid argument " ++ show arg
  | True = error $ "invalid argument" ++ arg
readArgsInner settings [] = return settings

validateInputFile :: String -> IO ()
validateInputFile path = do
  fileExists <- doesFileExist $ path
  isDir <- doesDirectoryExist $ path
  if fileExists
    then return ()
    else
      if isDir
        then errorAndExit $ "invalid input file (path " ++ show path ++ " exists, but is a directory)"
        else errorAndExit $ "invalid input file (path " ++ show path ++ " does not exist)"

main :: IO ()
main = do
  settings <- readArgs
  case file settings of
    Nothing -> putStrLn "expect input file"
    Just file' -> do
      validateInputFile file'
      handle <- openFile file' ReadMode
      contents <- hGetContents handle
      assembledInsts <- assembleAll assemblerNew $ lexerNew contents
      hClose handle
      if dbg settings
        then forM_ assembledInsts (\inst -> print inst)
        else return ()
      putStrLn $ emitCArray assembledInsts
