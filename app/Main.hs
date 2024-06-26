module Main where

import Assembler
import Data.List (isSuffixOf)
import Parser
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.Exit
import System.IO
import Token hiding (Str)

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
assembleAll :: Assembler -> Lexer -> IO AssembledProgram
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
      Right assembled' -> return assembled'
      Left e -> errorAndExit $ show e

data Settings = Settings
  { file :: Maybe String,
    dbg :: Bool,
    emitmode :: EmitMode
  }
  deriving (Show)

settingsDefault :: Settings
settingsDefault =
  Settings
    { file = Nothing,
      dbg = False,
      emitmode = ArrDec
    }

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: lbvm_asm [OPTIONS] [SOURCE PATH]"
  putStrLn "Options:"
  putStrLn "  --help             show this message"
  putStrLn "  --emit [EMIT MODE] emit mode (arrdec, arrhex, str)"
  return ()

readArgs :: IO Settings
readArgs = do
  args <- getArgs
  readArgsInner settingsDefault args
  where
    readArgsInner :: Settings -> [String] -> IO Settings
    readArgsInner _ ("--help" : _) = printHelp >>= return exitSuccess
    readArgsInner settings ("--dbg" : args) = readArgsInner settings {dbg = True} args
    readArgsInner settings ("--emit" : emitmode' : args) = do
      emitmode'' <- readEmitMode emitmode'
      readArgsInner settings {emitmode = emitmode''} args
    readArgsInner _ ["--emit"] = do
      errorAndExit $ "invalid options: expect emit mode (arrdec, arrhex, str) after `--emit`"
    readArgsInner settings@Settings {file = Nothing} (file' : args) = readArgsInner settings {file = Just file'} args
    readArgsInner Settings {file = Just file'} (arg : _) -- guess where the error is
      | mightBeSourceFile arg && mightBeSourceFile file' = errorAndExit "multiple source files are not supported"
      | mightBeSourceFile arg = errorAndExit $ "invalid argument " ++ show file' ++ " (multiple source files are not supported)"
      | mightBeSourceFile file' = errorAndExit $ "invalid argument " ++ show arg ++ " (multiple source files are not supported)"
      | True = error $ "invalid argument " ++ show arg ++ " (multiple source files are not supported)"
    readArgsInner settings [] = return settings

readEmitMode :: String -> IO EmitMode
readEmitMode s
  | "arrdec" <- s = return ArrDec
  | "arrhex" <- s = return ArrHex
  | "str" <- s = return Str
  | _ <- s = errorAndExit $ "invalid emit mode: " ++ show s ++ " (arrdec, arrhex, str)"

mightBeSourceFile :: String -> Bool
mightBeSourceFile s = (".asm" `isSuffixOf` s) || (".s" `isSuffixOf` s)

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
      assembled <- assembleAll assemblerNew $ lexerNew contents
      hClose handle
      putStrLn $ emit assembled (emitmode settings)
