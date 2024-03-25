module Assembler where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Word
import Inst.Inst (BigInst (imm), Inst (..), JumpInst (offset))
import Parser (Item (..), instOf, symbolOf)

data AssembleError = UndefinedSymbol String
  deriving (Show)

data UnsolvedSymbol = UnsolvedSymbol
  { name :: String,
    instIdx :: Int,
    baseAddr :: Word64
  }
  deriving (Show)

-- TODO: Use Unboxed Vector for `insts`.
data Assembler = Assembler
  { insts :: Vector Inst, -- Use Vectors here for efficient indexing later when solving symbols
    addrCounter :: Word64,
    unsolved :: [UnsolvedSymbol],
    symbols :: Map String Word64
  }
  deriving (Show)

assemblerNew :: Assembler
assemblerNew =
  Assembler
    { insts = V.empty,
      addrCounter = 0,
      unsolved = [],
      symbols = Map.fromList []
    }

instSize :: Inst -> Word64
instSize (Small _) = 4
instSize (Jump _) = 4
instSize (Big _) = 8

appendInst :: Assembler -> Inst -> IO Assembler
appendInst assembler inst' =  -- TODO: Reduce copying here by using mutable vectors
  return
    assembler
      { insts = insts',
        addrCounter = addrCounter assembler + instSize inst'
      }
  where
    insts' = (insts assembler) `V.snoc` inst'

feedItem :: Assembler -> Item -> IO Assembler
feedItem assembler (Inst inst') = assembler `appendInst` inst'
feedItem assembler (Label symbol) = do
  let symbols' = Map.insert symbol (addrCounter assembler) (symbols assembler)
  return assembler {symbols = symbols'}
feedItem assembler (WithSymbol symboled') = do
  let instIdx' = length $ insts assembler
  assembler' <- appendInst assembler $ instOf symboled'
  let unsolvedSymbol = UnsolvedSymbol (symbolOf symboled') instIdx' (addrCounter assembler')
  return
    assembler'
      { symbols = symbols assembler',
        unsolved = unsolvedSymbol : unsolved assembler'
      }

solveSymbol :: Assembler -> UnsolvedSymbol -> IO (Either AssembleError Assembler)
solveSymbol assembler unsolved' = case Map.lookup (name unsolved') (symbols assembler) of
  Just symbolLoc -> do
    let instIdx' = instIdx unsolved'
    let inst' = case (insts assembler V.! instIdx') of
          Small _ -> error "small inst with attached symbol"
          Jump jumpInst -> do
            let offset' = symbolLoc - (baseAddr unsolved')
            Jump $ jumpInst {offset = fromIntegral offset'}
          Big bigInst -> do
            Big $ bigInst {imm = symbolLoc}
    mv <- V.thaw $ insts assembler
    MV.write mv instIdx' inst'
    insts' <- V.freeze mv
    return $ Right assembler {insts = insts'}
  Nothing -> return $ Left $ UndefinedSymbol (name unsolved')

solveAllSymbols :: Assembler -> IO (Either AssembleError Assembler)
solveAllSymbols assembler = case (unsolved assembler) of
  [] -> return $ Right assembler
  (x : ys) -> do
    solveResult <- solveSymbol assembler x
    case solveResult of
      Right assembler' -> do
        let assembler'' = assembler' {unsolved = ys}
        solveAllSymbols assembler''
      Left e -> return $ Left e

finish :: Assembler -> IO (Either AssembleError (Vector Inst))
finish assembler = do
  solveResult <- solveAllSymbols assembler
  return $ fmap (\assembler' -> insts assembler') solveResult