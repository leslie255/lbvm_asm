module Assembler where

import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Word
import Inst.Inst (BigInst (imm), Inst, JumpInst (offset))
import qualified Inst.Inst as Inst
import Parser (Item (..), instOf, symbolOf)

pcInit :: Word64
pcInit = 32768

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
instSize (Inst.Small _) = 4
instSize (Inst.Jump _) = 4
instSize (Inst.Big _) = 8

appendInst :: Assembler -> Inst -> IO Assembler
appendInst assembler inst' =
  -- TODO: Reduce copying here by using mutable vectors
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
  let assembler' =
        if symbol == "start"
          then assembler {addrCounter = pcInit}
          else assembler
  let symbols' = Map.insert symbol (addrCounter assembler) (symbols assembler')
  return assembler' {symbols = symbols'}
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
          Inst.Small _ -> error "small inst with attached symbol"
          Inst.Jump jumpInst -> do
            let offset' = symbolLoc - (baseAddr unsolved')
            Inst.Jump $ jumpInst {offset = fromIntegral offset'}
          Inst.Big bigInst -> do
            Inst.Big $ bigInst {imm = symbolLoc}
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

-- GHC only supports x86_64, ARM and aarch64, so not much point checking platform endianness here.
word16ToLeBytes :: Word16 -> (Word8, Word8)
word16ToLeBytes x = (fromIntegral x, fromIntegral $ x .>>. 8)

word64ToLeBytes :: Word64 -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
word64ToLeBytes x =
  ( fromIntegral x,
    fromIntegral $ x .>>. 8,
    fromIntegral $ x .>>. 16,
    fromIntegral $ x .>>. 24,
    fromIntegral $ x .>>. 32,
    fromIntegral $ x .>>. 40,
    fromIntegral $ x .>>. 48,
    fromIntegral $ x .>>. 56
  )

instToCArrCode :: Inst -> String
instToCArrCode (Inst.Small inst') = show byte0 ++ "," ++ show byte1 ++ "," ++ show byte2 ++ "," ++ show byte3 ++ ","
  where
    byte0 = Inst.opcode inst'
    byte1 = (Inst.reg1 inst' .<<. 4) + Inst.reg0 inst'
    byte2 = (Inst.reg3 inst' .<<. 4) + Inst.reg2 inst'
    byte3 = Inst.flags inst'
instToCArrCode (Inst.Jump inst') = show byte0 ++ "," ++ show byte1 ++ "," ++ show byte2 ++ "," ++ show byte3 ++ ","
  where
    byte0 = Inst.opcode inst'
    (byte1, byte2) = word16ToLeBytes $ Inst.offset inst'
    byte3 = Inst.flags inst'
instToCArrCode (Inst.Big inst') =
  show
    byte0
    ++ ","
    ++ show byte1
    ++ ","
    ++ show byte2
    ++ ","
    ++ show byte3
    ++ ","
    ++ show byte4
    ++ ","
    ++ show byte5
    ++ ","
    ++ show byte6
    ++ ","
    ++ show byte7
    ++ ","
    ++ show byte8
    ++ ","
    ++ show byte9
    ++ ","
    ++ show byte10
    ++ ","
    ++ show byte11
    ++ ","
  where
    byte0 = Inst.opcode inst'
    byte1 = (Inst.reg1 inst' .<<. 4) + Inst.reg0 inst'
    byte2 = (Inst.reg3 inst' .<<. 4) + Inst.reg2 inst'
    byte3 = Inst.flags inst'
    (byte4, byte5, byte6, byte7, byte8, byte9, byte10, byte11) = word64ToLeBytes $ Inst.imm inst'

emitCArray :: Vector Inst -> String
emitCArray insts' = (emitCArrayInner insts' "{") ++ "}"

emitCArrayInner :: Vector Inst -> String -> String
emitCArrayInner insts' s = V.foldl (++) s $ V.map instToCArrCode insts'