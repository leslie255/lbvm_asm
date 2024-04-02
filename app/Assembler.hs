module Assembler where

-- import qualified Data.Vector.Unboxed.Mutable as MV

import Common
import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word
import Inst.Inst (Inst)
import qualified Inst.Inst as Inst
import Parser (Item (..), Segment (..), SymboledInst (..))
import Text.Printf (printf)

pcInit :: Word64
pcInit = 32768

data AssembleError = UndefinedSymbol String
  deriving (Show)

data UnsolvedSymbol = UnsolvedSymbol
  { name :: String,
    position :: Int,
    refAddr :: Maybe Word64
  }
  deriving (Show)

-- TODO: Use Unboxed Vector for `insts`.
data Assembler = Assembler
  { bytesText :: Vector Word8, -- Use Vectors here for efficient indexing later when solving symbols
    bytesData :: Vector Word8,
    addrCounter :: Word64,
    unsolved :: [UnsolvedSymbol],
    symbols :: Map String Word64
  }
  deriving (Show)

data AssembledProgram = AssembledProgram
  { dataSegment :: Vector Word8,
    textSegment :: Vector Word8
  }
  deriving (Show)

data EmitMode = ArrDec | ArrHex | Str
  deriving (Show, Eq)

currentSegment :: Assembler -> Segment
currentSegment Assembler {addrCounter = addrCounter'}
  | 0x10000 <= addrCounter' && addrCounter' < 0x20000 = Text
  | 0x20000 <= addrCounter' && addrCounter' < 0x30000 = Data
  | True = error "Invalid addrCounter"

assemblerNew :: Assembler
assemblerNew =
  Assembler
    { bytesText = V.empty,
      bytesData = V.empty,
      addrCounter = 0x10000,
      unsolved = [],
      symbols = Map.fromList []
    }

instSize :: Inst -> Word64
instSize (Inst.Small _) = 4
instSize (Inst.Jump _) = 4
instSize (Inst.Big _) = 8

appendBytes :: Assembler -> [Word8] -> IO Assembler
appendBytes assembler bytes
  | currentSegment assembler == Text = do
      bytesText' <- newBytes $ bytesText assembler
      return assembler {bytesText = bytesText', addrCounter = newAddr}
  | currentSegment assembler == Data = do
      bytesData' <- newBytes $ bytesData assembler
      return assembler {bytesData = bytesData', addrCounter = newAddr}
  | True = error "unreachable"
  where
    newBytes v = v `snocList` bytes
    newAddr = addrCounter assembler + fromIntegral (length bytes)

appendInst :: Assembler -> Inst -> IO Assembler
appendInst assembler inst' = assembler `appendBytes` (instToBytes inst')

-- TODO: Reduce copying here by using mutable vectors
snocList :: (V.Unbox a) => Vector a -> [a] -> IO (Vector a)
snocList xs v = return $ foldl V.snoc xs v

instToBytes :: Inst -> [Word8]
instToBytes inst'
  | Inst.Big biginst <- inst' =
      let Inst.BigInst opcode reg0 reg1 reg2 reg3 flags imm = biginst
       in [opcode, (reg0 .|. (reg1 .<<. 4)), (reg2 .|. (reg3 .<<. 4)), flags] ++ word64ToLeBytesList imm
  | Inst.Jump jumpinst <- inst' =
      let Inst.JumpInst opcode offset flags = jumpinst
       in opcode : word16ToLeBytesList offset ++ [flags]
  | Inst.Small smallinst <- inst' =
      let Inst.SmallInst opcode reg0 reg1 reg2 reg3 flags = smallinst
       in [opcode, (reg0 .|. (reg1 .<<. 4)), (reg2 .|. (reg3 .<<. 4)), flags]

feedItem :: Assembler -> Item -> IO Assembler
feedItem assembler (Inst inst') = assembler `appendInst` inst'
feedItem assembler (WithSymbol (SymboledBig (biginst, symbol))) = do
  let unsolved' =
        UnsolvedSymbol
          { name = symbol,
            position = fromIntegral $ addrCounter assembler + 4,
            refAddr = Nothing
          }
  let assembler' = assembler {unsolved = unsolved' : unsolved assembler}
  assembler' `appendInst` Inst.Big biginst
feedItem assembler (WithSymbol (SymboledJump (jumpinst, symbol))) = do
  let unsolved' =
        UnsolvedSymbol
          { name = symbol,
            position = fromIntegral $ addrCounter assembler + 1,
            refAddr = Just $ addrCounter assembler + 4
          }
  let assembler' = assembler {unsolved = unsolved' : unsolved assembler}
  assembler' `appendInst` Inst.Jump jumpinst
feedItem assembler (Label symbol) =
  let symbols' = Map.insert symbol (addrCounter assembler) (symbols assembler)
   in return assembler {symbols = symbols'}
feedItem assembler (Segment segment) =
  let addr = case segment of
        Text -> 0x10000
        Data -> 0x20000
   in return assembler {addrCounter = addr}
feedItem assembler (Bytes bytes) = assembler `appendBytes` bytes

solveSymbol :: Assembler -> UnsolvedSymbol -> IO (Either AssembleError Assembler)
solveSymbol assembler unsolved' = case Map.lookup (name unsolved') (symbols assembler) of
  Just symbolLoc -> case refAddr unsolved' of
    Just refAddr' -> do
      let offset = fromIntegral $ symbolLoc - refAddr' :: Word16
      let (offset0, offset1) = word16ToLeBytes offset
      let (mv_, idx) = case position unsolved' of
            pos | 0x10000 <= pos && pos + 1 < 0x20000 -> (V.thaw $ bytesText assembler, pos - 0x10000)
            pos | 0x20000 <= pos && pos + 1 < 0x30000 -> (V.thaw $ bytesData assembler, pos - 0x20000)
            _ -> error "addrCounter overflowed"
      mv <- mv_
      MV.write mv (idx) offset0
      MV.write mv (idx + 1) offset1
      v <- V.freeze mv
      let assembler' = case position unsolved' of
            pos | 0x10000 <= pos && pos + 1 < 0x20000 -> assembler {bytesText = v}
            pos | 0x20000 <= pos && pos + 1 < 0x30000 -> assembler {bytesData = v}
            _ -> error "unreachable"
      return $ Right assembler'
    Nothing -> do
      let (offset0, offset1, offset2, offset3, offset4, offset5, offset6, offset7) = word64ToLeBytes symbolLoc
      let (mv_, idx) = case position unsolved' of
            pos | 0x10000 <= pos && pos + 1 < 0x20000 -> (V.thaw $ bytesText assembler, pos - 0x10000)
            pos | 0x20000 <= pos && pos + 1 < 0x30000 -> (V.thaw $ bytesData assembler, pos - 0x20000)
            _ -> error "addrCounter overflowed"
      mv <- mv_
      MV.write mv (idx) offset0
      MV.write mv (idx + 1) offset1
      MV.write mv (idx + 2) offset2
      MV.write mv (idx + 3) offset3
      MV.write mv (idx + 4) offset4
      MV.write mv (idx + 5) offset5
      MV.write mv (idx + 6) offset6
      MV.write mv (idx + 7) offset7
      v <- V.freeze mv
      let assembler' = case position unsolved' of
            pos | 0x10000 <= pos && pos + 7 < 0x20000 -> assembler {bytesText = v}
            pos | 0x20000 <= pos && pos + 7 < 0x30000 -> assembler {bytesData = v}
            _ -> error "unreachable"
      return $ Right assembler'
  Nothing -> return $ Left $ UndefinedSymbol (name unsolved')

solveAllSymbols :: Assembler -> IO (Either AssembleError Assembler)
solveAllSymbols assembler@Assembler {unsolved = []} = return $ Right assembler
solveAllSymbols assembler@Assembler {unsolved = (x : ys)} = do
  solveResult <- solveSymbol assembler x
  case solveResult of
    Right assembler' -> solveAllSymbols assembler' {unsolved = ys}
    Left e -> return $ Left e

finish :: Assembler -> IO (Either AssembleError AssembledProgram)
finish assembler = do
  solveResult <- solveAllSymbols assembler
  return $ fmap (\assembler' -> AssembledProgram (bytesData assembler') (bytesText assembler')) solveResult

emitAsStr :: AssembledProgram -> String
emitAsStr assembled =
  (emitAsStrInner (textSegment assembled) "static const u8 text_segment[] = \"")
    ++ "\";\n"
    ++ (emitAsStrInner (dataSegment assembled) "static const u8 data_segment[] = \"")
    ++ "\";\n"
    ++ "u8 vmem_text[VMEM_SEG_SIZE] = {0};\n"
    ++ "u8 vmem_data[VMEM_SEG_SIZE] = {0};\n"
    ++ "u8 vmem_stack[VMEM_SEG_SIZE] = {0};\n"
    ++ "memcpy(vmem_text, text_segment, sizeof(text_segment));\n"
    ++ "memcpy(vmem_data, data_segment, sizeof(data_segment));\n"
  where
    emitAsStrInner :: V.Vector Word8 -> String -> String
    emitAsStrInner bytes' s = foldl (++) s (map emitByteAsChar (V.toList bytes'))
    emitByteAsChar :: Word8 -> String
    emitByteAsChar 0 = "\\0"
    emitByteAsChar 7 = "\\a"
    emitByteAsChar 8 = "\\b"
    emitByteAsChar 9 = "\\t"
    emitByteAsChar 10 = "\\n"
    emitByteAsChar 11 = "\\v"
    emitByteAsChar 12 = "\\f"
    emitByteAsChar 13 = "\\r"
    emitByteAsChar 34 = "\\\""
    emitByteAsChar 92 = "\\\""
    emitByteAsChar x | x >= 32 && x <= 126 && not (x >= 48 && x <= 57) && not (x >= 64 && x <= 70) && not (x >= 97 && x <= 102) = (printf "%c" x)
    emitByteAsChar x = (printf "\\x%02X" x)

emitAsDecArr :: AssembledProgram -> String
emitAsDecArr assembled =
  (emitAsDecArrInner (textSegment assembled) "static const u8 text_segment[] = {")
    ++ "};\n"
    ++ (emitAsDecArrInner (dataSegment assembled) "static const u8 data_segment[] = {")
    ++ "};\n"
    ++ "u8 vmem_text[VMEM_SEG_SIZE] = {0};\n"
    ++ "u8 vmem_data[VMEM_SEG_SIZE] = {0};\n"
    ++ "u8 vmem_stack[VMEM_SEG_SIZE] = {0};\n"
    ++ "memcpy(vmem_text, text_segment, sizeof(text_segment));\n"
    ++ "memcpy(vmem_data, data_segment, sizeof(data_segment));\n"
  where
    emitAsDecArrInner :: V.Vector Word8 -> String -> String
    emitAsDecArrInner bytes' s = foldl (++) s (map (\x -> show x ++ ",") (V.toList bytes'))

emitAsHexArr :: AssembledProgram -> String
emitAsHexArr assembled =
  (emitAsHexArrInner (textSegment assembled) "static const u8 text_segment[] = {")
    ++ "};\n"
    ++ (emitAsHexArrInner (dataSegment assembled) "static const u8 data_segment[] = {")
    ++ "};\n"
    ++ "u8 vmem_text[VMEM_SEG_SIZE] = {0};\n"
    ++ "u8 vmem_data[VMEM_SEG_SIZE] = {0};\n"
    ++ "u8 vmem_stack[VMEM_SEG_SIZE] = {0};\n"
    ++ "memcpy(vmem_text, text_segment, sizeof(text_segment));\n"
    ++ "memcpy(vmem_data, data_segment, sizeof(data_segment));\n"
  where
    emitAsHexArrInner :: V.Vector Word8 -> String -> String
    emitAsHexArrInner bytes' s = foldl (++) s (map (\x -> (printf "0x%02X," x)) (V.toList bytes'))

emit :: AssembledProgram -> EmitMode -> String
emit assembled ArrDec = emitAsDecArr assembled
emit assembled ArrHex = emitAsHexArr assembled
emit assembled Str = emitAsStr assembled