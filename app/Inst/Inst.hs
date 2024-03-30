module Inst.Inst where

import Data.Char (intToDigit)
import Data.Word
import qualified Inst.Opcode as Opcode
import qualified Inst.Reg as Reg
import Numeric (showIntAtBase)
import Text.Printf (printf)

data Inst = Small SmallInst | Jump JumpInst | Big BigInst

class AnyInst a where
  opcode :: a -> Word8
  flags :: a -> Word8

class (AnyInst a) => InstWithReg a where
  reg0 :: a -> Word8
  reg1 :: a -> Word8
  reg2 :: a -> Word8
  reg3 :: a -> Word8

data SmallInst = SmallInst
  { opcode' :: Word8,
    reg0' :: Word8,
    reg1' :: Word8,
    reg2' :: Word8,
    reg3' :: Word8,
    flags' :: Word8
  }

data JumpInst = JumpInst
  { opcode'' :: Word8,
    offset :: Word16,
    flags'' :: Word8
  }

data BigInst = BigInst
  { opcode''' :: Word8,
    reg0'' :: Word8,
    reg1'' :: Word8,
    reg2'' :: Word8,
    reg3'' :: Word8,
    flags''' :: Word8,
    imm :: Word64
  }

makeSmallInst :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> SmallInst
makeSmallInst opcode'''' reg0''' reg1''' reg2''' reg3''' flags'''' =
  SmallInst
    { opcode' = opcode'''',
      reg0' = reg0''',
      reg1' = reg1''',
      reg2' = reg2''',
      reg3' = reg3''',
      flags' = flags''''
    }

makeJumpInst :: Word8 -> Word16 -> Word8 -> JumpInst
makeJumpInst opcode'''' offset' flags'''' =
  JumpInst
    { opcode'' = opcode'''',
      offset = offset',
      flags'' = flags''''
    }

makeBigInst :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word64 -> BigInst
makeBigInst opcode'''' reg0''' reg1''' reg2''' reg3''' flags'''' imm' =
  BigInst
    { opcode''' = opcode'''',
      reg0'' = reg0''',
      reg1'' = reg1''',
      reg2'' = reg2''',
      reg3'' = reg3''',
      flags''' = flags'''',
      imm = imm'
    }

instance AnyInst SmallInst where
  opcode inst = opcode' inst
  flags inst = flags' inst

instance InstWithReg SmallInst where
  reg0 a = reg0' a
  reg1 a = reg1' a
  reg2 a = reg2' a
  reg3 a = reg3' a

instance AnyInst JumpInst where
  opcode inst = opcode'' inst
  flags inst = flags'' inst

instance AnyInst BigInst where
  opcode inst = opcode''' inst
  flags inst = flags''' inst

instance InstWithReg BigInst where
  reg0 a = reg0'' a
  reg1 a = reg1'' a
  reg2 a = reg2'' a
  reg3 a = reg3'' a

instance Show SmallInst where
  show inst =
    "("
      ++ Opcode.showOpcode (opcode inst)
      ++ " "
      ++ Reg.showReg (reg0 inst)
      ++ ","
      ++ Reg.showReg (reg1 inst)
      ++ ","
      ++ Reg.showReg (reg2 inst)
      ++ ","
      ++ Reg.showReg (reg3 inst)
      ++ ",0b"
      ++ showIntAtBase 2 intToDigit (flags inst) ""
      ++ ")"

instance Show JumpInst where
  show inst =
    "("
      ++ Opcode.showOpcode (opcode inst)
      ++ printf " 0x%04X" (offset inst)
      ++ ",0b"
      ++ showIntAtBase 2 intToDigit (flags inst) ""
      ++ ")"

instance Show BigInst where
  show inst =
    "("
      ++ Opcode.showOpcode (opcode inst)
      ++ " "
      ++ Reg.showReg (reg0 inst)
      ++ ","
      ++ Reg.showReg (reg1 inst)
      ++ ","
      ++ Reg.showReg (reg2 inst)
      ++ ","
      ++ Reg.showReg (reg3 inst)
      ++ ",0b"
      ++ showIntAtBase 2 intToDigit (flags inst) ""
      ++ printf ",0x%016X" (imm inst)
      ++ ")"

instance Show Inst where
  show (Small inst) = show inst
  show (Jump inst) = show inst
  show (Big inst) = show inst