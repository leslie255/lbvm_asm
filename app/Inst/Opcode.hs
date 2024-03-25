{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE BinaryLiterals #-}
module Inst.Opcode where

import Data.Word
import Data.Bits hiding (xor)
import Prelude hiding (not, or, and, mod, div)
import Text.Printf (printf)

makeOpcode :: Word8 -> Word8
makeOpcode x = (x .<<. 2) .&. 0b11111100

condflagN     = 0b00000001 :: Word8
condflagZ     = 0b00000010 :: Word8
condflagC     = 0b00000100 :: Word8
condflagV     = 0b00001000 :: Word8
condflagE     = 0b00010000 :: Word8
condflagG     = 0b00100000 :: Word8
condflagL     = 0b01000000 :: Word8
condflagGE    = (condflagG  .|. condflagE)
condflagLE    = (condflagL  .|. condflagE)
condflagNN    = (condflagN  .|. 0b10000000)
condflagNZ    = (condflagZ  .|. 0b10000000)
condflagNC    = (condflagC  .|. 0b10000000)
condflagNV    = (condflagV  .|. 0b10000000)
condflagNE    = (condflagE  .|. 0b10000000)
condflagNG    = (condflagG  .|. 0b10000000)
condflagNL    = (condflagL  .|. 0b10000000)
condflagNGE   = (condflagGE .|. 0b10000000)
condflagNLE   = (condflagLE .|. 0b10000000)

qword = 0b00 :: Word8
dword = 0b01 :: Word8
word  = 0b10 :: Word8
byte  = 0b11 :: Word8

brk         = makeOpcode(0)   -- interrupt
cbrk        = makeOpcode(1)
nop         = makeOpcode(2)   -- nop
load_imm    = makeOpcode(3)   -- memory
load_dir    = makeOpcode(4)
load_ind    = makeOpcode(5)
store_imm   = makeOpcode(6)
store_dir   = makeOpcode(7)
store_ind   = makeOpcode(8)
mov         = makeOpcode(9)
cmp         = makeOpcode(10)  -- branching
fcmp        = makeOpcode(11)
csel        = makeOpcode(12)
b           = makeOpcode(13)
j           = makeOpcode(14)
add         = makeOpcode(15)  -- arithmetics
sub         = makeOpcode(16)
mul         = makeOpcode(17)
div         = makeOpcode(18)
mod         = makeOpcode(19)
iadd        = makeOpcode(20)
isub        = makeOpcode(21)
imul        = makeOpcode(22)
idiv        = makeOpcode(23)
imod        = makeOpcode(24)
fadd        = makeOpcode(25)
fsub        = makeOpcode(26)
fmul        = makeOpcode(27)
fdiv        = makeOpcode(28)
fmod        = makeOpcode(29)
ineg        = makeOpcode(30)
fneg        = makeOpcode(31)
shl         = makeOpcode(32)
shr         = makeOpcode(33)
and         = makeOpcode(34)
or          = makeOpcode(35)
xor         = makeOpcode(36)
not         = makeOpcode(37)
muladd      = makeOpcode(38)
call        = makeOpcode(39)
ccall       = makeOpcode(40)
ret         = makeOpcode(41)
push        = makeOpcode(42)
pop         = makeOpcode(43)
libc_call   = makeOpcode(44)
native_call = makeOpcode(45)
breakpoint  = 0b11111100

showOpcode :: Word8 -> String
showOpcode opcode = let oplen = opcode .&. 0b00000011 in
   case opcode .&. 0b11111100 of
      x | x == brk         -> "brk "         ++ showOplen(oplen)
      x | x == cbrk        -> "cbrk "        ++ showOplen(oplen)
      x | x == nop         -> "nop "         ++ showOplen(oplen)
      x | x == load_imm    -> "load_imm "    ++ showOplen(oplen)
      x | x == load_dir    -> "load_dir "    ++ showOplen(oplen)
      x | x == load_ind    -> "load_ind "    ++ showOplen(oplen)
      x | x == store_imm   -> "store_imm "   ++ showOplen(oplen)
      x | x == store_dir   -> "store_dir "   ++ showOplen(oplen)
      x | x == store_ind   -> "store_ind "   ++ showOplen(oplen)
      x | x == mov         -> "mov "         ++ showOplen(oplen)
      x | x == cmp         -> "cmp "         ++ showOplen(oplen)
      x | x == fcmp        -> "fcmp "        ++ showOplen(oplen)
      x | x == csel        -> "csel "        ++ showOplen(oplen)
      x | x == b           -> "b "           ++ showOplen(oplen)
      x | x == j           -> "j "           ++ showOplen(oplen)
      x | x == add         -> "add "         ++ showOplen(oplen)
      x | x == sub         -> "sub "         ++ showOplen(oplen)
      x | x == mul         -> "mul "         ++ showOplen(oplen)
      x | x == div         -> "div "         ++ showOplen(oplen)
      x | x == mod         -> "mod "         ++ showOplen(oplen)
      x | x == iadd        -> "iadd "        ++ showOplen(oplen)
      x | x == isub        -> "isub "        ++ showOplen(oplen)
      x | x == imul        -> "imul "        ++ showOplen(oplen)
      x | x == idiv        -> "idiv "        ++ showOplen(oplen)
      x | x == imod        -> "imod "        ++ showOplen(oplen)
      x | x == fadd        -> "fadd "        ++ showOplen(oplen)
      x | x == fsub        -> "fsub "        ++ showOplen(oplen)
      x | x == fmul        -> "fmul "        ++ showOplen(oplen)
      x | x == fdiv        -> "fdiv "        ++ showOplen(oplen)
      x | x == fmod        -> "fmod "        ++ showOplen(oplen)
      x | x == ineg        -> "ineg "        ++ showOplen(oplen)
      x | x == fneg        -> "fneg "        ++ showOplen(oplen)
      x | x == shl         -> "shl "         ++ showOplen(oplen)
      x | x == shr         -> "shr "         ++ showOplen(oplen)
      x | x == and         -> "and "         ++ showOplen(oplen)
      x | x == or          -> "or "          ++ showOplen(oplen)
      x | x == xor         -> "xor "         ++ showOplen(oplen)
      x | x == not         -> "not "         ++ showOplen(oplen)
      x | x == muladd      -> "muladd "      ++ showOplen(oplen)
      x | x == call        -> "call "        ++ showOplen(oplen)
      x | x == ccall       -> "ccall "       ++ showOplen(oplen)
      x | x == ret         -> "ret "         ++ showOplen(oplen)
      x | x == push        -> "push "        ++ showOplen(oplen)
      x | x == pop         -> "pop "         ++ showOplen(oplen)
      x | x == libc_call   -> "libc_call "   ++ showOplen(oplen)
      x | x == native_call -> "native_call " ++ showOplen(oplen)
      x | x == breakpoint  -> "breakpoint "  ++ showOplen(oplen)
      x -> (printf "(unknown 0x%02X)" x)

showOplen :: Word8 -> String
showOplen 0 = "qword"
showOplen 1 = "dword"
showOplen 2 = "word"
showOplen 3 = "byte"
showOplen _ = error "illegal oplen"