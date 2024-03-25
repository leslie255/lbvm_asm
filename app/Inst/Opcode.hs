{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE BinaryLiterals #-}
module Inst.Opcode where

import Data.Word
import Data.Bits hiding (xor)
import Prelude hiding (not, or, and, mod, div)
import Text.Printf (printf)

make_opcode :: Word8 -> Word8
make_opcode x = (x .<<. 2) .&. 0b11111100

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

brk         = make_opcode(0) 
cbrk        = make_opcode(1)
nop         = make_opcode(2) 
load_imm    = make_opcode(3) 
load_dir    = make_opcode(4)
load_ind    = make_opcode(5)
store_imm   = make_opcode(6)
store_dir   = make_opcode(7)
store_ind   = make_opcode(8)
mov         = make_opcode(9)
cmp         = make_opcode(10)
csel        = make_opcode(11)
b           = make_opcode(12)
j           = make_opcode(13)
add         = make_opcode(14)
sub         = make_opcode(15)
mul         = make_opcode(16)
div         = make_opcode(17)
mod         = make_opcode(18)
iadd        = make_opcode(19)
isub        = make_opcode(20)
imul        = make_opcode(21)
idiv        = make_opcode(22)
imod        = make_opcode(23)
fadd        = make_opcode(24)
fsub        = make_opcode(25)
fmul        = make_opcode(26)
fdiv        = make_opcode(27)
fmod        = make_opcode(28)
and         = make_opcode(29)
or          = make_opcode(30)
xor         = make_opcode(31)
not         = make_opcode(32)
muladd      = make_opcode(33)
call        = make_opcode(34)
ccall       = make_opcode(35)
ret         = make_opcode(36)
push        = make_opcode(37)
pop         = make_opcode(38)
libc_call   = make_opcode(39)
native_call = make_opcode(40)
breakpoint  = 0xFC :: Word8

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