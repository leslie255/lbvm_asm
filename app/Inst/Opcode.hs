{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Inst.Opcode where

import Data.Word
import Data.Bits hiding (xor)
import Prelude hiding (not, or, and, mod, div)
import Text.Printf (printf)

make_opcode :: Word8 -> Word8
make_opcode x = (x .<<. 2) .&. 0xFC -- 0xFC = 0b11111100

qword = 0 :: Word8
dword = 1 :: Word8
word  = 2 :: Word8
byte  = 3 :: Word8

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
showOpcode opcode | (opcode .&. 0xFC) == brk         = "brk " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == cbrk        = "cbrk " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == nop         = "nop " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == load_imm    = "load_imm " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == load_dir    = "load_dir " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == load_ind    = "load_ind " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == store_imm   = "store_imm " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == store_dir   = "store_dir " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == store_ind   = "store_ind " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == mov         = "mov " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == cmp         = "cmp " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == csel        = "csel " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == b           = "b " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == j           = "j " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == add         = "add " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == sub         = "sub " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == mul         = "mul " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == div         = "div " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == mod         = "mod " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == iadd        = "iadd " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == isub        = "isub " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == imul        = "imul " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == idiv        = "idiv " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == imod        = "imod " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == fadd        = "fadd " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == fsub        = "fsub " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == fmul        = "fmul " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == fdiv        = "fdiv " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == fmod        = "fmod " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == and         = "and " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == or          = "or " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == xor         = "xor " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == not         = "not " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == muladd      = "muladd " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == call        = "call " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == ccall       = "ccall " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == ret         = "ret " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == push        = "push " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == pop         = "pop " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == libc_call   = "libc_call " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == native_call = "native_call " ++ showOplen(opcode .&. 0x03)
showOpcode opcode | (opcode .&. 0xFC) == breakpoint  = "breakpoint " ++ showOplen(opcode .&. 0x03)
showOpcode x = (printf "(unknown 0x%02X)" x)

showOplen :: Word8 -> String
showOplen 0 = "qword"
showOplen 1 = "dword"
showOplen 2 = "word"
showOplen 3 = "byte"
showOplen _ = error "illegal oplen"