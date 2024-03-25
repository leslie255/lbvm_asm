{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Inst.Reg where

import Data.Word

r0      = 0  :: Word8
r1      = 1  :: Word8
r2      = 2  :: Word8
r3      = 3  :: Word8
r4      = 4  :: Word8
r5      = 5  :: Word8
r6      = 6  :: Word8
r7      = 7  :: Word8
r8      = 8  :: Word8
r9      = 9  :: Word8
r10     = 10 :: Word8
r11     = 11 :: Word8
r12     = 12 :: Word8
r13     = 13 :: Word8
status  = 14 :: Word8
sp      = 15 :: Word8

showReg :: Word8 -> String
showReg x
  | x == r0     = "r0"
  | x == r1     = "r1"
  | x == r2     = "r2"
  | x == r3     = "r3"
  | x == r4     = "r4"
  | x == r5     = "r5"
  | x == r6     = "r6"
  | x == r7     = "r7"
  | x == r8     = "r8"
  | x == r9     = "r9"
  | x == r10    = "r10"
  | x == r11    = "r11"
  | x == r12    = "r12"
  | x == r13    = "r13"
  | x == status = "status"
  | x == sp     = "sp"
  | True = error "unreachable pattern"