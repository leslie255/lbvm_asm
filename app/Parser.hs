{-# LANGUAGE BinaryLiterals #-}

module Parser where

import Data.Bits ((.|.))
import Data.Word
import qualified Inst.Inst as Inst
import qualified Inst.Opcode as Opcode
import qualified Inst.Reg as Reg
import Token

data Item = Inst Inst.Inst | Label String

instance Show Item where
  show (Inst inst) = show inst
  show (Label name) = show name ++ ":"

data ParseErrorKind = ContainsUnknownChar Char | UnknownIdent String | InvalidOperands | InvalidOplen String | InvalidRegName String | InvalidVmemFlag String | InvalidCondFlag String | JumpOffsetTooLarge Word64

instance Show ParseErrorKind where
  show (ContainsUnknownChar c) = "(ContainsUnknownChar " ++ show c ++ ")"
  show (UnknownIdent s) = "(UnknownIdent " ++ show s ++ ")"
  show (InvalidOperands) = "InvalidOperands"
  show (InvalidOplen s) = "(InvalidOplen " ++ show s ++ ")"
  show (InvalidRegName s) = "(InvalidRegName " ++ show s ++ ")"
  show (InvalidVmemFlag s) = "(InvalidVmemFlag " ++ show s ++ ")"
  show (InvalidCondFlag s) = "(InvalidCondFlag " ++ show s ++ ")"
  show (JumpOffsetTooLarge x) = "(JumpOffsetTooLarge " ++ show x ++ ")"

data ParseError = ParseError
  { errorKind :: ParseErrorKind,
    lineNum :: Int
  }

instance Show ParseError where
  show (ParseError {errorKind = e, lineNum = l}) = show e ++ "@line" ++ show l

makeError :: ParseErrorKind -> Int -> ParseError
makeError errorKind' lineNum' = ParseError {errorKind = errorKind', lineNum = lineNum'}

parseLine :: [Token] -> Int -> Either ParseError Item
parseLine [] _ = error "parseLine recieves empty line"
parseLine (UnknownChar c : _) line = Left (makeError (ContainsUnknownChar c) line)
parseLine [Ident ident, Colon] _ = Right (Label ident)
parseLine (Ident ident : tokens) line = parseInst ident tokens line
parseLine _ _ = error "TODO"

parseInst :: String -> [Token] -> Int -> Either ParseError Item
parseInst "brk" [] _ = Right (Inst (Inst.Small (Inst.makeSmallInst Opcode.brk 0 0 0 0 0)))
parseInst "brk" _ line = Left (makeError InvalidOperands line)
parseInst "cbrk" condflags line = do
  condflags' <- parseCondFlags condflags line
  Right (Inst (Inst.Small (Inst.makeSmallInst Opcode.cbrk 0 0 0 0 condflags')))
parseInst "nop" [] _ = Right (Inst (Inst.Small (Inst.makeSmallInst Opcode.nop 0 0 0 0 0)))
parseInst "nop" _ line = Left (makeError InvalidOperands line)
parseInst "load_imm" [Ident oplen, Ident dest, Comma, Num imm] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  let inst = (Inst.makeBigInst (Opcode.load_imm + oplen') dest' 0 0 0 0 imm)
  Right (Inst (Inst.Big inst))
parseInst "load_imm" _ line = Left (makeError InvalidOperands line)
parseInst "load_dir" [Ident oplen, Ident dest, Comma, Ident src, Comma, Ident vmem] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  src' <- parseReg src line
  vmem' <- parseVMemFlag vmem line
  let inst = Inst.makeSmallInst (Opcode.load_dir + oplen') dest' src' 0 0 vmem'
  Right (Inst (Inst.Small inst))
parseInst "load_dir" _ line = Left (makeError InvalidOperands line)
parseInst "load_ind" [Ident oplen, Ident dest, Comma, Ident src, Comma, Ident vmem, Comma, Num imm] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  src' <- parseReg src line
  vmem' <- parseVMemFlag vmem line
  let inst = Inst.makeBigInst (Opcode.load_ind + oplen') dest' src' 0 0 vmem' imm
  Right (Inst (Inst.Big inst))
parseInst "load_ind" _ line = Left (makeError InvalidOperands line)
parseInst "store_imm" [Ident oplen, Ident src, Comma, Ident vmem, Comma, Num imm] line = do
  oplen' <- parseOplen oplen line
  src' <- parseReg src line
  vmem' <- parseVMemFlag vmem line
  let inst = Inst.makeBigInst (Opcode.store_imm + oplen') 0 src' 0 0 vmem' imm
  Right (Inst (Inst.Big inst))
parseInst "store_imm" _ line = Left (makeError InvalidOperands line)
parseInst "store_dir" [Ident oplen, Ident dest, Comma, Ident src, Comma, Ident vmem] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  src' <- parseReg src line
  vmem' <- parseVMemFlag vmem line
  let inst = Inst.makeSmallInst (Opcode.store_dir + oplen') dest' src' 0 0 vmem'
  Right (Inst (Inst.Small inst))
parseInst "store_dir" _ line = Left (makeError InvalidOperands line)
parseInst "store_ind" [Ident oplen, Ident dest, Comma, Ident src, Comma, Ident vmem, Comma, Num imm] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  src' <- parseReg src line
  vmem' <- parseVMemFlag vmem line
  let inst = Inst.makeBigInst (Opcode.store_ind + oplen') dest' src' 0 0 vmem' imm
  Right (Inst (Inst.Big inst))
parseInst "store_ind" _ line = Left (makeError InvalidOperands line)
parseInst "mov" [Ident oplen, Ident dest, Comma, Ident src] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  src' <- parseReg src line
  let inst = Inst.makeSmallInst (Opcode.mov + oplen') dest' src' 0 0 0
  Right (Inst (Inst.Small inst))
parseInst "mov" _ line = Left (makeError InvalidOperands line)
parseInst "cmp" [Ident oplen, Ident lhs, Comma, Ident rhs] line = do
  oplen' <- parseOplen oplen line
  lhs' <- parseReg lhs line
  rhs' <- parseReg rhs line
  let inst = Inst.makeSmallInst (Opcode.cmp + oplen') lhs' rhs' 0 0 0
  Right (Inst (Inst.Small inst))
parseInst "cmp" _ line = Left (makeError InvalidOperands line)
parseInst "csel" (Ident oplen : Ident dest : Comma : Ident lhs : Comma : Ident rhs : Comma : condflags) line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  lhs' <- parseReg lhs line
  rhs' <- parseReg rhs line
  condflags' <- parseCondFlags condflags line
  let inst = Inst.makeSmallInst (Opcode.csel + oplen') dest' lhs' rhs' 0 condflags'
  Right (Inst (Inst.Small inst))
parseInst "csel" _ line = Left (makeError InvalidOperands line)
parseInst "b" (Num offset : condflags) line = do
  offset' <- parseJumpOffset offset line
  condflags' <- parseCondFlags condflags line
  let inst = Inst.makeJumpInst Opcode.b offset' condflags'
  Right (Inst (Inst.Jump inst))
parseInst "b" (Ident _ : _) _ = error "TODO: Labels"
parseInst "b" _ line = Left (makeError InvalidOperands line)
parseInst "j" [Num offset] line = do
  offset' <- parseJumpOffset offset line
  let inst = Inst.makeJumpInst Opcode.b offset' 0
  Right (Inst (Inst.Jump inst))
parseInst "j" [Ident _] _ = error "TODO: Labels"
parseInst "j" _ line = Left (makeError InvalidOperands line)
parseInst "add" tokens line = parseArithmeticInst Opcode.add tokens line
parseInst "sub" tokens line = parseArithmeticInst Opcode.sub tokens line
parseInst "mul" tokens line = parseArithmeticInst Opcode.mul tokens line
parseInst "div" tokens line = parseArithmeticInst Opcode.div tokens line
parseInst "mod" tokens line = parseArithmeticInst Opcode.mod tokens line
parseInst "iadd" tokens line = parseArithmeticInst Opcode.iadd tokens line
parseInst "isub" tokens line = parseArithmeticInst Opcode.isub tokens line
parseInst "imul" tokens line = parseArithmeticInst Opcode.imul tokens line
parseInst "idiv" tokens line = parseArithmeticInst Opcode.idiv tokens line
parseInst "imod" tokens line = parseArithmeticInst Opcode.imod tokens line
parseInst "fadd" tokens line = parseArithmeticInst Opcode.fadd tokens line
parseInst "fsub" tokens line = parseArithmeticInst Opcode.fsub tokens line
parseInst "fmul" tokens line = parseArithmeticInst Opcode.fmul tokens line
parseInst "fdiv" tokens line = parseArithmeticInst Opcode.fdiv tokens line
parseInst "fmod" tokens line = parseArithmeticInst Opcode.fmod tokens line
parseInst "and" tokens line = parseArithmeticInst Opcode.and tokens line
parseInst "or" tokens line = parseArithmeticInst Opcode.or tokens line
parseInst "xor" tokens line = parseArithmeticInst Opcode.xor tokens line
parseInst "not" [Ident oplen, Comma, Ident dest, Comma, Ident lhs] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  lhs' <- parseReg lhs line
  let inst = Inst.makeSmallInst (Opcode.not + oplen') dest' lhs' 0 0 0
  Right (Inst (Inst.Small inst))
parseInst "not" _ line = Left (makeError (InvalidOperands) line)
parseInst "muladd" [Ident oplen, Comma, Ident dest, Comma, Ident lhs, Comma, Ident rhs, Comma, Ident rhs2] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  lhs' <- parseReg lhs line
  rhs' <- parseReg rhs line
  rhs2' <- parseReg rhs2 line
  let inst = Inst.makeSmallInst (Opcode.muladd + oplen') dest' lhs' rhs' rhs2' 0
  Right (Inst (Inst.Small inst))
parseInst "muladd" _ line = Left (makeError (InvalidOperands) line)
parseInst "call" [Num offset] line = do
  offset' <- parseJumpOffset offset line
  let inst = Inst.makeJumpInst Opcode.call offset' 0
  Right (Inst (Inst.Jump inst))
parseInst "call" [Ident _] _ = error "TODO: Labels"
parseInst "call" _ line = Left (makeError InvalidOperands line)
parseInst "ccall" (Num offset : condflags) line = do
  offset' <- parseJumpOffset offset line
  condflags' <- parseCondFlags condflags line
  let inst = Inst.makeJumpInst Opcode.ccall offset' condflags'
  Right (Inst (Inst.Jump inst))
parseInst "ccall" (Ident _ : _) _ = error "TODO: Labels"
parseInst "ccall" _ line = Left (makeError InvalidOperands line)
parseInst "ret" [] _ = Right (Inst (Inst.Small (Inst.makeSmallInst Opcode.ret 0 0 0 0 0)))
parseInst "ret" _ line = Left (makeError InvalidOperands line)
parseInst "push" [Ident oplen, Ident src] line = do
  oplen' <- parseOplen oplen line
  src' <- parseReg src line
  let inst = Inst.makeSmallInst (Opcode.push + oplen') src' 0 0 0 0
  Right (Inst (Inst.Small inst))
parseInst "push" _ line = Left (makeError InvalidOperands line)
parseInst "pop" [Ident oplen, Ident src] line = do
  oplen' <- parseOplen oplen line
  src' <- parseReg src line
  let inst = Inst.makeSmallInst (Opcode.pop + oplen') src' 0 0 0 0
  Right (Inst (Inst.Small inst))
parseInst "pop" _ line = Left (makeError InvalidOperands line)
parseInst "libc_call" _ _ = error "TODO"
parseInst "breakpoint" [] _ = Right (Inst (Inst.Small (Inst.makeSmallInst Opcode.ret 0 0 0 0 0)))
parseInst "breakpoint" _ line = Left (makeError InvalidOperands line)
parseInst x _ line = Left (makeError (UnknownIdent x) line)

parseArithmeticInst :: Word8 -> [Token] -> Int -> Either ParseError Item
parseArithmeticInst opcode [Ident oplen, Comma, Ident dest, Comma, Ident lhs, Comma, Ident rhs] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  lhs' <- parseReg lhs line
  rhs' <- parseReg rhs line
  let inst = Inst.makeSmallInst (opcode + oplen') dest' lhs' rhs' 0 0
  Right (Inst (Inst.Small inst))
parseArithmeticInst _ _ line = Left (makeError (InvalidOperands) line)

parseCondFlags :: [Token] -> Int -> Either ParseError Word8
parseCondFlags [Ident "n"] _ = Right Opcode.condflagN
parseCondFlags [Ident "z"] _ = Right Opcode.condflagZ
parseCondFlags [Ident "c"] _ = Right Opcode.condflagC
parseCondFlags [Ident "v"] _ = Right Opcode.condflagV
parseCondFlags [Ident "e"] _ = Right Opcode.condflagE
parseCondFlags [Ident "g"] _ = Right Opcode.condflagG
parseCondFlags [Ident "l"] _ = Right Opcode.condflagL
parseCondFlags [Ident "nn"] _ = Right Opcode.condflagNN
parseCondFlags [Ident "nz"] _ = Right Opcode.condflagNZ
parseCondFlags [Ident "nc"] _ = Right Opcode.condflagNC
parseCondFlags [Ident "nv"] _ = Right Opcode.condflagNV
parseCondFlags [Ident "ne"] _ = Right Opcode.condflagNE
parseCondFlags [Ident "ng"] _ = Right Opcode.condflagNG
parseCondFlags [Ident "nl"] _ = Right Opcode.condflagNL
parseCondFlags (Ident "n" : Plus : ts) line = do flag <- parseCondFlags ts line; Right (flag .|. Opcode.condflagN)
parseCondFlags (Ident "z" : Plus : ts) line = do flag <- parseCondFlags ts line; Right (flag .|. Opcode.condflagZ)
parseCondFlags (Ident "c" : Plus : ts) line = do flag <- parseCondFlags ts line; Right (flag .|. Opcode.condflagC)
parseCondFlags (Ident "v" : Plus : ts) line = do flag <- parseCondFlags ts line; Right (flag .|. Opcode.condflagV)
parseCondFlags (Ident "e" : Plus : ts) line = do flag <- parseCondFlags ts line; Right (flag .|. Opcode.condflagE)
parseCondFlags (Ident "g" : Plus : ts) line = do flag <- parseCondFlags ts line; Right (flag .|. Opcode.condflagG)
parseCondFlags (Ident "l" : Plus : ts) line = do flag <- parseCondFlags ts line; Right (flag .|. Opcode.condflagL)
parseCondFlags (Ident "nn" : Plus : ts) line = do flag <- parseCondFlags ts line; Right (flag .|. Opcode.condflagNN)
parseCondFlags (Ident "nz" : Plus : ts) line = do flag <- parseCondFlags ts line; Right (flag .|. Opcode.condflagNZ)
parseCondFlags (Ident "nc" : Plus : ts) line = do flag <- parseCondFlags ts line; Right (flag .|. Opcode.condflagNC)
parseCondFlags (Ident "nv" : Plus : ts) line = do flag <- parseCondFlags ts line; Right (flag .|. Opcode.condflagNV)
parseCondFlags (Ident "ne" : Plus : ts) line = do flag <- parseCondFlags ts line; Right (flag .|. Opcode.condflagNE)
parseCondFlags (Ident "ng" : Plus : ts) line = do flag <- parseCondFlags ts line; Right (flag .|. Opcode.condflagNG)
parseCondFlags (Ident "nl" : Plus : ts) line = do flag <- parseCondFlags ts line; Right (flag .|. Opcode.condflagNL)
parseCondFlags _ _ = error "TODO"

parseVMemFlag :: String -> Int -> Either ParseError Word8
parseVMemFlag "vmem" _ = Right 0
parseVMemFlag "real" _ = Right 1
parseVMemFlag x line = Left (makeError (InvalidVmemFlag x) line)

parseOplen :: String -> Int -> Either ParseError Word8
parseOplen "qword" _ = Right 0
parseOplen "dword" _ = Right 1
parseOplen "word" _ = Right 2
parseOplen "byte" _ = Right 4
parseOplen x line = Left (makeError (InvalidOplen x) line)

parseReg :: String -> Int -> Either ParseError Word8
parseReg "r0" _ = Right Reg.r0
parseReg "r1" _ = Right Reg.r1
parseReg "r2" _ = Right Reg.r2
parseReg "r3" _ = Right Reg.r3
parseReg "r4" _ = Right Reg.r4
parseReg "r5" _ = Right Reg.r5
parseReg "r6" _ = Right Reg.r6
parseReg "r7" _ = Right Reg.r7
parseReg "r8" _ = Right Reg.r8
parseReg "r9" _ = Right Reg.r9
parseReg "r10" _ = Right Reg.r10
parseReg "r11" _ = Right Reg.r11
parseReg "r12" _ = Right Reg.r12
parseReg "r13" _ = Right Reg.r13
parseReg "status" _ = Right Reg.status
parseReg "sp" _ = Right Reg.sp
parseReg x line = Left (makeError (InvalidRegName x) line)

parseJumpOffset :: Word64 -> Int -> Either ParseError Word16
parseJumpOffset x _ | x <= fromIntegral (maxBound :: Word16) = Right (fromIntegral x)
parseJumpOffset x line = Left (makeError (JumpOffsetTooLarge x) line)