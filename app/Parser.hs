module Parser where

import Data.Word
import qualified Inst.Inst as Inst
import qualified Inst.Opcode as Opcode
import qualified Inst.Reg as Reg
import Token

data Item = Inst Inst.Inst | Label String

instance Show Item where
  show (Inst inst) = show inst
  show (Label name) = show name ++ ":"

data ParseErrorKind = ContainsUnknownChar Char | UnknownIdent String | InvalidOperands | InvalidOplen String | InvalidRegName String | InvalidVmemFlag String | InvalidCondFlag String

instance Show ParseErrorKind where
  show (ContainsUnknownChar c) = "(ContainsUnknownChar " ++ show c ++ ")"
  show (UnknownIdent s) = "(UnknownIdent " ++ show s ++ ")"
  show (InvalidOperands) = "InvalidOperands"
  show (InvalidOplen s) = "(InvalidOplen " ++ show s ++ ")"
  show (InvalidRegName s) = "(InvalidRegName " ++ show s ++ ")"
  show (InvalidVmemFlag s) = "(InvalidVmemFlag " ++ show s ++ ")"
  show (InvalidCondFlag s) = "(InvalidCondFlag " ++ show s ++ ")"

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
parseInst "cbrk" [Ident condflags] line = do
  condflags' <- parseCondFlags condflags line
  Right (Inst (Inst.Small (Inst.makeSmallInst Opcode.cbrk 0 0 0 0 condflags')))
parseInst "cbrk" _ line = Left (makeError InvalidOperands line)
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
parseInst "store_imm" [Ident oplen, Ident src, Comma, Num imm] line = do
  oplen' <- parseOplen oplen line
  src' <- parseReg src line
  let inst = Inst.makeBigInst (Opcode.store_imm + oplen') 0 src' 0 0 0 imm
  Right (Inst (Inst.Big inst))
parseInst "store_imm" _ line = Left (makeError InvalidOperands line)
parseInst "store_dir" [Ident oplen, Ident dest, Comma, Ident src, Comma, Ident vmem] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  src' <- parseReg src line
  vmem' <- parseVMemFlag vmem line
  let inst = Inst.makeSmallInst (Opcode.store_dir + oplen') dest' src' 0 0 vmem';
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
parseInst "cmp" _ _ = error "TODO"
parseInst "csel" _ _ = error "TODO"
parseInst "b" _ _ = error "TODO"
parseInst "j" _ _ = error "TODO"
parseInst "add" _ _ = error "TODO"
parseInst "sub" _ _ = error "TODO"
parseInst "mul" _ _ = error "TODO"
parseInst "div" _ _ = error "TODO"
parseInst "mod" _ _ = error "TODO"
parseInst "iadd" _ _ = error "TODO"
parseInst "isub" _ _ = error "TODO"
parseInst "imul" _ _ = error "TODO"
parseInst "idiv" _ _ = error "TODO"
parseInst "imod" _ _ = error "TODO"
parseInst "fadd" _ _ = error "TODO"
parseInst "fsub" _ _ = error "TODO"
parseInst "fmul" _ _ = error "TODO"
parseInst "fdiv" _ _ = error "TODO"
parseInst "fmod" _ _ = error "TODO"
parseInst "and" _ _ = error "TODO"
parseInst "or" _ _ = error "TODO"
parseInst "xor" _ _ = error "TODO"
parseInst "not" _ _ = error "TODO"
parseInst "muladd" _ _ = error "TODO"
parseInst "call" _ _ = error "TODO"
parseInst "ccall" _ _ = error "TODO"
parseInst "ret" _ _ = error "TODO"
parseInst "push" _ _ = error "TODO"
parseInst "pop" _ _ = error "TODO"
parseInst "libc_call" _ _ = error "TODO"
parseInst "native_call" _ _ = error "TODO"
parseInst "breakpoint" _ _ = error "TODO"
parseInst x _ line = Left (makeError (UnknownIdent x) line)

parseCondFlags :: String -> Int -> Either ParseError Word8
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
