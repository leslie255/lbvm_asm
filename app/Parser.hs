{-# LANGUAGE BinaryLiterals #-}

module Parser where

import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word
import Inst.Inst (Inst)
import qualified Inst.Inst as Inst
import qualified Inst.Opcode as Opcode
import qualified Inst.Reg as Reg
import Token

data Item = Inst Inst | WithSymbol SymboledInst | Label String | Segment Segment | Bytes [Word8]
  deriving (Show)

data SymboledInst = SymboledJump (Inst.JumpInst, String) | SymboledBig (Inst.BigInst, String)
  deriving (Show)

data Segment = Text | Data
  deriving (Show, Eq)

symbolOf :: SymboledInst -> String
symbolOf (SymboledJump (_, x)) = x
symbolOf (SymboledBig (_, x)) = x

instOf :: SymboledInst -> Inst
instOf (SymboledJump (x, _)) = Inst.Jump x
instOf (SymboledBig (x, _)) = Inst.Big x

data ParseErrorKind
  = InvalidSyntax
  | ContainsUnknownChar Char
  | UnknownIdent String
  | InvalidOperands
  | InvalidOplen String
  | InvalidRegName String
  | InvalidVmemFlag String
  | InvalidCondFlag
  | JumpOffsetTooLarge Word64
  | InvalidLibcCallcode String
  | InvalidSegmentName String
  | InvalidByteLiteral
  | NumTooLarge
  deriving (Show)

data ParseError = ParseError
  { errorKind :: ParseErrorKind,
    lineNum :: Int
  }

instance Show ParseError where
  show (ParseError {errorKind = e, lineNum = l}) = show e ++ "@line" ++ show l

parseLine :: [Token] -> Int -> Either ParseError Item
parseLine [] _ = error "parseLine recieves empty line"
parseLine [Ident "segment", Ident "text"] _ = Right $ Segment Text
parseLine [Ident "segment", Ident "data"] _ = Right $ Segment Data
parseLine (Ident "bytes" : tokens) line = parseBytes tokens line
parseLine [Ident "segment", Ident s] line = Left $ ParseError (InvalidSegmentName s) line
parseLine [Ident ident, Colon] _ = Right $ Label ident
parseLine (Ident ident : tokens) line = parseInst ident tokens line
parseLine _ line = Left $ ParseError InvalidSyntax line

parseBytes :: [Token] -> Int -> Either ParseError Item
parseBytes tokens line' = do
  bytes <- parseBytesInner tokens line' []
  Right $ Bytes bytes
  where
    parseBytesInner :: [Token] -> Int -> [Word8] -> Either ParseError [Word8]
    parseBytesInner (t@(Num _) : Comma : ts) line bytes = parseBytesInner (t : ts) line bytes
    parseBytesInner (Num n : ts) line bytes
      | n <= fromIntegral (maxBound :: Word8) =
          parseBytesInner ts line $ bytes ++ [fromIntegral n]
    parseBytesInner (Num _ : _) line _ = Left $ ParseError (NumTooLarge) line
    parseBytesInner (t@(Str _) : Comma : ts) line bytes = parseBytesInner (t : ts) line bytes
    parseBytesInner (Str s : ts) line bytes =
      let byteString = BS.unpack $ TE.encodeUtf8 $ T.pack s
       in parseBytesInner ts line (bytes ++ byteString)
    parseBytesInner [] _ bytes = Right bytes
    parseBytesInner _ line _ = Left $ ParseError (InvalidByteLiteral) line

parseInst :: String -> [Token] -> Int -> Either ParseError Item
parseInst "brk" [] _ = Right $ Inst $ Inst.Small (Inst.makeSmallInst Opcode.brk 0 0 0 0 0)
parseInst "brk" _ line = Left (ParseError InvalidOperands line)
parseInst "cbrk" condflags line = do
  condflags' <- parseCondFlags condflags line
  Right $ Inst $ Inst.Small $ Inst.makeSmallInst Opcode.cbrk 0 0 0 0 condflags'
parseInst "nop" [] _ = Right $ Inst $ Inst.Small (Inst.makeSmallInst Opcode.nop 0 0 0 0 0)
parseInst "nop" _ line = Left (ParseError InvalidOperands line)
parseInst "load_imm" [Ident oplen, Ident dest, Comma, Num imm] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  Right $ Inst $ Inst.Big $ Inst.makeBigInst (Opcode.load_imm + oplen') dest' 0 0 0 0 imm
parseInst "load_imm" [Ident oplen, Ident dest, Comma, Ident label] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  Right $ WithSymbol $ SymboledBig $ (Inst.makeBigInst (Opcode.load_imm + oplen') dest' 0 0 0 0 0, label)
parseInst "load_imm" _ line = Left (ParseError InvalidOperands line)
parseInst "load_dir" [Ident oplen, Ident dest, Comma, Ident src, Comma, Ident vmem] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  src' <- parseReg src line
  vmem' <- parseVMemFlag vmem line
  Right $ Inst $ Inst.Small $ Inst.makeSmallInst (Opcode.load_dir + oplen') dest' src' 0 0 vmem'
parseInst "load_dir" _ line = Left (ParseError InvalidOperands line)
parseInst "load_ind" [Ident oplen, Ident dest, Comma, Ident src, Comma, Ident vmem, Comma, Num imm] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  src' <- parseReg src line
  vmem' <- parseVMemFlag vmem line
  Right $ Inst $ Inst.Big $ Inst.makeBigInst (Opcode.load_ind + oplen') dest' src' 0 0 vmem' imm
parseInst "load_ind" [Ident oplen, Ident dest, Comma, Ident src, Comma, Ident vmem, Comma, Ident label] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  src' <- parseReg src line
  vmem' <- parseVMemFlag vmem line
  Right $ WithSymbol $ SymboledBig $ (Inst.makeBigInst (Opcode.load_ind + oplen') dest' src' 0 0 vmem' 0, label)
parseInst "load_ind" _ line = Left (ParseError InvalidOperands line)
parseInst "store_imm" [Ident oplen, Ident src, Comma, Ident vmem, Comma, Num imm] line = do
  oplen' <- parseOplen oplen line
  src' <- parseReg src line
  vmem' <- parseVMemFlag vmem line
  Right $ Inst $ Inst.Big $ Inst.makeBigInst (Opcode.store_imm + oplen') 0 src' 0 0 vmem' imm
parseInst "store_imm" [Ident oplen, Ident src, Comma, Ident vmem, Comma, Ident label] line = do
  oplen' <- parseOplen oplen line
  src' <- parseReg src line
  vmem' <- parseVMemFlag vmem line
  Right $ WithSymbol $ SymboledBig $ (Inst.makeBigInst (Opcode.store_imm + oplen') 0 src' 0 0 vmem' 0, label)
parseInst "store_imm" _ line = Left (ParseError InvalidOperands line)
parseInst "store_dir" [Ident oplen, Ident dest, Comma, Ident src, Comma, Ident vmem] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  src' <- parseReg src line
  vmem' <- parseVMemFlag vmem line
  Right $ Inst $ Inst.Small $ Inst.makeSmallInst (Opcode.store_dir + oplen') dest' src' 0 0 vmem'
parseInst "store_dir" _ line = Left (ParseError InvalidOperands line)
parseInst "store_ind" [Ident oplen, Ident dest, Comma, Ident src, Comma, Ident vmem, Comma, Num imm] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  src' <- parseReg src line
  vmem' <- parseVMemFlag vmem line
  Right $ Inst $ Inst.Big $ Inst.makeBigInst (Opcode.store_ind + oplen') dest' src' 0 0 vmem' imm
parseInst "store_ind" [Ident oplen, Ident dest, Comma, Ident src, Comma, Ident vmem, Comma, Ident label] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  src' <- parseReg src line
  vmem' <- parseVMemFlag vmem line
  Right $ WithSymbol $ SymboledBig $ (Inst.makeBigInst (Opcode.store_ind + oplen') dest' src' 0 0 vmem' 0, label)
parseInst "store_ind" _ line = Left (ParseError InvalidOperands line)
parseInst "mov" [Ident oplen, Ident dest, Comma, Ident src] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  src' <- parseReg src line
  Right $ Inst $ Inst.Small $ Inst.makeSmallInst (Opcode.mov + oplen') dest' src' 0 0 0
parseInst "mov" _ line = Left (ParseError InvalidOperands line)
parseInst "cmp" [Ident oplen, Ident lhs, Comma, Ident rhs] line = do
  oplen' <- parseOplen oplen line
  lhs' <- parseReg lhs line
  rhs' <- parseReg rhs line
  Right $ Inst $ Inst.Small $ Inst.makeSmallInst (Opcode.cmp + oplen') lhs' rhs' 0 0 0
parseInst "cmp" _ line = Left (ParseError InvalidOperands line)
parseInst "fcmp" [Ident oplen, Ident lhs, Comma, Ident rhs] line = do
  oplen' <- parseOplen oplen line
  lhs' <- parseReg lhs line
  rhs' <- parseReg rhs line
  Right $ Inst $ Inst.Small $ Inst.makeSmallInst (Opcode.fcmp + oplen') lhs' rhs' 0 0 0
parseInst "fcmp" _ line = Left (ParseError InvalidOperands line)
parseInst "csel" (Ident oplen : Ident dest : Comma : Ident lhs : Comma : Ident rhs : Comma : condflags) line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  lhs' <- parseReg lhs line
  rhs' <- parseReg rhs line
  condflags' <- parseCondFlags condflags line
  Right $ Inst $ Inst.Small $ Inst.makeSmallInst (Opcode.csel + oplen') dest' lhs' rhs' 0 condflags'
parseInst "csel" _ line = Left (ParseError InvalidOperands line)
parseInst "b" (Num offset : Comma : condflags) line = do
  offset' <- parseJumpOffset offset line
  condflags' <- parseCondFlags condflags line
  Right $ Inst $ Inst.Jump $ Inst.makeJumpInst Opcode.b offset' condflags'
parseInst "b" (Ident label : Comma : condflags) line = do
  condflags' <- parseCondFlags condflags line
  Right $ WithSymbol $ SymboledJump $ (Inst.makeJumpInst Opcode.b 0 condflags', label)
parseInst "b" _ line = Left (ParseError InvalidOperands line)
parseInst "j" [Num offset] line = do
  offset' <- parseJumpOffset offset line
  Right $ Inst $ Inst.Jump $ Inst.makeJumpInst Opcode.b offset' 0
parseInst "j" [Ident label] _ = do
  Right $ WithSymbol $ SymboledJump $ (Inst.makeJumpInst Opcode.j 0 0, label)
parseInst "j" _ line = Left (ParseError InvalidOperands line)
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
parseInst "ineg" [Ident oplen, Ident dest, Comma, Ident lhs] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  lhs' <- parseReg lhs line
  Right $ Inst $ Inst.Small $ Inst.makeSmallInst (Opcode.ineg + oplen') dest' lhs' 0 0 0
parseInst "ineg" _ line = Left $ ParseError (InvalidOperands) line
parseInst "fneg" [Ident oplen, Ident dest, Comma, Ident lhs] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  lhs' <- parseReg lhs line
  Right $ Inst $ Inst.Small $ Inst.makeSmallInst (Opcode.fneg + oplen') dest' lhs' 0 0 0
parseInst "fneg" _ line = Left $ ParseError (InvalidOperands) line
parseInst "shl" tokens line = parseArithmeticInst Opcode.shl tokens line
parseInst "shr" tokens line = parseArithmeticInst Opcode.shr tokens line
parseInst "and" tokens line = parseArithmeticInst Opcode.and tokens line
parseInst "or" tokens line = parseArithmeticInst Opcode.or tokens line
parseInst "xor" tokens line = parseArithmeticInst Opcode.xor tokens line
parseInst "not" [Ident oplen, Ident dest, Comma, Ident lhs] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  lhs' <- parseReg lhs line
  Right $ Inst $ Inst.Small $ Inst.makeSmallInst (Opcode.not + oplen') dest' lhs' 0 0 0
parseInst "not" _ line = Left $ ParseError (InvalidOperands) line
parseInst "muladd" [Ident oplen, Ident dest, Comma, Ident lhs, Comma, Ident rhs, Comma, Ident rhs2] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  lhs' <- parseReg lhs line
  rhs' <- parseReg rhs line
  rhs2' <- parseReg rhs2 line
  Right $ Inst $ Inst.Small $ Inst.makeSmallInst (Opcode.muladd + oplen') dest' lhs' rhs' rhs2' 0
parseInst "muladd" _ line = Left (ParseError (InvalidOperands) line)
parseInst "call" [Num offset] line = do
  offset' <- parseJumpOffset offset line
  Right $ Inst $ Inst.Jump $ Inst.makeJumpInst Opcode.call offset' 0
parseInst "call" [Ident label] _ = do
  Right $ WithSymbol $ SymboledJump $ (Inst.makeJumpInst Opcode.call 0 0, label)
parseInst "call" _ line = Left (ParseError InvalidOperands line)
parseInst "ccall" (Num offset : Comma : condflags) line = do
  offset' <- parseJumpOffset offset line
  condflags' <- parseCondFlags condflags line
  Right $ Inst $ Inst.Jump $ Inst.makeJumpInst Opcode.ccall offset' condflags'
parseInst "ccall" (Ident label : Comma : condflags) line = do
  condflags' <- parseCondFlags condflags line
  Right $ WithSymbol $ SymboledJump $ (Inst.makeJumpInst Opcode.ccall 0 condflags', label)
parseInst "ccall" _ line = Left (ParseError InvalidOperands line)
parseInst "ret" [] _ = Right $ Inst $ Inst.Small $ Inst.makeSmallInst Opcode.ret 0 0 0 0 0
parseInst "ret" _ line = Left $ ParseError InvalidOperands line
parseInst "push" [Ident oplen, Ident src] line = do
  oplen' <- parseOplen oplen line
  src' <- parseReg src line
  Right $ Inst $ Inst.Small $ Inst.makeSmallInst (Opcode.push + oplen') src' 0 0 0 0
parseInst "push" _ line = Left (ParseError InvalidOperands line)
parseInst "pop" [Ident oplen, Ident src] line = do
  oplen' <- parseOplen oplen line
  src' <- parseReg src line
  Right $ Inst $ Inst.Small $ Inst.makeSmallInst (Opcode.pop + oplen') src' 0 0 0 0
parseInst "pop" _ line = Left (ParseError InvalidOperands line)
parseInst "libc_call" [Ident callcode] line = do
  callcode' <- parseLibcCallcode callcode line
  Right $ Inst $ Inst.Small $ Inst.makeSmallInst (Opcode.libc_call) 0 0 0 0 callcode'
parseInst "libc_call" _ line = Left $ ParseError InvalidOperands line
parseInst "native_call" _ line = error $ "Not implemented: native_call instruction (@line" ++ show line ++ ")"
parseInst "vtoreal" [Ident dest, Comma, Ident src] line = do
  dest' <- parseReg dest line
  src' <- parseReg src line
  Right $ Inst $ Inst.Small $ Inst.makeSmallInst (Opcode.vtoreal) src' dest' 0 0 0
parseInst "vtoreal" _ line = Left $ ParseError InvalidOperands line
parseInst "breakpoint" [] _ = Right $ Inst $ Inst.Small $ Inst.makeSmallInst Opcode.breakpoint 0 0 0 0 0
parseInst "breakpoint" _ line = Left $ ParseError InvalidOperands line
parseInst x _ line = Left $ ParseError (UnknownIdent x) line

parseArithmeticInst :: Word8 -> [Token] -> Int -> Either ParseError Item
parseArithmeticInst opcode [Ident oplen, Ident dest, Comma, Ident lhs, Comma, Ident rhs] line = do
  oplen' <- parseOplen oplen line
  dest' <- parseReg dest line
  lhs' <- parseReg lhs line
  rhs' <- parseReg rhs line
  Right $ Inst $ Inst.Small $ Inst.makeSmallInst (opcode + oplen') dest' lhs' rhs' 0 0
parseArithmeticInst _ _ line = Left $ ParseError (InvalidOperands) line

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
parseCondFlags _ line = Left $ ParseError InvalidCondFlag line

parseVMemFlag :: String -> Int -> Either ParseError Word8
parseVMemFlag "vmem" _ = Right 0
parseVMemFlag "real" _ = Right 1
parseVMemFlag x line = Left $ ParseError (InvalidVmemFlag x) line

parseOplen :: String -> Int -> Either ParseError Word8
parseOplen "q" _ = Right Opcode.qword
parseOplen "d" _ = Right Opcode.dword
parseOplen "w" _ = Right Opcode.word
parseOplen "b" _ = Right Opcode.byte
parseOplen x line = Left $ ParseError (InvalidOplen x) line

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
parseReg x line = Left $ ParseError (InvalidRegName x) line

parseJumpOffset :: Word64 -> Int -> Either ParseError Word16
parseJumpOffset x line
  | x <= fromIntegral (maxBound :: Word16) = Right (fromIntegral x)
  | True = Left $ ParseError (JumpOffsetTooLarge x) line

parseLibcCallcode :: String -> Int -> Either ParseError Word8
parseLibcCallcode "exit" _ = Right 255
parseLibcCallcode "malloc" _ = Right 1
parseLibcCallcode "realloc" _ = Right 2
parseLibcCallcode "free" _ = Right 3
parseLibcCallcode "fwrite" _ = Right 4
parseLibcCallcode "fread" _ = Right 5
parseLibcCallcode "printf" _ = Right 6
parseLibcCallcode "fprintf" _ = Right 7
parseLibcCallcode "scanf" _ = Right 8
parseLibcCallcode "fscanf" _ = Right 9
parseLibcCallcode "puts" _ = Right 10
parseLibcCallcode "fputs" _ = Right 11
parseLibcCallcode "snprintf" _ = Right 12
parseLibcCallcode "fopen" _ = Right 13
parseLibcCallcode "fclose" _ = Right 14
parseLibcCallcode "memcpy" _ = Right 15
parseLibcCallcode "memmove" _ = Right 16
parseLibcCallcode "memset" _ = Right 17
parseLibcCallcode "bzero" _ = Right 18
parseLibcCallcode "strlen" _ = Right 19
parseLibcCallcode "strcpy" _ = Right 20
parseLibcCallcode "strcat" _ = Right 21
parseLibcCallcode "strcmp" _ = Right 22
parseLibcCallcode x l = Left $ ParseError (InvalidLibcCallcode x) l
