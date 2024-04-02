module Common where
import Data.Word
import Data.Bits

-- GHC only supports x86_64, ARM and aarch64, so not much point checking platform endianness here.
word16ToLeBytesList :: Word16 -> [Word8]
word16ToLeBytesList x = [fromIntegral x, fromIntegral $ x .>>. 8]

word32ToLeBytesList :: Word32 -> [Word8]
word32ToLeBytesList x =
  [ fromIntegral x,
    fromIntegral $ x .>>. 8,
    fromIntegral $ x .>>. 16,
    fromIntegral $ x .>>. 24
  ]

word64ToLeBytesList :: Word64 -> [Word8]
word64ToLeBytesList x =
  [ fromIntegral x,
    fromIntegral $ x .>>. 8,
    fromIntegral $ x .>>. 16,
    fromIntegral $ x .>>. 24,
    fromIntegral $ x .>>. 32,
    fromIntegral $ x .>>. 40,
    fromIntegral $ x .>>. 48,
    fromIntegral $ x .>>. 56
  ]

-- GHC only supports x86_64, ARM and aarch64, so not much point checking platform endianness here.
word16ToLeBytes :: Word16 -> (Word8, Word8)
word16ToLeBytes x = (fromIntegral x, fromIntegral $ x .>>. 8)

word32ToLeBytes :: Word32 -> (Word8, Word8, Word8, Word8)
word32ToLeBytes x =
  ( fromIntegral x,
    fromIntegral $ x .>>. 8,
    fromIntegral $ x .>>. 16,
    fromIntegral $ x .>>. 24
  )

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
