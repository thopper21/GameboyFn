module CPU where

    import Data.Word (Word8, Word16)
    import Data.Bool (Bool)
    import Data.IntMap (IntMap)

    data Registers = Registers {
        a :: Word8,
        b :: Word8,
        c :: Word8,
        d :: Word8,
        e :: Word8,
        h :: Word8,
        l :: Word8,
        f :: Word8,
        pc :: Word16,
        sp :: Word16
    } deriving (Show)

    newtype Memory = IntMap Word8

    data CPU = CPU { registers :: Registers, memory :: Memory }

    toWord16 :: (Registers -> Word8) -> (Registers -> Word8) -> Registers -> Word16
    toWord16 hi lo registers = fromIntegral (hi registers) * 256 + fromIntegral (lo registers)

    af :: Registers -> Word16
    af = toWord16 a f

    bc :: Registers -> Word16
    bc = toWord16 b c

    de :: Registers -> Word16
    de = toWord16 d e

    hl :: Registers -> Word16
    hl = toWord16 h l