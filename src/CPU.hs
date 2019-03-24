module CPU where

    import Data.Bits
    import Data.Bool (Bool)
    import Data.IntMap (IntMap)
    import Data.Word (Word8, Word16)

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

    pack :: Word8 -> Word8 -> Word16
    pack hi lo = fromIntegral hi * 256 + fromIntegral lo

    unpack :: Word16 -> (Word8, Word8)
    unpack val = (fromIntegral (shiftR val 8), fromIntegral val)

    toWord16 :: (Registers -> Word8) -> (Registers -> Word8) -> Registers -> Word16
    toWord16 hi lo registers = pack (hi registers) (lo registers) 

    af :: Registers -> Word16
    af = toWord16 a f

    bc :: Registers -> Word16
    bc = toWord16 b c

    de :: Registers -> Word16
    de = toWord16 d e

    hl :: Registers -> Word16
    hl = toWord16 h l

    setAF :: Registers -> Word16 -> Registers
    setAF registers val = let (hi, lo) = unpack val
        in registers { a = hi, f = lo }

    setBC :: Registers -> Word16 -> Registers
    setBC registers val = let (hi, lo) = unpack val
        in registers { b = hi, c = lo }

    setDE :: Registers -> Word16 -> Registers
    setDE registers val = let (hi, lo) = unpack val
        in registers { d = hi, e = lo }

    setHL :: Registers -> Word16 -> Registers
    setHL registers val = let (hi, lo) = unpack val
        in registers { h = hi, l = lo }