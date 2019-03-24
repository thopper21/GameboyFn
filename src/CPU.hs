module CPU where

    import Data.Word (Word8, Word16)
    import Data.Bool (Bool)

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