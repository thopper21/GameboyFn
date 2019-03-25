module CPU where

    import Register (Registers)
    import Data.IntMap (IntMap)
    import Data.Word (Word8)

    newtype Memory = IntMap Word8

    data CPU = CPU { registers :: Registers, memory :: Memory }