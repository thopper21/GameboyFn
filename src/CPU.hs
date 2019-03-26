module CPU where

import           Data.IntMap (IntMap)
import           Data.Word   (Word8)
import           Register    (Registers)

newtype Memory =
  IntMap Word8

data CPU = CPU
  { registers :: Registers
  , memory    :: Memory
  }
