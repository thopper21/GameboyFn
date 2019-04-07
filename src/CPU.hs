module CPU where

import           Data.IntMap (IntMap)
import           Data.Word   (Word8)
import           Memory      (Memory)
import           Register    (Registers)

data CPU = CPU
  { registers :: Registers
  , memory    :: Memory
  }
