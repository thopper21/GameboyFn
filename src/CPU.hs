module CPU where

import           Data.IntMap (IntMap)
import           Data.Word   (Word8)
import           Memory      (Memory, emptyMemory)
import           Register    (Registers, emptyRegisters)

data CPU = CPU
  { registers :: Registers
  , memory    :: Memory
  }

emptyCPU :: CPU
emptyCPU = CPU {registers = emptyRegisters, memory = emptyMemory}
