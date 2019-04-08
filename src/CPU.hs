module CPU
  ( CPU(..)
  , emptyCPU
  , instruction
  ) where

import           Data.IntMap (IntMap)
import           Data.Word   (Word16, Word8)
import           Memory      (Memory, emptyMemory, read16)
import           Register    (Register16 (..), Registers, emptyRegisters,
                              getRegister16, setRegister16)

data CPU = CPU
  { registers :: Registers
  , memory    :: Memory
  }

emptyCPU :: CPU
emptyCPU = CPU {registers = emptyRegisters, memory = emptyMemory}

nop :: CPU -> (CPU, Int)
nop cpu = (cpu, 4)

getOperation :: Word16 -> CPU -> (CPU, Int)
getOperation op =
  case op of
    0x00 -> nop

instruction :: CPU -> (CPU, Int)
instruction cpu =
  let reg = registers cpu
      mem = memory cpu
      pc = getRegister16 PC reg
      op = read8 mem pc
      reg' = setRegister16 PC (pc + 1) reg
      cpu' = cpu {registers = reg'}
   in getOperation op cpu'
