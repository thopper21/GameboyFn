module Instruction
  ( instruction
  ) where

import           CPU       (CPU (..))
import           Data.Word (Word16, Word8)
import           Memory    (Memory, read16, read8)
import           Register  (Register16 (..), getRegister16, setRegister16)

nop :: CPU -> (CPU, Int)
nop cpu = (cpu, 0)

readImmediate :: (Word16 -> Memory -> a) -> Int -> CPU -> (CPU, a, Int)
readImmediate read width cpu@CPU {registers = reg, memory = mem} =
  let pc = getRegister16 PC reg
      value = read pc mem
      reg' = setRegister16 PC (pc + fromIntegral width) reg
      cpu' = cpu {registers = reg'}
   in (cpu', value, 4 * width)

readImmediate8 :: CPU -> (CPU, Word8, Int)
readImmediate8 = readImmediate read8 1

readImmediate16 :: CPU -> (CPU, Word16, Int)
readImmediate16 = readImmediate read16 2

writeReg16 :: Register16 -> CPU -> Word16 -> (CPU, Int)
writeReg16 register cpu value =
  let registers' = setRegister16 register value (registers cpu)
   in (cpu {registers = registers'}, 0)

readConstant16 :: Word16 -> Int -> CPU -> (Word16, Int)
readConstant16 value time _ = (value, time)

ld :: (CPU -> a -> (CPU, Int)) -> (CPU -> (a, Int)) -> CPU -> (CPU, Int)
ld write read cpu =
  let (value, readTime) = read cpu
      (cpu', writeTime) = write cpu value
   in (cpu', readTime + writeTime)

getOperation :: Word8 -> CPU -> (CPU, Int)
getOperation op cpu =
  case op of
    0x00 -> nop cpu
    0x01 ->
      let (cpu', value, readTime) = readImmediate16 cpu
       in ld (writeReg16 BC) (readConstant16 value readTime) cpu'

instruction :: CPU -> (CPU, Int)
instruction cpu =
  let (cpu', op, readTime) = readImmediate8 cpu
      (cpu'', opTime) = getOperation op cpu'
   in (cpu'', readTime + opTime)
