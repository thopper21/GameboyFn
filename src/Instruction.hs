module Instruction
  ( instruction
  ) where

import           CPU       (CPU (..))
import           Data.Word (Word16, Word8)
import           Memory    (Memory, read16, read8, write8)
import           Register  (Register16 (..), Register8 (..), getRegister16,
                            getRegister8, setRegister16)

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

readRegister8 :: Register8 -> CPU -> (Word8, Int)
readRegister8 register cpu = (getRegister8 register . registers $ cpu, 0)

writeReg16 :: Register16 -> CPU -> Word16 -> (CPU, Int)
writeReg16 register cpu value =
  let registers' = setRegister16 register value (registers cpu)
   in (cpu {registers = registers'}, 0)

writeAddress8 :: Word16 -> CPU -> Word8 -> (CPU, Int)
writeAddress8 address cpu value =
  (cpu {memory = write8 address value . memory $ cpu}, 4)

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
    0x02 ->
      let address = getRegister16 BC . registers $ cpu
       in ld (writeAddress8 address) (readRegister8 A) cpu

instruction :: CPU -> (CPU, Int)
instruction cpu =
  let (cpu', op, readTime) = readImmediate8 cpu
      (cpu'', opTime) = getOperation op cpu'
   in (cpu'', readTime + opTime)
