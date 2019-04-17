module Instruction
  ( instruction
  ) where

import           CPU       (CPU (..))
import           Data.Word (Word16, Word8)
import           Memory    (Memory, read16, read8, write8)
import           Register  (Register16 (..), Register8 (..), getRegister16,
                            getRegister8, setRegister16)

nop :: CPU -> CPU
nop = id

readImmediate :: (Word16 -> Memory -> a) -> Int -> CPU -> (CPU, a)
readImmediate read width cpu@CPU {registers = reg, memory = mem, time = t} =
  let pc = getRegister16 PC reg
      value = read pc mem
      reg' = setRegister16 PC (pc + fromIntegral width) reg
      cpu' = cpu {registers = reg', time = t + 4 * width}
   in (cpu', value)

readImmediate8 :: CPU -> (CPU, Word8)
readImmediate8 = readImmediate read8 1

readImmediate16 :: CPU -> (CPU, Word16)
readImmediate16 = readImmediate read16 2

readRegister8 :: Register8 -> CPU -> Word8
readRegister8 register = getRegister8 register . registers

writeReg16 :: Register16 -> CPU -> Word16 -> CPU
writeReg16 register cpu value =
  let registers' = setRegister16 register value (registers cpu)
   in cpu {registers = registers'}

writeAddress8 :: Word16 -> CPU -> Word8 -> CPU
writeAddress8 address cpu value =
  cpu {memory = write8 address value . memory $ cpu, time = (+ 4) . time $ cpu}

ld :: (CPU -> a -> CPU) -> (CPU -> a) -> CPU -> CPU
ld write read cpu =
  let value = read cpu
   in write cpu value

getOperation :: Word8 -> CPU -> CPU
getOperation op cpu =
  case op of
    0x00 -> cpu
    0x01 ->
      let (cpu', value) = readImmediate16 cpu
       in ld (writeReg16 BC) (const value) cpu'
    0x02 ->
      let address = getRegister16 BC . registers $ cpu
       in ld (writeAddress8 address) (readRegister8 A) cpu

instruction :: CPU -> CPU
instruction cpu =
  let (cpu', op) = readImmediate8 cpu
   in getOperation op cpu'
