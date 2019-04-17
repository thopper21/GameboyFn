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

addTime :: Int -> CPU -> CPU
addTime t cpu = cpu {time = (+ t) . time $ cpu}

readRegister16 :: Register16 -> CPU -> Word16
readRegister16 register = getRegister16 register . registers

writeRegister16 :: Register16 -> Word16 -> CPU -> CPU
writeRegister16 register value cpu =
  cpu {registers = setRegister16 register value . registers $ cpu}

readImmediate :: (Word16 -> Memory -> a) -> Int -> CPU -> (CPU, a)
readImmediate read width cpu =
  let pc = readRegister16 PC cpu
      value = read pc $ memory cpu
      cpu' = writeRegister16 PC (pc + fromIntegral width) cpu
      cpu'' = addTime (4 * width) cpu'
   in (cpu'', value)

readImmediate8 :: CPU -> (CPU, Word8)
readImmediate8 = readImmediate read8 1

readImmediate16 :: CPU -> (CPU, Word16)
readImmediate16 = readImmediate read16 2

readRegister8 :: Register8 -> CPU -> Word8
readRegister8 register = getRegister8 register . registers

writeAddress8 :: Word16 -> Word8 -> CPU -> CPU
writeAddress8 address value cpu =
  let memory' = write8 address value . memory $ cpu
      cpu' = addTime 4 cpu
   in cpu' {memory = memory'}

ld :: (a -> CPU -> CPU) -> (CPU -> a) -> CPU -> CPU
ld write read cpu = write (read cpu) cpu

getOperation :: Word8 -> CPU -> CPU
getOperation op cpu =
  case op of
    0x00 -> nop cpu
    0x01 ->
      let (cpu', value) = readImmediate16 cpu
       in ld (writeRegister16 BC) (const value) cpu'
    0x02 ->
      let address = getRegister16 BC . registers $ cpu
       in ld (writeAddress8 address) (readRegister8 A) cpu

instruction :: CPU -> CPU
instruction cpu =
  let (cpu', op) = readImmediate8 cpu
   in getOperation op cpu'
