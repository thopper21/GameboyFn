module Instruction
  ( instruction
  ) where

import           Control.Monad.State.Lazy
import           CPU                      (CPU (..))
import           Data.Word                (Word16, Word8)
import           Memory                   (Memory, read16, read8, write8)
import           Register                 (Flag (..), Register16 (..),
                                           Register8 (..), clearFlag,
                                           getRegister16, getRegister8, setFlag,
                                           setRegister16, setRegister8)

nop :: State CPU ()
nop = modify id

addTime :: Int -> State CPU ()
addTime t = modify $ \cpu -> cpu {time = (+ t) . time $ cpu}

readRegister16 :: Register16 -> State CPU Word16
readRegister16 register = gets $ getRegister16 register . registers

writeRegister16 :: Register16 -> Word16 -> State CPU ()
writeRegister16 register value =
  modify $ \cpu ->
    cpu {registers = setRegister16 register value . registers $ cpu}

readRegister8 :: Register8 -> State CPU Word8
readRegister8 register = gets $ getRegister8 register . registers

writeRegister8 :: Register8 -> Word8 -> State CPU ()
writeRegister8 register value =
  modify $ \cpu ->
    cpu {registers = setRegister8 register value . registers $ cpu}

setF :: Flag -> State CPU ()
setF flag = modify $ \cpu -> cpu {registers = setFlag flag . registers $ cpu}

clearF :: Flag -> State CPU ()
clearF flag =
  modify $ \cpu -> cpu {registers = clearFlag flag . registers $ cpu}

readMemory8 :: Word16 -> State CPU Word8
readMemory8 address = gets $ read8 address . memory

readMemory16 :: Word16 -> State CPU Word16
readMemory16 address = gets $ read16 address . memory

writeMemory8 :: Word16 -> Word8 -> State CPU ()
writeMemory8 address value =
  modify $ \cpu -> cpu {memory = write8 address value . memory $ cpu}

readImmediate :: (Word16 -> State CPU a) -> Int -> State CPU a
readImmediate read width = do
  pc <- readRegister16 PC
  value <- read pc
  writeRegister16 PC (pc + fromIntegral width)
  addTime (4 * width)
  return value

readImmediate8 :: State CPU Word8
readImmediate8 = readImmediate readMemory8 1

readImmediate16 :: State CPU Word16
readImmediate16 = readImmediate readMemory16 2

writeAddress8 :: Word16 -> Word8 -> State CPU ()
writeAddress8 address value = do
  writeMemory8 address value
  addTime 4

ld :: (a -> State CPU ()) -> State CPU a -> State CPU ()
ld write read = read >>= write

inc16 :: Register16 -> State CPU ()
inc16 register = do
  value <- readRegister16 register
  writeRegister16 register (value + 1)
  addTime 4

inc8 :: Register8 -> State CPU ()
inc8 register = do
  value <- readRegister8 register
  writeRegister8 register (value + 1)
  clearF AddSub
  updateFlag (value == 255) Zero
  updateFlag (value == 15) HalfCarry

updateFlag :: Bool -> Flag -> State CPU ()
updateFlag b =
  if b
    then setF
    else clearF

getOperation :: Word8 -> State CPU ()
getOperation op =
  case op of
    0x00 -> nop
    0x01 -> ld (writeRegister16 BC) readImmediate16
    0x02 -> do
      address <- readRegister16 BC
      ld (writeAddress8 address) (readRegister8 A)
    0x03 -> inc16 BC
    0x04 -> inc8 B

instruction :: CPU -> CPU
instruction =
  execState $ do
    op <- readImmediate8
    getOperation op
