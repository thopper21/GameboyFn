module Instruction
  ( instruction
  ) where

import           Control.Monad.State.Lazy
import           CPU                      (CPU (..))
import           Data.Word                (Word16, Word8)
import           Memory                   (Memory, read16, read8, write8)
import           Register                 (Register16 (..), Register8 (..),
                                           getRegister16, getRegister8,
                                           setRegister16)

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

readRegister8 :: Register8 -> State CPU Word8
readRegister8 register = gets $ getRegister8 register . registers

writeAddress8 :: Word16 -> Word8 -> State CPU ()
writeAddress8 address value = do
  writeMemory8 address value
  addTime 4

ld :: (a -> State CPU ()) -> State CPU a -> State CPU ()
ld write read = read >>= write

getOperation :: Word8 -> State CPU ()
getOperation op =
  case op of
    0x00 -> nop
    0x01 -> ld (writeRegister16 BC) readImmediate16
    0x02 -> do
      address <- readRegister16 BC
      ld (writeAddress8 address) (readRegister8 A)
    0x03 -> do
      value <- readRegister16 BC
      writeRegister16 BC (value + 1)
      addTime 4

instruction :: CPU -> CPU
instruction =
  execState $ do
    op <- readImmediate8
    getOperation op
