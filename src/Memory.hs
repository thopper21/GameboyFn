module Memory
  ( Memory
  , emptyMemory
  , read8
  , read16
  , write8
  , write16
  ) where

import           Bits
import           Data.IntMap
import           Data.Word   (Word16, Word8)

newtype Memory =
  Memory (IntMap Word8)

emptyMemory :: Memory
emptyMemory = Memory empty

read8 :: Memory -> Word16 -> Word8
read8 (Memory map) address = findWithDefault 0 (fromIntegral address) map

read16 :: Memory -> Word16 -> Word16
read16 memory address =
  let hi = read8 memory address
      lo = read8 memory $ address + 1
   in pack hi lo

write8 :: Memory -> Word16 -> Word8 -> Memory
write8 (Memory map) address value =
  Memory $ insert (fromIntegral address) value map

write16 :: Memory -> Word16 -> Word16 -> Memory
write16 memory address value =
  let (hi, lo) = unpack value
      memory' = write8 memory address hi
   in write8 memory' (address + 1) lo
