module Memory
  ( Memory
  , emptyMemory
  , read8
  , read16
  , write8
  , write16
  ) where

import           Bits
import           Control.Monad.State
import           Data.IntMap
import           Data.Word           (Word16, Word8)

newtype Memory =
  Memory (IntMap Word8)

emptyMemory :: Memory
emptyMemory = Memory empty

read8 :: Word16 -> Memory -> Word8
read8 address (Memory map) = findWithDefault 0 (fromIntegral address) map

read16 :: Word16 -> Memory -> Word16
read16 address memory =
  let hi = read8 address memory
      lo = read8 (address + 1) memory
   in pack hi lo

write8 :: Word16 -> Word8 -> Memory -> Memory
write8 address value (Memory map) =
  Memory $ insert (fromIntegral address) value map

write16 :: Word16 -> Word16 -> Memory -> Memory
write16 address value =
  let (hi, lo) = unpack value
   in write8 address hi . write8 (address + 1) lo
