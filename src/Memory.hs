module Memory
  ( Memory
  , empty
  , read8
  , read16
  , write8
  , write16
  ) where

import qualified Data.IntMap as IM (IntMap, empty)
import           Data.Word   (Word16, Word8)

newtype Memory =
  Memory (IM.IntMap Word8)

empty :: Memory
empty = Memory IM.empty

read8 :: Memory -> Word8
read8 _ = 0

read16 :: Memory -> Word16
read16 _ = 0

write8 :: Memory -> Word8 -> Memory
write8 memory _ = memory

write16 :: Memory -> Word16 -> Memory
write16 memory _ = memory
