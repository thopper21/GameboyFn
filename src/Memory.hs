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

read8 :: Memory -> Word16 -> Word8
read8 _ _ = 0

read16 :: Memory -> Word16 -> Word16
read16 _ _ = 0

write8 :: Memory -> Word16 -> Word8 -> Memory
write8 memory _ _ = memory

write16 :: Memory -> Word16 -> Word16 -> Memory
write16 memory _ _ = memory
