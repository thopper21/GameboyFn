module Bits where

import           Data.Bits
import           Data.Word (Word16, Word8)

pack :: Word8 -> Word8 -> Word16
pack hi lo = fromIntegral hi * 256 + fromIntegral lo

unpack :: Word16 -> (Word8, Word8)
unpack val = (fromIntegral $ shiftR val 8, fromIntegral val)
