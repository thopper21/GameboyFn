{-# LANGUAGE TemplateHaskell #-}

module Register
  ( Register8(..)
  , Register16(..)
  , Registers
  , Flag(..)
  , emptyRegisters
  , getRegister8
  , getRegister16
  , setRegister8
  , setRegister16
  , setFlag
  , clearFlag
  , getFlag
  ) where

import           Bits
import           Control.Lens
import           Control.Monad
import           Data.Bits
import           Data.Word     (Word16, Word8)

data Register8
  = A
  | B
  | C
  | D
  | E
  | H
  | L
  | F

data Register16
  = AF
  | BC
  | DE
  | HL
  | PC
  | SP

data Flag
  = Zero
  | Carry
  | AddSub
  | HalfCarry

data Registers = Registers
  { _a  :: Word8
  , _b  :: Word8
  , _c  :: Word8
  , _d  :: Word8
  , _e  :: Word8
  , _h  :: Word8
  , _l  :: Word8
  , _f  :: Word8
  , _pc :: Word16
  , _sp :: Word16
  } deriving (Show)

makeLenses ''Registers

view16 lensHi lensLo = liftM2 pack (view lensHi) (view lensLo)

set16 lensHi lensLo registers val = set lensHi hi . set lensLo lo $ registers
  where
    (hi, lo) = unpack val

af :: Lens' Registers Word16
af = lens (view16 a f) (set16 a f)

bc :: Lens' Registers Word16
bc = lens (view16 b c) (set16 b c)

de :: Lens' Registers Word16
de = lens (view16 d e) (set16 d e)

hl :: Lens' Registers Word16
hl = lens (view16 h l) (set16 h l)

emptyRegisters =
  Registers
    { _a = 0
    , _b = 0
    , _c = 0
    , _d = 0
    , _e = 0
    , _h = 0
    , _l = 0
    , _f = 0
    , _pc = 0
    , _sp = 0
    }

toLens8 register =
  case register of
    A -> a
    B -> b
    C -> c
    D -> d
    E -> e
    H -> h
    L -> l
    F -> f

toLens16 register =
  case register of
    AF -> af
    BC -> bc
    DE -> de
    HL -> hl
    PC -> pc
    SP -> sp

getRegister8 :: Register8 -> Registers -> Word8
getRegister8 = view . toLens8

getRegister16 :: Register16 -> Registers -> Word16
getRegister16 = view . toLens16

setRegister8 :: Register8 -> Word8 -> Registers -> Registers
setRegister8 = set . toLens8

setRegister16 :: Register16 -> Word16 -> Registers -> Registers
setRegister16 = set . toLens16

getFlagBit :: Flag -> Int
getFlagBit flag =
  case flag of
    Zero      -> 7
    AddSub    -> 6
    HalfCarry -> 5
    Carry     -> 4

updateFlag :: (Word8 -> Int -> Word8) -> Flag -> Registers -> Registers
updateFlag update flag = over f (`update` getFlagBit flag)

setFlag :: Flag -> Registers -> Registers
setFlag = updateFlag setBit

clearFlag :: Flag -> Registers -> Registers
clearFlag = updateFlag clearBit

getFlag :: Flag -> Registers -> Bool
getFlag flag registers = testBit (view f registers) (getFlagBit flag)
