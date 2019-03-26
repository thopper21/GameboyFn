{-# LANGUAGE TemplateHaskell #-}

module Register (
    Register8(..),
    Register16(..),
    Registers,
    emptyRegisters,
    getRegister8,
    getRegister16,
    setRegister8,
    setRegister16) where

    import Control.Lens
    import Control.Monad
    import Data.Bits
    import Data.Word (Word8, Word16)

    data Register8 = A | B | C | D | E | H | L | F
    data Register16 = AF | BC | DE | HL | PC | SP

    data Registers = Registers {
        _a :: Word8,
        _b :: Word8,
        _c :: Word8,
        _d :: Word8,
        _e :: Word8,
        _h :: Word8,
        _l :: Word8,
        _f :: Word8,
        _pc :: Word16,
        _sp :: Word16
    } deriving (Show)

    makeLenses ''Registers

    pack :: Word8 -> Word8 -> Word16
    pack hi lo = fromIntegral hi * 256 + fromIntegral lo

    unpack :: Word16 -> (Word8, Word8)
    unpack val = (fromIntegral $ shiftR val 8, fromIntegral val)

    view16 lensHi lensLo = liftM2 pack (view lensHi) (view lensLo)

    set16 lensHi lensLo registers val =
        set lensHi hi . set lensLo lo $ registers
        where (hi, lo) = unpack val

    af :: Lens' Registers Word16
    af = lens (view16 a f) (set16 a f)

    bc :: Lens' Registers Word16
    bc = lens (view16 b c) (set16 b c)

    de :: Lens' Registers Word16
    de = lens (view16 d e) (set16 d e)
        
    hl :: Lens' Registers Word16
    hl = lens (view16 h l) (set16 h l)

    emptyRegisters = Registers {
        _a = 0,
        _b = 0,
        _c = 0,
        _d = 0,
        _e = 0,
        _h = 0,
        _l = 0,
        _f = 0,
        _pc = 0,
        _sp = 0
    }

    toLens8 A = a
    toLens8 B = b
    toLens8 C = c
    toLens8 D = d
    toLens8 E = e
    toLens8 H = h
    toLens8 L = l
    toLens8 F = f

    toLens16 AF = af
    toLens16 BC = bc
    toLens16 DE = de
    toLens16 HL = hl
    toLens16 PC = pc
    toLens16 SP = sp

    getRegister8 :: Register8 -> Registers -> Word8
    getRegister8 = view . toLens8

    getRegister16 :: Register16 -> Registers -> Word16
    getRegister16 = view . toLens16

    setRegister8 :: Register8 -> Word8 -> Registers -> Registers
    setRegister8 = set . toLens8

    setRegister16 :: Register16 -> Word16 -> Registers -> Registers
    setRegister16 = set . toLens16