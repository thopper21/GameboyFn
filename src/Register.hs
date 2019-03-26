module Register (Register8(..), Register16(..), Registers, emptyRegisters, getRegister8, getRegister16, setRegister8, setRegister16) where

    import Control.Monad
    import Data.Bits
    import Data.Word (Word8, Word16)

    data Register8 = A | B | C | D | E | H | L | F
    data Register16 = AF | BC | DE | HL | PC | SP

    data Registers = Registers {
        a :: Word8,
        b :: Word8,
        c :: Word8,
        d :: Word8,
        e :: Word8,
        h :: Word8,
        l :: Word8,
        f :: Word8,
        pc :: Word16,
        sp :: Word16
    } deriving (Show)

    emptyRegisters = Registers {
        a = 0,
        b = 0,
        c = 0,
        d = 0,
        e = 0,
        h = 0,
        l = 0,
        f = 0,
        pc = 0,
        sp = 0
    }

    newtype Memory = IntMap Word8

    data CPU = CPU { registers :: Registers, memory :: Memory }

    pack :: Word8 -> Word8 -> Word16
    pack hi lo = fromIntegral hi * 256 + fromIntegral lo

    unpack :: Word16 -> (Word8, Word8)
    unpack val = (fromIntegral $ shiftR val 8, fromIntegral val)

    toWord16 :: (Registers -> Word8) -> (Registers -> Word8) -> Registers -> Word16
    toWord16 = liftM2 pack 

    af :: Registers -> Word16
    af = toWord16 a f

    bc :: Registers -> Word16
    bc = toWord16 b c

    de :: Registers -> Word16
    de = toWord16 d e

    hl :: Registers -> Word16
    hl = toWord16 h l

    setAF :: Registers -> Word16 -> Registers
    setAF registers val = let (hi, lo) = unpack val
        in registers { a = hi, f = lo }

    setBC :: Registers -> Word16 -> Registers
    setBC registers val = let (hi, lo) = unpack val
        in registers { b = hi, c = lo }

    setDE :: Registers -> Word16 -> Registers
    setDE registers val = let (hi, lo) = unpack val
        in registers { d = hi, e = lo }

    setHL :: Registers -> Word16 -> Registers
    setHL registers val = let (hi, lo) = unpack val
        in registers { h = hi, l = lo }

    setPC :: Registers -> Word16 -> Registers
    setPC registers val = registers { pc = val }

    setSP :: Registers -> Word16 -> Registers
    setSP registers val = registers { sp = val }

    getRegister8 :: Register8 -> Registers -> Word8
    getRegister8 A = a
    getRegister8 B = b
    getRegister8 C = c
    getRegister8 D = d
    getRegister8 E = e
    getRegister8 H = h
    getRegister8 L = l
    getRegister8 F = f

    getRegister16 :: Register16 -> Registers -> Word16
    getRegister16 AF = af
    getRegister16 BC = bc
    getRegister16 DE = de
    getRegister16 HL = hl
    getRegister16 PC = pc
    getRegister16 SP = sp

    setRegister8 :: Register8 -> Word8 -> Registers -> Registers
    setRegister8 A val registers = registers { a = val }
    setRegister8 B val registers = registers { b = val }
    setRegister8 C val registers = registers { c = val }
    setRegister8 D val registers = registers { d = val }
    setRegister8 E val registers = registers { e = val }
    setRegister8 H val registers = registers { h = val }
    setRegister8 L val registers = registers { l = val }
    setRegister8 F val registers = registers { f = val }

    setRegister16 :: Register16 -> Word16 -> Registers -> Registers
    setRegister16 AF = flip setAF
    setRegister16 BC = flip setBC
    setRegister16 DE = flip setDE
    setRegister16 HL = flip setHL
    setRegister16 PC = flip setPC
    setRegister16 SP = flip setSP