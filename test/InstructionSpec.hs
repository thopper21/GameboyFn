module InstructionSpec where

import           CPU
import           Instruction
import           Memory
import           Register
import           Test.Hspec

toCPU :: Memory -> Registers -> CPU
toCPU memory registers = CPU {memory = memory, registers = registers, time = 0}

fromMemory :: Memory -> CPU
fromMemory memory = toCPU memory emptyRegisters

fromRegisters :: Registers -> CPU
fromRegisters = toCPU emptyMemory

incrementsPC op amount =
  it ("Increments PC by " ++ show amount) $
  let memory = write8 0 op emptyMemory
      cpu = instruction $ fromMemory memory
      value = getRegister16 PC . registers $ cpu
   in value `shouldBe` amount

costsCycles op cycles =
  it ("Costs " ++ show cycles ++ " cycles") $
  let memory = write8 0 op emptyMemory
      cpu = instruction $ fromMemory memory
   in time cpu `shouldBe` cycles

spec :: Spec
spec =
  describe "Instruction" $ do
    describe "NOP (0x00)" $ do
      0x00 `incrementsPC` 1
      0x00 `costsCycles` 4
    describe "LD" $ do
      describe "BC,nn (0x01)" $ do
        0x01 `incrementsPC` 3
        0x01 `costsCycles` 12
        it "Writes to BC" $
          let memory = write8 0 0x01 . write16 1 42 $ emptyMemory
              cpu = instruction $ fromMemory memory
              result = getRegister16 BC . registers $ cpu
           in result `shouldBe` 42
        it "Includes both hi and lo bytes" $
          let memory = write8 0 0x01 . write16 1 4242 $ emptyMemory
              cpu = instruction $ fromMemory memory
              result = getRegister16 BC . registers $ cpu
           in result `shouldBe` 4242
      describe "(BC),nn (0x02)" $ do
        0x02 `incrementsPC` 1
        0x02 `costsCycles` 8
        it "Writes A to (BC)" $
          let reg = setRegister8 A 42 . setRegister16 BC 123 $ emptyRegisters
              mem = write8 0 0x02 emptyMemory
              cpu = instruction $ toCPU mem reg
              value = read8 123 . memory $ cpu
           in value `shouldBe` 42
    describe "INC" $
      describe "BC (0x03)" $ do
        0x03 `incrementsPC` 1
        0x03 `costsCycles` 8
        it "Increments BC" $
          let reg = setRegister16 BC 42 emptyRegisters
              mem = write8 0 0x03 emptyMemory
              cpu = instruction $ toCPU mem reg
              value = getRegister16 BC . registers $ cpu
           in value `shouldBe` 43
