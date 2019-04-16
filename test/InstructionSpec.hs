module InstructionSpec where

import           CPU
import           Instruction
import           Memory
import           Register
import           Test.Hspec

fromMemory :: Memory -> CPU
fromMemory memory = CPU {memory = memory, registers = emptyRegisters}

incrementsPC op amount =
  it ("Increments PC by " ++ show amount) $
    let memory = write8 0 op emptyMemory
        (cpu, _) = instruction $ fromMemory memory
        value = getRegister16 PC . registers $ cpu
    in value `shouldBe` amount

costsCycles op cycles = 
  it ("Costs " ++ show cycles ++ " cycles") $
    let memory = write8 0 op emptyMemory
        (_, time) = instruction $ fromMemory memory
    in time `shouldBe` cycles

spec :: Spec
spec = do
  describe "Instruction" $
    describe "NOP (0x00)" $ do
      0x00 `incrementsPC` 1
      0x00 `costsCycles` 4
  describe "LD" $
    describe "BC,nn (0x01)" $ do
      0x01 `incrementsPC` 3
      0x01 `costsCycles` 12
      it "Writes to BC" $
        let memory = write8 0 0x01 . write16 1 42 $ emptyMemory
            (cpu, _) = instruction $ fromMemory memory
            result = getRegister16 BC . registers $ cpu
         in result `shouldBe` 42
      it "Includes both hi and lo bytes" $
        let memory = write8 0 0x01 . write16 1 4242 $ emptyMemory
            (cpu, _) = instruction $ fromMemory memory
            result = getRegister16 BC . registers $ cpu
         in result `shouldBe` 4242
