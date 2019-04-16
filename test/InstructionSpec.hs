module InstructionSpec where

import           CPU
import           Instruction
import           Memory
import           Register
import           Test.Hspec

spec :: Spec
spec = do
  describe "Instruction" $
    describe "NOP" $ do
      it "costs 4 cycles" $
        let (cpu', time) = instruction emptyCPU
         in time `shouldBe` 4
      it "NOP increments PC" $
        let (cpu', time) = instruction emptyCPU
            reg = registers cpu'
         in getRegister16 PC reg `shouldBe` 1
  describe "LD" $
    describe "BC,nn" $ do
      it "Costs 12 cycles" $
        let memory = write8 0 0x01 emptyMemory
            cpu = CPU {memory = memory, registers = emptyRegisters}
            (cpu', time) = instruction cpu
         in time `shouldBe` 12
      it "Writes to BC" $
        let memory = write8 0 0x01 emptyMemory
            memory' = write16 1 42 memory
            cpu = CPU {memory = memory', registers = emptyRegisters}
            (cpu', time) = instruction cpu
            reg = registers cpu'
         in getRegister16 BC reg `shouldBe` 42
      it "Includes both hi and lo bytes" $
        let memory = write8 0 0x01 emptyMemory
            memory' = write16 1 4242 memory
            cpu = CPU {memory = memory', registers = emptyRegisters}
            (cpu', time) = instruction cpu
            reg = registers cpu'
         in getRegister16 BC reg `shouldBe` 4242
