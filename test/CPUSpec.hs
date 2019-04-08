module CPUSpec where

import           CPU
import           Register
import           Test.Hspec

spec :: Spec
spec =
  describe "operation" $
  describe "NOP" $ do
    it "costs 4 cycles" $
      let (cpu', time) = instruction emptyCPU
       in time `shouldBe` 4
    it "NOP increments PC" $
      let (cpu', time) = instruction emptyCPU
          reg = registers cpu'
       in getRegister16 PC reg `shouldBe` 1
