module MemorySpec where

import           Memory
import           Test.Hspec

spec :: Spec
spec =
  describe "Memory" $ do
    it "Reads and writes general bytes" $
      let memory = write8 123 42 emptyMemory
       in read8 123 memory `shouldBe` 42
    it "Reads and writes general words" $
      let memory = write16 123 42 emptyMemory
       in read16 123 memory `shouldBe` 42
    it "Defaults to 0 when not written" $ read8 123 emptyMemory `shouldBe` 0
