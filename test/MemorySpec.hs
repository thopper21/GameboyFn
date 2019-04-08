module MemorySpec where

import           Memory
import           Test.Hspec

spec :: Spec
spec =
  describe "Memory" $ do
    it "Reads and writes general bytes" $
      let memory = write8 emptyMemory 123 42
       in read8 memory 123 `shouldBe` 42
    it "Reads and writes general words" $
      let memory = write16 emptyMemory 123 42
       in read16 memory 123 `shouldBe` 42
    it "Defaults to 0 when not written" $ read8 emptyMemory 123 `shouldBe` 0
