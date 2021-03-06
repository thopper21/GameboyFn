module RegisterSpec where

import           Data.Word  (Word16, Word8)
import           Register
import           Test.Hspec

specPack hi lo out =
  let val =
        getRegister16 out . setRegister8 hi 128 . setRegister8 lo 64 $
        emptyRegisters
   in val `shouldBe` (128 * 256 + 64)

specUnpack input hi lo =
  let registers = setRegister16 input (128 * 256 + 128) emptyRegisters
   in (getRegister8 hi registers, getRegister8 lo registers) `shouldBe`
      (128, 128)

spec :: Spec
spec =
  describe "register" $ do
    it "Packs A and F into AF" $ specPack A F AF
    it "Packs B and C into BC" $ specPack B C BC
    it "Packs D and E into DE" $ specPack D E DE
    it "Packs H and L into HL" $ specPack H L HL
    it "Unpacks AF into A and F" $ specUnpack AF A F
    it "Unpacks BC into B and C" $ specUnpack BC B C
    it "Unpacks DE into D and E" $ specUnpack DE D E
    it "Unpacks HL into H and L" $ specUnpack HL H L
