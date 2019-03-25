module RegisterSpec where

    import CPU
    import Data.Word (Word8, Word16)
    import Test.Hspec

    defaultRegisters = Registers {
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

    specPack :: Register8 -> Register8 -> Register16 -> Word16
    specPack hi lo out = getRegister16 out . setRegister8 hi 128 . setRegister8 lo 64 $ defaultRegisters

    specUnpack :: Register16 -> Register8 -> Register8 -> (Word8, Word8)
    specUnpack input hi lo = 
        let registers = setRegister16 input 32896 defaultRegisters
        in (getRegister8 hi registers, getRegister8 lo registers)

    spec :: Spec
    spec = describe "register" $ do
        it "Packs A and F into AF" $
            specPack A F AF `shouldBe` 32832
        
        it "Packs B and C into BC" $
            specPack B C BC `shouldBe` 32832
            
        it "Packs D and E into DE" $
            specPack D E DE `shouldBe` 32832

        it "Packs H and L into HL" $
            specPack H L HL `shouldBe` 32832

        it "Unpacks AF into A and F" $
            specUnpack AF A F `shouldBe` (128, 128)
        
        it "Unpacks BC into B and C" $
            specUnpack BC B C `shouldBe` (128, 128)
            
        it "Unpacks DE into D and E" $
            specUnpack DE D E `shouldBe` (128, 128)

        it "Unpacks HL into H and L" $
            specUnpack HL H L `shouldBe` (128, 128)