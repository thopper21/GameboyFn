module RegisterSpec where

    import Test.Hspec
    import CPU

    defaultRegister = Registers {
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

    spec :: Spec
    spec = describe "register" $ do
        it "Packs A and F into AF" $
            af (defaultRegister { a = 128, f = 128 }) `shouldBe` 32896
        
        it "Packs B and C into BC" $
            bc (defaultRegister { b = 128, c = 128 }) `shouldBe` 32896
            
        it "Packs D and E into DE" $
            de (defaultRegister { d = 128, e = 128 }) `shouldBe` 32896

        it "Packs H and L into HL" $
            hl (defaultRegister { h = 128, l = 128 }) `shouldBe` 32896

        it "Unpacks AF into A and F" $
            let registers = setAF defaultRegister 32896
            in (a registers, f registers) `shouldBe` (128, 128)
        
        it "Unpacks BC into B and C" $
            let registers = setBC defaultRegister 32896
            in (b registers, c registers) `shouldBe` (128, 128)
            
        it "Unpacks DE into D and E" $
            let registers = setDE defaultRegister 32896
            in (d registers, e registers) `shouldBe` (128, 128)

        it "Unpacks HL into H and L" $
            let registers = setHL defaultRegister 32896
            in (h registers, l registers) `shouldBe` (128, 128)