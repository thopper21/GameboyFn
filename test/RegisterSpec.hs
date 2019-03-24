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
        it "Combines A and F for AF" $
            af (defaultRegister { a = 128, f = 128 }) `shouldBe` 32896
        
        it "Combines B and C for BC" $
            bc (defaultRegister { b = 128, c = 128 }) `shouldBe` 32896
            
        it "Combines D and E for DE" $
            de (defaultRegister { d = 128, e = 128 }) `shouldBe` 32896

        it "Combines H and L for HL" $
            bc (defaultRegister { b = 128, c = 128 }) `shouldBe` 32896