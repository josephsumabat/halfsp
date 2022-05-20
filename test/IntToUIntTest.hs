module IntToUIntTest where

import Test.Tasty.Hspec
import GhcideSteal (intToUInt)
import Language.LSP.Types

spec_intToUInt :: Spec
spec_intToUInt =
  describe "intToUInt" $ do
    it "works with minBound" $
      intToUInt (fromIntegral $ minBound @UInt) `shouldBe` Just (minBound @UInt)
    it "works with maxBound" $
      intToUInt (fromIntegral $ maxBound @UInt) `shouldBe` Just (maxBound @UInt)
    it "doesn't work with minBound - 1" $
      intToUInt (fromIntegral (minBound @UInt) - 1) `shouldBe` Nothing
    it "doesn't work with maxBound + 1" $
      intToUInt (fromIntegral (maxBound @UInt) + 1) `shouldBe` Nothing
