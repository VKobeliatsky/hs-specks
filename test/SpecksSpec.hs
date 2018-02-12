module SpecksSpec where

import           Specks
import           Test.Hspec

spec :: Spec
spec =
  before (shuffledSpecks (2, 4)) $
    describe "speckIndex" $ do
      it "should return Nothing if element is not found" $
        \field -> speckIndex (Speck 42) field `shouldBe` Nothing
      it "should return Just with coords of a given element" $
        \field -> speckIndex Cursor field `shouldBe` Just (2, 4)
