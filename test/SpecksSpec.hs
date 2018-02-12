module SpecksSpec where

import           Specks
import           Test.Hspec

spec :: Spec
spec =
  before (shuffledSpecks (2, 4)) $
    describe "speckCoords" $ do
      it "should return Nothing if element is not found" $
        \field -> speckCoords (Speck 42) field `shouldBe` Nothing
      it "should return Just with coords of a given element" $
        \field -> speckCoords Cursor field `shouldBe` Just (1, 3)
