module SpecksSpec where

import           Specks
import           Test.Hspec

spec :: Spec
spec =
  before (shuffledSpecks (3, 5)) $ do
    describe "speckCoords" $ do
      it "should return Nothing if element is not found" $
        \field -> speckCoords (Speck 42) field `shouldBe` Nothing
      it "should return Just with coords of a given element" $
        \field -> speckCoords Cursor field `shouldBe` Just (2, 4)
    describe "neighbourSpecks" $ do
      it "should return neighbours for a speck in the middle" $
        \field -> neighbourSpecks (1, 1) field `shouldBe` [field!(0, 1), field!(1, 2), field!(2, 1), field!(1, 0)]
      it "should return left and top element for the last speck" $
        \field -> neighbourSpecks (2, 4) field `shouldBe` [field!(1, 4), field!(2, 3)]
      it "should return bottom and right element for the first spec" $
        \field -> neighbourSpecks (0, 0) field `shouldBe` [field!(0, 1), field!(1, 0)]
      it "should return right, bottom and left element for the top spec" $
        \field -> neighbourSpecks (0, 1) field `shouldBe` [field!(0, 2), field!(1, 1), field!(0, 0)]
      it "should return top, right and left element for the bottom spec" $
        \field -> neighbourSpecks (2, 1) field `shouldBe` [field!(1, 1), field!(2, 2), field!(2, 0)]
