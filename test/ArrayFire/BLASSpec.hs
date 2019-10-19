{-# LANGUAGE TypeApplications #-}
module ArrayFire.BLASSpec where

import ArrayFire    hiding (not)

import Data.Complex
import Test.Hspec

spec :: Spec
spec =
  describe "BLAS spec" $ do
    it "Should matmul two matrices" $ do
      (matrix @Double (2,2) [[2,2],[2,2]] `matmul` matrix @Double (2,2) [[2,2],[2,2]]) None None
        `shouldBe` matrix @Double (2,2) [[8,8],[8,8]]
    it "Should dot product two vectors" $ do
      dot (vector @Double 2 (repeat 2)) (vector @Double 2 (repeat 2)) None None
        `shouldBe`
           scalar @Double 8
    it "Should produce scalar dot product between two vectors as a Complex number" $ do
      dotAll (vector @Double 2 (repeat 2)) (vector @Double 2 (repeat 2)) None None
        `shouldBe`
           8.0 :+ 0.0
    it "Should take the transpose of a matrix" $ do
      transpose (matrix @Double (2,2) [[1,1],[2,2]]) False
        `shouldBe`
           matrix @Double (2,2) [[1,2],[1,2]]
    it "Should take the transpose of a matrix in place" $ do
      let m = matrix @Double (2,2) [[1,1],[2,2]]
      transposeInPlace m False
      m `shouldBe` matrix @Double (2,2) [[1,2],[1,2]]





