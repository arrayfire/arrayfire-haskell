{-# LANGUAGE TypeApplications #-}
module ArrayFire.LAPACKSpec where

import qualified ArrayFire       as A
import           Prelude
import           Test.Hspec

spec :: Spec
spec =
  describe "LAPACK spec" $ do
    it "Should have LAPACK available" $ do
      A.isLAPACKAvailable `shouldBe` True
    it "Should perform svd" $ do
      let (s,v,d) = A.svd $ A.matrix @Double (4,2) [ [1,2,3,4], [5,6,7,8] ]
      A.getDims s `shouldBe` (4,4,1,1)
      A.getDims v `shouldBe` (2,1,1,1)
      A.getDims d `shouldBe` (2,2,1,1)
    it "Should perform svd in place" $ do
      let (s,v,d) = A.svdInPlace $ A.matrix @Double (4,2) [ [1,2,3,4], [5,6,7,8] ]
      A.getDims s `shouldBe` (4,4,1,1)
      A.getDims v `shouldBe` (2,1,1,1)
      A.getDims d `shouldBe` (2,2,1,1)
    it "Should perform lu" $ do
      let (s,v,d) = A.lu $ A.matrix @Double (2,2) [[3,1],[4,2]]
      A.getDims s `shouldBe` (2,2,1,1)
      A.getDims v `shouldBe` (2,2,1,1)
      A.getDims d `shouldBe` (2,1,1,1)
    it "Should perform qr" $ do
      let (s,v,d) = A.lu $ A.matrix @Double (3,3) [[12,6,4],[-51,167,24],[4,-68,-41]]
      A.getDims s `shouldBe` (3,3,1,1)
      A.getDims v `shouldBe` (3,3,1,1)
      A.getDims d `shouldBe` (3,1,1,1)
    it "Should get determinant of Double" $ do
      let eles = [[3 A.:+ 1, 8 A.:+ 1], [4 A.:+ 1, 6 A.:+ 1]]
          (x,y) = A.det (A.matrix @(A.Complex Double) (2,2) eles)
      x `shouldBe` (-14)
      let (x,y) = A.det $ A.matrix @Double (2,2) [[3,8],[4,6]]
      x `shouldBe` (-14)
--    it "Should calculate inverse" $ do
--      let x = flip A.inverse A.None $ A.matrix @Double (2,2) [[4.0,7.0],[2.0,6.0]]
--      x `shouldBe` A.matrix (2,2) [[0.6,-0.7],[-0.2,0.4]]
--    it "Should calculate psuedo inverse" $ do
--      let x = A.pinverse (A.matrix @Double (2,2) [[4,7],[2,6]]) 1.0 A.None
--      x `shouldBe` A.matrix @Double (2,2) [[0.6,-0.2],[-0.7,0.4]]
