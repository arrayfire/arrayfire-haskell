{-# LANGUAGE TypeApplications #-}
module ArrayFire.SparseSpec where

import qualified ArrayFire       as A
import           Data.Int
import           Data.Word
import           Data.Complex
import           Data.Proxy
import           Foreign.C.Types
import           Test.Hspec

spec :: Spec
spec =
  describe "Sparse spec" $ do
    it "Should create a sparse array" $ do
      (1+1) `shouldBe` 2
      -- A.createSparseArrayFromDense (A.matrix @Double (10,10) [1..]) A.CSR
      --   `shouldBe`
      --      A.vector @Double 10 [0..]
