{-# LANGUAGE TypeApplications #-}
module ArrayFire.IndexSpec where

import qualified ArrayFire as A
import           Test.Hspec

spec :: Spec
spec =
  describe "Index" $ do

    describe "index" $ do
      it "indexes a sub-range of a vector" $ do
        A.index (A.vector @Int 10 [1..]) [A.Seq 0 4 1]
          `shouldBe` A.vector @Int 5 [1..]
      it "indexes every other element with step=2" $ do
        A.index (A.vector @Int 6 [0,1,2,3,4,5]) [A.Seq 0 4 2]
          `shouldBe` A.vector @Int 3 [0,2,4]
      it "selects the full vector with afSpan" $ do
        let arr = A.vector @Int 5 [1..]
        A.index arr [A.afSpan] `shouldBe` arr

    describe "afSpan" $ do
      it "equals Seq 1 1 0 (the ArrayFire span sentinel)" $ do
        A.afSpan `shouldBe` A.Seq 1 1 0

    describe "lookup" $ do
      it "gathers elements by an index array" $ do
        let arr = A.vector @Double 5 [10, 20, 30, 40, 50]
            idx = A.vector @Int 3 [0, 2, 4]
        A.lookup arr idx 0
          `shouldBe` A.vector @Double 3 [10, 30, 50]
      it "allows repeated indices" $ do
        let arr = A.vector @Int 5 [10, 20, 30, 40, 50]
            idx = A.vector @Int 4 [0, 0, 4, 4]
        A.lookup arr idx 0
          `shouldBe` A.vector @Int 4 [10, 10, 50, 50]

    describe "assignSeq" $ do
      it "assigns into a middle slice of a vector" $ do
        let arr = A.vector @Double 5 [1..]
            src = A.vector @Double 3 [0, 0, 0]
        A.assignSeq arr [A.Seq 1 3 1] src
          `shouldBe` A.vector @Double 5 [1, 0, 0, 0, 5]
      it "assigns a single element" $ do
        let arr = A.vector @Double 5 [1..]
            src = A.scalar @Double 99
        A.assignSeq arr [A.Seq 2 2 1] src
          `shouldBe` A.vector @Double 5 [1, 2, 99, 4, 5]
      it "overwrites the full vector via afSpan" $ do
        let arr = A.vector @Double 5 [1..]
            src = A.vector @Double 5 (repeat 0)
        A.assignSeq arr [A.afSpan] src `shouldBe` src

    describe "indexGen" $ do
      it "indexes a sub-range of a vector with seqIdx" $ do
        let arr = A.vector @Double 5 [10, 20, 30, 40, 50]
        A.indexGen arr [A.seqIdx (A.Seq 0 2 1) False]
          `shouldBe` A.vector @Double 3 [10, 20, 30]
      it "indexes a 2D sub-matrix with two seqIdx" $ do
        -- matrix (3,3): columns [[1,2,3],[4,5,6],[7,8,9]]
        -- rows 0-1, cols 0-1 → columns [[1,2],[4,5]]
        let arr = A.matrix @Double (3,3) [[1,2,3],[4,5,6],[7,8,9]]
        A.indexGen arr [ A.seqIdx (A.Seq 0 1 1) False
                       , A.seqIdx (A.Seq 0 1 1) False ]
          `shouldBe` A.matrix @Double (2,2) [[1,2],[4,5]]

    describe "assignGen" $ do
      it "assigns into a vector slice with seqIdx" $ do
        let arr    = A.vector @Double 5 [1..]
            src    = A.vector @Double 3 [0, 0, 0]
            result = A.assignGen arr [A.seqIdx (A.Seq 1 3 1) False] src
        A.indexGen result [A.seqIdx (A.Seq 1 3 1) False] `shouldBe` src
      it "assigns into a 2D sub-matrix with two seqIdx" $ do
        let arr    = A.matrix @Double (3,3) [[1,2,3],[4,5,6],[7,8,9]]
            src    = A.matrix @Double (2,2) [[0,0],[0,0]]
            result = A.assignGen arr [ A.seqIdx (A.Seq 0 1 1) False
                                     , A.seqIdx (A.Seq 0 1 1) False ] src
        A.indexGen result [ A.seqIdx (A.Seq 0 1 1) False
                          , A.seqIdx (A.Seq 0 1 1) False ]
          `shouldBe` src
