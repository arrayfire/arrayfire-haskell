{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ArrayFire.IndexSpec where

import qualified ArrayFire as A
import           Data.Function ((&))
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (NonEmptyList (..), choose, forAll)

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
        let arr   = A.vector @Double 5 [10, 20, 30, 40, 50]
            ixArr = A.vector @Int 3 [0, 2, 4]
        A.lookup arr ixArr 0
          `shouldBe` A.vector @Double 3 [10, 30, 50]
      it "allows repeated indices" $ do
        let arr   = A.vector @Int 5 [10, 20, 30, 40, 50]
            ixArr = A.vector @Int 4 [0, 0, 4, 4]
        A.lookup arr ixArr 0
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

    describe "(!) operator" $ do
      it "indexes a 1D sub-range with range" $ do
        let arr = A.vector @Double 5 [10, 20, 30, 40, 50]
        (arr A.! A.range 0 2)
          `shouldBe` A.vector @Double 3 [10, 20, 30]
      it "indexes a single element with at" $ do
        let arr = A.vector @Double 5 [10, 20, 30, 40, 50]
        (arr A.! A.at 2)
          `shouldBe` A.scalar @Double 30
      it "indexes a 2D sub-matrix with a tuple" $ do
        let arr = A.matrix @Double (3,3) [[1,2,3],[4,5,6],[7,8,9]]
        (arr A.! (A.range 0 1, A.range 0 1))
          `shouldBe` A.matrix @Double (2,2) [[1,2],[4,5]]

    describe "(.~) operator" $ do
      it "assigns into a 1D slice" $ do
        let arr    = A.vector @Double 5 [1..]
            src    = A.vector @Double 3 [0, 0, 0]
            result = arr & A.range 1 3 A..~ src
        (result A.! A.range 1 3) `shouldBe` src
      it "assigns into a 2D sub-matrix" $ do
        let arr    = A.matrix @Double (3,3) [[1,2,3],[4,5,6],[7,8,9]]
            src    = A.matrix @Double (2,2) [[0,0],[0,0]]
            result = arr & (A.range 0 1, A.range 0 1) A..~ src
        (result A.! (A.range 0 1, A.range 0 1)) `shouldBe` src

    describe "rangeStep" $ do
      it "selects every other element" $ do
        let arr = A.vector @Double 6 [0,1,2,3,4,5]
        (arr A.! A.rangeStep 0 4 2)
          `shouldBe` A.vector @Double 3 [0,2,4]

    describe "indexing properties" $ do
      -- afSpan selects all elements, recovering the original array exactly.
      prop "index with afSpan is identity" $ \(NonEmpty xs) ->
        let arr = A.vector @Double (length xs) xs
        in A.index arr [A.afSpan] == arr

      -- Read-after-write: reading back the slice just written returns the source.
      prop "index (assignSeq arr seqs src) seqs = src" $
        forAll (choose (1, 20)) $ \n ->
          forAll (choose (0, n-1)) $ \lo ->
          forAll (choose (lo, n-1)) $ \hi ->
          \(xs :: [Double]) (ys :: [Double]) ->
            let arr = A.vector @Double n (take n (xs ++ repeat 0))
                src = A.vector @Double (hi - lo + 1) (take (hi - lo + 1) (ys ++ repeat 0))
                seqs = [A.Seq (fromIntegral lo) (fromIntegral hi) 1]
            in A.index (A.assignSeq arr seqs src) seqs == src

      -- lookup with identity permutation [0..n-1] returns the original array.
      prop "lookup with identity permutation is identity" $ \(NonEmpty xs) ->
        let n      = length xs
            arr    = A.vector @Double n xs
            ixArr  = A.vector @Int n [0..n-1]
        in A.lookup arr ixArr 0 == arr

      -- (.~) write-then-read consistency via the (!) operator.
      prop "(.~) then (!) recovers the written slice" $
        forAll (choose (2, 20)) $ \n ->
          forAll (choose (0, n-1)) $ \lo ->
          forAll (choose (lo, n-1)) $ \hi ->
          \(xs :: [Double]) (ys :: [Double]) ->
            let arr    = A.vector @Double n (take n (xs ++ repeat 0))
                src    = A.vector @Double (hi - lo + 1) (take (hi - lo + 1) (ys ++ repeat 0))
                result = arr & A.rangeStep lo hi 1 A..~ src
            in (result A.! A.rangeStep lo hi 1) == src
