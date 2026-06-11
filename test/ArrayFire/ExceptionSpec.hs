{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ArrayFire.ExceptionSpec where

import           Control.Exception          (evaluate, try)
import qualified ArrayFire                  as A
import           ArrayFire.Exception
import           ArrayFire.Internal.Defines (AFErr (..))
import           Test.Hspec

spec :: Spec
spec = describe "Exception spec" $ do

  -- The error-code → constructor table is the heart of the FFI error path;
  -- a wrong entry silently mislabels every failure of that kind.
  describe "toAFExceptionType" $ do

    it "maps every documented AFErr code to its constructor" $
      map (toAFExceptionType . AFErr)
        [101,102,103,201,202,203,204,205,207,208,301,302,303,401,402,501,502,503,998,999]
        `shouldBe`
        [ NoMemoryError, DriverError, RuntimeError, InvalidArrayError, ArgError
        , SizeError, TypeError, DiffTypeError, BatchError, DeviceError
        , NotSupportedError, NotConfiguredError, NonFreeError, NoDblError
        , NoGfxError, LoadLibError, LoadSymError, BackendMismatchError
        , InternalError, UnknownError
        ]

    it "maps unrecognized codes to UnhandledError" $ do
      toAFExceptionType (AFErr 0)     `shouldBe` UnhandledError
      toAFExceptionType (AFErr 12345) `shouldBe` UnhandledError

  -- End-to-end: a genuine ArrayFire failure must cross the FFI boundary as a
  -- typed 'AFException', not a crash or an opaque error.
  describe "library errors surface as AFException" $

    it "a matmul dimension mismatch throws a typed AFException" $ do
      let a = A.mkArray @Double [2,3] [1..6]   -- 2x3
          b = A.mkArray @Double [2,2] [1..4]   -- 2x2  (inner dims 3 /= 2)
      r <- try (evaluate (A.getElements (A.matmul a b A.None A.None)))
             :: IO (Either AFException Int)
      case r of
        Right n ->
          expectationFailure ("expected an AFException, but got " ++ show n)
        Left (AFException ty code _msg) -> do
          ty   `shouldSatisfy` (`elem` [SizeError, ArgError])
          code `shouldSatisfy` (> 0)
