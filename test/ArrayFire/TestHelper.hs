module ArrayFire.TestHelper where

import qualified ArrayFire       as A
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec

-- | True when running on the AF 3.8.2 OpenCL backend.
--
-- AF 3.8.2 OpenCL has two distinct classes of breakage:
--
--  * Vision kernels (FAST, Harris, ORB, SUSAN): thresholds ignored, garbage
--    feature coordinates, af_orb can abort the process.
--
--  * Asynchronous BLAS (af_matmul, af_gemm): clBLAS enqueues kernels without
--    a synchronisation barrier on the output buffer.  Subsequent JIT
--    operations read the unfilled buffer, producing wrong results.  The CPU
--    backend uses synchronous BLAS so it is unaffected.
brokenOpenCL :: Bool
brokenOpenCL = unsafePerformIO ((== A.OpenCL) <$> A.getActiveBackend)
{-# NOINLINE brokenOpenCL #-}

-- | Skip an expectation on the broken AF 3.8.2 OpenCL backend.
skipOnBrokenOpenCL :: String -> Expectation -> Expectation
skipOnBrokenOpenCL reason action
  | brokenOpenCL = pendingWith reason
  | otherwise    = action
