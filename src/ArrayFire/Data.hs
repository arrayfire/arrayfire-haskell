module ArrayFire.Data where

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

-- import ArrayFire.Internal.Util
import ArrayFire.Internal.Data
import ArrayFire.Exception
import ArrayFire.Internal.Defines

    -- /**
    --     \param[out] arr is the generated array of given type
    --     \param[in] val is the value of each element in the generated array
    --     \param[in] ndims is size of dimension array \p dims
    --     \param[in] dims is the array containing sizes of the dimension
    --     \param[in] type is the type of array to generate
    --    \ingroup data_func_constant
    -- /

constant
  :: Double -- ^ Value of each element in the array
  -> Int -- ^ Size of dimension array
  -> DimT -- ^ Array containing sizes of the dimension
  -> AFDtype -- ^ Type of the Array
  -> IO AFArray
constant val ndims dims dtype = do
  alloca $ \arr ->
    alloca $ \dimt -> do
      poke dimt dims
      r <- af_constant arr val (fromIntegral ndims) dimt dtype
      peek arr
