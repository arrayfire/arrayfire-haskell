module ArrayFire.Internal where

import Foreign.Ptr

import ArrayFire.Types
import ArrayFire.FFI

import ArrayFire.Internal.Internal
import ArrayFire.Internal.Defines

-- af_err af_create_strided_array(af_array *arr, const void *data, const dim_t offset, const unsigned ndims, const dim_t *const dims, const dim_t *const strides, const af_dtype ty, const af_source location);
-- af_err af_get_strides(dim_t *s0, dim_t *s1, dim_t *s2, dim_t *s3, const af_array arr);
getOffset
  :: Array a
  -> Int
getOffset = fromIntegral . (`infoFromArray` af_get_offset)

getRawPtr
  :: Array a
  -> AFArray
getRawPtr = (`infoFromArray` af_get_raw_ptr)

isLinear
  :: Array a
  -> Bool
isLinear = (`infoFromArray` af_is_linear)

isOwner
  :: Array a
  -> Bool
isOwner = (`infoFromArray` af_is_owner)

getAllocatedBytes
  :: Array a
  -> Int
getAllocatedBytes =
  fromIntegral . (`infoFromArray` af_get_allocated_bytes)
