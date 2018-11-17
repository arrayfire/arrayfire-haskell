module Data.Array.Fire.Internal.Random where

-- AFAPI af_err af_create_random_engine(af_random_engine *engine, af_random_engine_type rtype, uintl seed);
-- AFAPI af_err af_retain_random_engine(af_random_engine *out, const af_random_engine engine);
-- AFAPI af_err af_random_engine_set_type(af_random_engine *engine, const af_random_engine_type rtype);
-- AFAPI af_err af_random_engine_get_type(af_random_engine_type *rtype, const af_random_engine engine);
-- AFAPI af_err af_random_uniform(af_array *out, const unsigned ndims, const dim_t * const dims, const af_dtype type, af_random_engine engine);
-- AFAPI af_err af_random_normal(af_array *out, const unsigned ndims, const dim_t * const dims, const af_dtype type, af_random_engine engine);
-- AFAPI af_err af_random_engine_set_seed(af_random_engine *engine, const uintl seed);
-- AFAPI af_err af_get_default_random_engine(af_random_engine *engine);
-- AFAPI af_err af_set_default_random_engine_type(const af_random_engine_type rtype);
-- AFAPI af_err af_random_engine_get_seed(uintl * const seed, af_random_engine engine);
-- AFAPI af_err af_release_random_engine(af_random_engine engine);
-- AFAPI af_err af_randu(af_array *out, const unsigned ndims, const dim_t * const dims, const af_dtype type);
-- AFAPI af_err af_randn(af_array *out, const unsigned ndims, const dim_t * const dims, const af_dtype type);
-- AFAPI af_err af_set_seed(const uintl seed);
-- AFAPI af_err af_get_seed(uintl *seed);


