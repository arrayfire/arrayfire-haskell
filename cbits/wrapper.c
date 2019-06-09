#include "arrayfire.h"

af_err af_random_engine_set_type_(af_random_engine engine, const af_random_engine_type rtype) { return af_random_engine_set_type(&engine, rtype); }

af_err af_random_engine_set_seed_(af_random_engine engine, const unsigned long long seed) {
  return af_random_engine_set_seed(&engine, seed);
}
