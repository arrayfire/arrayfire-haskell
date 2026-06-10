#include "arrayfire.h"

af_err af_random_engine_set_type_(af_random_engine engine, const af_random_engine_type rtype) { return af_random_engine_set_type(&engine, rtype); }

af_err af_random_engine_set_seed_(af_random_engine engine, const unsigned long long seed) {
  return af_random_engine_set_seed(&engine, seed);
}

void zeroOutArray (af_array * arr) {
  (*arr) = 0;
}

static volatile int af_shutting_down = 0;

void af_notify_shutdown(void) {
  af_shutting_down = 1;
}

/* Safe finalizer: no-ops on null handles and after af_notify_shutdown(). */
void af_release_array_safe(af_array arr) {
  if (!af_shutting_down && arr)
    af_release_array(arr);
}
