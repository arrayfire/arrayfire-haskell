#include "defines.h"

af_err afcu_get_stream(cudaStream_t* stream, int id);
af_err afcu_get_native_id(int* nativeid, int id);
af_err afcu_set_native_id(int nativeid);
