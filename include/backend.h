#include "defines.h"

af_err af_set_backend(const af_backend bknd);
af_err af_get_backend_count(unsigned* num_backends);
af_err af_get_available_backends(int* backends);
af_err af_get_backend_id(af_backend *backend, const af_array in);
af_err af_get_active_backend(af_backend *backend);
af_err af_get_device_id(int *device, const af_array in);




