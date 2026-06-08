#include "defines.h"

af_err af_info();
af_err af_init();
af_err af_info_string(char** str, const bool verbose);
af_err af_device_info(char* d_name, char* d_platform, char *d_toolkit, char* d_compute);
af_err af_get_device_count(int *num_of_devices);
af_err af_get_dbl_support(bool* available, const int device);
af_err af_set_device(const int device);
af_err af_get_device(int *device);
af_err af_sync(const int device);
af_err af_alloc_device(void **ptr, const dim_t bytes);
af_err af_free_device(void *ptr);
af_err af_alloc_pinned(void **ptr, const dim_t bytes);
af_err af_free_pinned(void *ptr);
af_err af_alloc_host(void **ptr, const dim_t bytes);
af_err af_free_host(void *ptr);
af_err af_device_array(af_array *arr, const void *data, const unsigned ndims, const dim_t * const dims, const af_dtype type);
af_err af_device_mem_info(size_t *alloc_bytes, size_t *alloc_buffers, size_t *lock_bytes, size_t *lock_buffers);
af_err af_print_mem_info(const char *msg, const int device_id);
af_err af_device_gc();
af_err af_set_mem_step_size(const size_t step_bytes);
af_err af_get_mem_step_size(size_t *step_bytes);
af_err af_lock_device_ptr(const af_array arr);
af_err af_unlock_device_ptr(const af_array arr);
af_err af_lock_array(const af_array arr);
af_err af_is_locked_array(bool *res, const af_array arr);
af_err af_get_device_ptr(void **ptr, const af_array arr);
