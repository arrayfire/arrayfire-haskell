#include "defines.h"

af_err af_print_array(af_array arr);
af_err af_print_array_gen(const char *exp, const af_array arr, const int precision);
af_err af_save_array(int *index, const char* key, const af_array arr, const char *filename, const bool append);
af_err af_read_array_index(af_array *out, const char *filename, const unsigned index);
af_err af_read_array_key(af_array *out, const char *filename, const char* key);
af_err af_read_array_key_check(int *index, const char *filename, const char* key);
af_err af_array_to_string(char **output, const char *exp, const af_array arr, const int precision, const bool transpose);
af_err af_example_function(af_array* out, const af_array in, const af_someenum_t param);
af_err af_get_version(int *major, int *minor, int *patch);
const char *af_get_revision();
af_err af_get_size_of(size_t *size, af_dtype type);
