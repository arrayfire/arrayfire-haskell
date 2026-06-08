#include "defines.h"

af_err af_create_features(af_features *feat, dim_t num);
af_err af_retain_features(af_features *out, const af_features feat);
af_err af_get_features_num(dim_t *num, const af_features feat);
af_err af_get_features_xpos(af_array *out, const af_features feat);
af_err af_get_features_ypos(af_array *out, const af_features feat);
af_err af_get_features_score(af_array *score, const af_features feat);
af_err af_get_features_orientation(af_array *orientation, const af_features feat);
af_err af_get_features_size(af_array *size, const af_features feat);
af_err af_release_features(af_features feat);
