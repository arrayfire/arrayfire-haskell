module Data.Array.Fire.Internal.Images where

-- AFAPI af_err af_gradient(af_array *dx, af_array *dy, const af_array in);
-- AFAPI af_err af_load_image(af_array *out, const char* filename, const bool isColor);
-- AFAPI af_err af_save_image(const char* filename, const af_array in);
-- AFAPI af_err af_load_image_memory(af_array *out, const void* ptr);
-- AFAPI af_err af_save_image_memory(void** ptr, const af_array in, const af_image_format format);
-- AFAPI af_err af_delete_image_memory(void* ptr);
-- AFAPI af_err af_load_image_native(af_array *out, const char* filename);
-- AFAPI af_err af_save_image_native(const char* filename, const af_array in);
-- AFAPI af_err af_is_image_io_available(bool *out);
-- AFAPI af_err af_resize(af_array *out, const af_array in, const dim_t odim0, const dim_t odim1, const af_interp_type method);
-- AFAPI af_err af_transform(af_array *out, const af_array in, const af_array transform, const dim_t odim0, const dim_t odim1, const af_interp_type method, const bool inverse);
-- AFAPI af_err af_transform_coordinates(af_array *out, const af_array tf, const float d0, const float d1);
-- AFAPI af_err af_rotate(af_array *out, const af_array in, const float theta, const bool crop, const af_interp_type method);
-- AFAPI af_err af_translate(af_array *out, const af_array in, const float trans0, const float trans1, const dim_t odim0, const dim_t odim1, const af_interp_type method);
-- AFAPI af_err af_scale(af_array *out, const af_array in, const float scale0, const float scale1, const dim_t odim0, const dim_t odim1, const af_interp_type method);
-- AFAPI af_err af_skew(af_array *out, const af_array in, const float skew0, const float skew1, const dim_t odim0, const dim_t odim1, const af_interp_type method, const bool inverse);
-- AFAPI af_err af_histogram(af_array *out, const af_array in, const unsigned nbins, const double minval, const double maxval);
-- AFAPI af_err af_dilate(af_array *out, const af_array in, const af_array mask);
-- AFAPI af_err af_dilate3(af_array *out, const af_array in, const af_array mask);
--     AFAPI af_err af_erode(af_array *out, const af_array in, const af_array mask);
--     AFAPI af_err af_erode3(af_array *out, const af_array in, const af_array mask);
--     AFAPI af_err af_bilateral(af_array *out, const af_array in, const float spatial_sigma, const float chromatic_sigma, const bool isColor);
--     AFAPI af_err af_mean_shift(af_array *out, const af_array in, const float spatial_sigma, const float chromatic_sigma, const unsigned iter, const bool is_color);
--    AFAPI af_err af_minfilt(af_array *out, const af_array in, const dim_t wind_length, const dim_t wind_width, const af_border_type edge_pad);
--     AFAPI af_err af_maxfilt(af_array *out, const af_array in, const dim_t wind_length, const dim_t wind_width, const af_border_type edge_pad);
--     AFAPI af_err af_regions(af_array *out, const af_array in, const af_connectivity connectivity, const af_dtype ty);
--     AFAPI af_err af_sobel_operator(af_array *dx, af_array *dy, const af_array img, const unsigned ker_size);
--     AFAPI af_err af_rgb2gray(af_array* out, const af_array in, const float rPercent, const float gPercent, const float bPercent);
--     AFAPI af_err af_gray2rgb(af_array* out, const af_array in, const float rFactor, const float gFactor, const float bFactor);
--     AFAPI af_err af_hist_equal(af_array *out, const af_array in, const af_array hist);
--     AFAPI af_err af_gaussian_kernel(af_array *out, const int rows, const int cols, const double sigma_r, const double sigma_c);
--     AFAPI af_err af_hsv2rgb(af_array* out, const af_array in);
--     AFAPI af_err af_rgb2hsv(af_array* out, const af_array in);
--     AFAPI af_err af_color_space(af_array *out, const af_array image, const af_cspace_t to, const af_cspace_t from);
--    AFAPI af_err af_unwrap(af_array *out, const af_array in, const dim_t wx, const dim_t wy, const dim_t sx, const dim_t sy, const dim_t px, const dim_t py, const bool is_column);
--   AFAPI af_err af_wrap(af_array *out, const af_array in, const dim_t ox, const dim_t oy, const dim_t wx, const dim_t wy, const dim_t sx, const dim_t sy, const dim_t px, const dim_t py, const bool is_column);
--     AFAPI af_err af_sat(af_array *out, const af_array in);
--     AFAPI af_err af_ycbcr2rgb(af_array* out, const af_array in, const af_ycc_std standard);
--     AFAPI af_err af_rgb2ycbcr(af_array* out, const af_array in, const af_ycc_std standard);
--     AFAPI af_err af_moments(af_array *out, const af_array in, const af_moment_type moment);
--     AFAPI af_err af_moments_all(double* out, const af_array in, const af_moment_type moment);
-- AFAPI af_err af_canny(af_array* out, const af_array in, const af_canny_threshold threshold_type, const float low_threshold_ratio, const float high_threshold_ratio, const unsigned sobel_window, const bool is_fast);
--     AFAPI af_err af_anisotropic_diffusion(af_array* out, const af_array in,
    --                                       const float timestep,
    --                                       const float conductance,
    --                                       const unsigned iterations,
    --                                       const af_flux_function fftype,
    --                                       const af_diffusion_eq diffusion_kind);

    -- AFAPI af_err af_iterative_deconv(af_array* out,
    --                                  const af_array in, const af_array ker,
    --                                  const unsigned iterations,
    --                                  const float relax_factor,
    --                                  const af_iterative_deconv_algo algo);

    -- AFAPI af_err af_inverse_deconv(af_array* out, const af_array in,
    --                                const af_array psf, const float gamma,
    --                                const af_inverse_deconv_algo algo);













