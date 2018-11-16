#define AF_API_VERSION 1.0

typedef enum {
    f32,    ///< 32-bit floating point values
    c32,    ///< 32-bit complex floating point values
    f64,    ///< 64-bit complex floating point values
    c64,    ///< 64-bit complex floating point values
    b8 ,    ///< 8-bit boolean values
    s32,    ///< 32-bit signed integral values
    u32,    ///< 32-bit unsigned integral values
    u8 ,    ///< 8-bit unsigned integral values
    s64,    ///< 64-bit signed integral values
    u64,    ///< 64-bit unsigned integral values
    s16,    ///< 16-bit signed integral values
    u16,    ///< 16-bit unsigned integral values
} af_dtype;

typedef enum {
    ///
    /// The function returned successfully
    ///
    AF_SUCCESS            =   0,

    // 100-199 Errors in environment

    ///
    /// The system or device ran out of memory
    ///
    AF_ERR_NO_MEM         = 101,

    ///
    /// There was an error in the device driver
    ///
    AF_ERR_DRIVER         = 102,

    ///
    /// There was an error with the runtime environment
    ///
    AF_ERR_RUNTIME        = 103,

    // 200-299 Errors in input parameters

    ///
    /// The input array is not a valid af_array object
    ///
    AF_ERR_INVALID_ARRAY  = 201,

    ///
    /// One of the function arguments is incorrect
    ///
    AF_ERR_ARG            = 202,

    ///
    /// The size is incorrect
    ///
    AF_ERR_SIZE           = 203,

    ///
    /// The type is not suppported by this function
    ///
    AF_ERR_TYPE           = 204,

    ///
    /// The type of the input arrays are not compatible
    ///
    AF_ERR_DIFF_TYPE      = 205,

    ///
    /// Function does not support GFOR / batch mode
    ///
    AF_ERR_BATCH          = 207,

    ///
    /// Input does not belong to the current device.
    ///
    AF_ERR_DEVICE         = 208,

    // 300-399 Errors for missing software features

    ///
    /// The option is not supported
    ///
    AF_ERR_NOT_SUPPORTED  = 301,

    ///
    /// This build of ArrayFire does not support this feature
    ///
    AF_ERR_NOT_CONFIGURED = 302,

    ///
    /// This build of ArrayFire is not compiled with "nonfree" algorithms
    ///
    AF_ERR_NONFREE        = 303,

    // 400-499 Errors for missing hardware features

    ///
    /// This device does not support double
    ///
    AF_ERR_NO_DBL         = 401,

    ///
    /// This build of ArrayFire was not built with graphics or this device does
    /// not support graphics
    ///
    AF_ERR_NO_GFX         = 402,

    // 500-599 Errors specific to heterogenous API

    ///
    /// There was an error when loading the libraries
    ///
    AF_ERR_LOAD_LIB       = 501,

    ///
    /// There was an error when loading the symbols
    ///
    AF_ERR_LOAD_SYM       = 502,

    ///
    /// There was a mismatch between the input array and the active backend
    ///
    AF_ERR_ARR_BKND_MISMATCH    = 503,

    // 900-999 Errors from upstream libraries and runtimes

    ///
    /// There was an internal error either in ArrayFire or in a project
    /// upstream
    ///
    AF_ERR_INTERNAL       = 998,

    ///
    /// Unknown Error
    ///
    AF_ERR_UNKNOWN        = 999
} af_err;
