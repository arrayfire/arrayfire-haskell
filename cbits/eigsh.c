/*
 * cbits/eigsh.c
 *
 * GPU-resident symmetric eigendecomposition via cuSOLVER.
 *
 * Supports f32 (cusolverDnSsyevd) and f64 (cusolverDnDsyevd).
 * cuSOLVER is resolved at runtime through dlopen/dlsym — no link-time
 * dependency on CUDA toolkit.  The only link-time requirements are
 * libaf (ArrayFire) and libdl.
 *
 * Returns AF_ERR_RUNTIME when CUDA backend is not active or cuSOLVER
 * cannot be found (graceful degradation on CPU/OpenCL builds).
 *
 * Ordering: cusolverDnDsyevd returns eigenvalues in ascending order,
 * matching hmatrix's eigSH convention.
 */

#define _GNU_SOURCE
#define AF_DEFINE_CUDA_TYPES   /* gives us cudaStream_t in af/cuda.h */
#include "arrayfire.h"
#include "af/cuda.h"
#include <dlfcn.h>
#include <stddef.h>

/* ── minimal cuSOLVER types (avoids needing CUDA toolkit headers) ── */
typedef void *cusolverDnHandle_t;
typedef void *cudaStream_t_t;         /* distinct name to avoid redefinition */
typedef int   cusolverStatus_t;

#define CUSOLVER_STATUS_SUCCESS   0
#define CUBLAS_FILL_MODE_LOWER    0
#define CUSOLVER_EIG_MODE_VECTOR  1

/* ── function pointer typedefs ── */
typedef cusolverStatus_t (*pfn_Create)    (cusolverDnHandle_t *);
typedef cusolverStatus_t (*pfn_SetStream) (cusolverDnHandle_t, cudaStream_t);

typedef cusolverStatus_t (*pfn_DsyevdBuf)(cusolverDnHandle_t, int, int,
    int, const double *, int, const double *, int *);
typedef cusolverStatus_t (*pfn_Dsyevd)   (cusolverDnHandle_t, int, int,
    int, double *, int, double *, double *, int, int *);

typedef cusolverStatus_t (*pfn_SsyevdBuf)(cusolverDnHandle_t, int, int,
    int, const float *, int, const float *, int *);
typedef cusolverStatus_t (*pfn_Ssyevd)   (cusolverDnHandle_t, int, int,
    int, float *, int, float *, float *, int, int *);

/* ── module-level state ── */
static cusolverDnHandle_t g_handle    = NULL;
static pfn_Create         fn_Create   = NULL;
static pfn_SetStream      fn_SetStr   = NULL;
static pfn_DsyevdBuf      fn_DsyBuf  = NULL;
static pfn_Dsyevd         fn_Dsyevd  = NULL;
static pfn_SsyevdBuf      fn_SsyBuf  = NULL;
static pfn_Ssyevd         fn_Ssyevd  = NULL;
static int                g_init      = 0;  /* 0 = uninitialised */

static af_err load_and_init(void)
{
    /* Try the exact versioned name first (already loaded by AF CUDA backend),
     * then fall back to an unversioned symlink if present.              */
    void *lib = dlopen("libcusolver.so.11", RTLD_NOW | RTLD_NOLOAD);
    if (!lib) lib = dlopen("libcusolver.so.11", RTLD_NOW | RTLD_GLOBAL);
    if (!lib) lib = dlopen("libcusolver.so",    RTLD_NOW | RTLD_GLOBAL);
    if (!lib) return AF_ERR_RUNTIME;

    fn_Create  = (pfn_Create)     dlsym(lib, "cusolverDnCreate");
    fn_SetStr  = (pfn_SetStream)  dlsym(lib, "cusolverDnSetStream");
    fn_DsyBuf  = (pfn_DsyevdBuf) dlsym(lib, "cusolverDnDsyevd_bufferSize");
    fn_Dsyevd  = (pfn_Dsyevd)    dlsym(lib, "cusolverDnDsyevd");
    fn_SsyBuf  = (pfn_SsyevdBuf) dlsym(lib, "cusolverDnSsyevd_bufferSize");
    fn_Ssyevd  = (pfn_Ssyevd)    dlsym(lib, "cusolverDnSsyevd");

    if (!fn_Create || !fn_SetStr || !fn_DsyBuf || !fn_Dsyevd ||
        !fn_SsyBuf || !fn_Ssyevd)
        return AF_ERR_RUNTIME;

    if (fn_Create(&g_handle) != CUSOLVER_STATUS_SUCCESS)
        return AF_ERR_INTERNAL;

    /* Bind cuSOLVER to ArrayFire's CUDA stream (device 0) so that
     * cuSOLVER kernels are sequenced correctly with AF operations.  */
    cudaStream_t stream = NULL;
    if (afcu_get_stream(&stream, 0) == AF_SUCCESS && stream)
        fn_SetStr(g_handle, stream);

    return AF_SUCCESS;
}

static af_err ensure_init(void)
{
    if (g_init) return g_handle ? AF_SUCCESS : AF_ERR_RUNTIME;
    g_init = 1;
    return load_and_init();
}

/* ── core eigensolver: writes eigenvectors into d_A, eigenvalues into d_W ── */
static af_err run_syevd(int is_double, int n, void *d_A, void *d_W)
{
    int lwork;
    cusolverStatus_t st;

    if (is_double) {
        st = fn_DsyBuf(g_handle, CUSOLVER_EIG_MODE_VECTOR, CUBLAS_FILL_MODE_LOWER,
                       n, (const double *)d_A, n, (const double *)d_W, &lwork);
    } else {
        st = fn_SsyBuf(g_handle, CUSOLVER_EIG_MODE_VECTOR, CUBLAS_FILL_MODE_LOWER,
                       n, (const float  *)d_A, n, (const float  *)d_W, &lwork);
    }
    if (st != CUSOLVER_STATUS_SUCCESS) return AF_ERR_INTERNAL;

    dim_t wsz = (dim_t)lwork * (is_double ? sizeof(double) : sizeof(float));

    void *d_work = NULL, *d_info = NULL;
    af_err err;
    if ((err = af_alloc_device_v2(&d_work, wsz))          != AF_SUCCESS) return err;
    if ((err = af_alloc_device_v2(&d_info, sizeof(int)))  != AF_SUCCESS) {
        af_free_device_v2(d_work);
        return err;
    }

    if (is_double) {
        st = fn_Dsyevd(g_handle, CUSOLVER_EIG_MODE_VECTOR, CUBLAS_FILL_MODE_LOWER,
                       n, (double *)d_A, n, (double *)d_W,
                       (double *)d_work, lwork, (int *)d_info);
    } else {
        st = fn_Ssyevd(g_handle, CUSOLVER_EIG_MODE_VECTOR, CUBLAS_FILL_MODE_LOWER,
                       n, (float  *)d_A, n, (float  *)d_W,
                       (float  *)d_work, lwork, (int *)d_info);
    }

    af_free_device_v2(d_work);
    af_free_device_v2(d_info);
    return (st == CUSOLVER_STATUS_SUCCESS) ? AF_SUCCESS : AF_ERR_INTERNAL;
}

/* ── public entry point exposed to Haskell ── */
af_err af_eigsh(af_array *evals_out, af_array *evecs_out, const af_array input)
{
    af_err err;

    if ((err = ensure_init()) != AF_SUCCESS) return err;

    af_dtype dtype;
    if ((err = af_get_type(&dtype, input)) != AF_SUCCESS) return err;
    if (dtype != f64 && dtype != f32) return AF_ERR_TYPE;

    dim_t d0, d1, d2, d3;
    if ((err = af_get_dims(&d0, &d1, &d2, &d3, input)) != AF_SUCCESS) return err;
    int n = (int)d0;

    /* Working copy: cuSOLVER overwrites A in-place with eigenvectors */
    af_array evecs;
    if ((err = af_copy_array(&evecs, input)) != AF_SUCCESS) return err;

    /* Eigenvalue output: n-element array owned and managed by ArrayFire */
    af_array evals;
    dim_t n_dim = (dim_t)n;
    if ((err = af_constant(&evals, 0.0, 1, &n_dim, dtype)) != AF_SUCCESS) {
        af_release_array(evecs);
        return err;
    }

    /* Lock both arrays and obtain raw device pointers for cuSOLVER */
    void *d_A = NULL, *d_W = NULL;
    if ((err = af_get_device_ptr(&d_A, evecs)) != AF_SUCCESS) {
        af_release_array(evecs); af_release_array(evals);
        return err;
    }
    if ((err = af_get_device_ptr(&d_W, evals)) != AF_SUCCESS) {
        af_unlock_array(evecs);
        af_release_array(evecs); af_release_array(evals);
        return err;
    }

    err = run_syevd(dtype == f64, n, d_A, d_W);

    /* Unlock: ArrayFire resumes ownership and sees the in-place modifications */
    af_unlock_array(evecs);
    af_unlock_array(evals);

    if (err != AF_SUCCESS) {
        af_release_array(evecs); af_release_array(evals);
        return err;
    }

    *evals_out = evals;
    *evecs_out = evecs;
    return AF_SUCCESS;
}
