/*
 * cbits/eigsh.c
 *
 * Symmetric eigendecomposition with backend dispatch:
 *
 *   CUDA  — cusolverDnDsyevd / cusolverDnSsyevd via dlopen/dlsym.
 *           Binds cuSOLVER to ArrayFire's CUDA stream (best-effort).
 *           Uses AF pinned memory for devInfo so convergence failures
 *           (devInfo != 0) are detected and trigger the CPU fallback.
 *           Falls back to the CPU path when cuSOLVER is unavailable.
 *
 *   CPU / OpenCL — Classical Jacobi eigenvalue algorithm on the host.
 *           af_get_data_ptr copies the matrix to host memory; the Jacobi
 *           sweeps diagonalise it in place; af_create_array puts the
 *           results back.  Handles degenerate eigenvalues correctly and
 *           needs no external library.
 *
 * No link-time dependency on the CUDA toolkit or libafcuda.
 */

#define _GNU_SOURCE
#include "arrayfire.h"
#include <dlfcn.h>
#include <math.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

/* ── column-major element access ── */
#define ELEM(a, r, c, n) ((a)[(r) + (size_t)(c) * (n)])

/* ══════════════════════════════════════════════════════════════════════════
 * Jacobi eigenvalue algorithm (host, column-major, real symmetric).
 *
 * On entry  a[n*n] — symmetric matrix.
 * On exit   a[n*n] — eigenvectors as columns.
 *           evals[n] — eigenvalues in the order Jacobi produced them
 *                      (NOT yet sorted).
 * Returns 0 on success, 1 if malloc fails.
 * ══════════════════════════════════════════════════════════════════════════*/

static int jacobi_d(int n, double *a, double *evals)
{
    double *v = malloc((size_t)n * n * sizeof(double));
    if (!v) return 1;

    memset(v, 0, (size_t)n * n * sizeof(double));
    for (int i = 0; i < n; i++) ELEM(v, i, i, n) = 1.0;

    /* Scale-invariant convergence threshold. */
    double amax = 0.0;
    for (int c = 0; c < n; c++)
        for (int r = 0; r < n; r++) {
            double val = fabs(ELEM(a, r, c, n));
            if (val > amax) amax = val;
        }
    double tol = 1e-14 * (amax > 0.0 ? amax : 1.0);

    /* Classical Jacobi performs one rotation per iteration; a sweep is
     * ~n^2/2 rotations and convergence typically needs O(log) sweeps, so
     * 10*n*n rotations is a generous budget. Hitting it means we failed to
     * converge and must report an error rather than silently return
     * inaccurate results (the old cap of 50*n was routinely exhausted for
     * n in the low hundreds). */
    long max_rot   = 10L * n * n + 100;
    int  converged = (n <= 1);
    for (long rot = 0; rot < max_rot; rot++) {
        /* Locate largest off-diagonal element */
        int p = 0, q = 1;
        double max_off = 0.0;
        for (int c = 1; c < n; c++) {
            for (int r = 0; r < c; r++) {
                double val = fabs(ELEM(a, r, c, n));
                if (val > max_off) { max_off = val; p = r; q = c; }
            }
        }
        if (max_off < tol) { converged = 1; break; }

        double apq  = ELEM(a, p, q, n);
        double tau  = (ELEM(a, q, q, n) - ELEM(a, p, p, n)) / (2.0 * apq);
        double sign = (tau >= 0.0) ? 1.0 : -1.0;
        double t    = sign / (fabs(tau) + sqrt(1.0 + tau * tau));
        double cs   = 1.0 / sqrt(1.0 + t * t);
        double sn   = t * cs;

        /* Rotate A */
        ELEM(a, p, p, n) -= t * apq;
        ELEM(a, q, q, n) += t * apq;
        ELEM(a, p, q, n) = ELEM(a, q, p, n) = 0.0;
        for (int r = 0; r < n; r++) {
            if (r == p || r == q) continue;
            double arp = ELEM(a, r, p, n), arq = ELEM(a, r, q, n);
            ELEM(a, r, p, n) = ELEM(a, p, r, n) = cs * arp - sn * arq;
            ELEM(a, r, q, n) = ELEM(a, q, r, n) = cs * arq + sn * arp;
        }
        /* Accumulate rotation in V */
        for (int r = 0; r < n; r++) {
            double vrp = ELEM(v, r, p, n), vrq = ELEM(v, r, q, n);
            ELEM(v, r, p, n) = cs * vrp - sn * vrq;
            ELEM(v, r, q, n) = cs * vrq + sn * vrp;
        }
    }

    for (int i = 0; i < n; i++) evals[i] = ELEM(a, i, i, n);
    memcpy(a, v, (size_t)n * n * sizeof(double));
    free(v);
    return converged ? 0 : 2;
}

static int jacobi_f(int n, float *a, float *evals)
{
    float *v = malloc((size_t)n * n * sizeof(float));
    if (!v) return 1;

    memset(v, 0, (size_t)n * n * sizeof(float));
    for (int i = 0; i < n; i++) ELEM(v, i, i, n) = 1.0f;

    /* Scale-invariant convergence threshold. */
    float amax = 0.0f;
    for (int c = 0; c < n; c++)
        for (int r = 0; r < n; r++) {
            float val = fabsf(ELEM(a, r, c, n));
            if (val > amax) amax = val;
        }
    float tol = 1e-6f * (amax > 0.0f ? amax : 1.0f);

    long max_rot   = 10L * n * n + 100;
    int  converged = (n <= 1);
    for (long rot = 0; rot < max_rot; rot++) {
        int p = 0, q = 1;
        float max_off = 0.0f;
        for (int c = 1; c < n; c++) {
            for (int r = 0; r < c; r++) {
                float val = fabsf(ELEM(a, r, c, n));
                if (val > max_off) { max_off = val; p = r; q = c; }
            }
        }
        if (max_off < tol) { converged = 1; break; }

        float apq  = ELEM(a, p, q, n);
        float tau  = (ELEM(a, q, q, n) - ELEM(a, p, p, n)) / (2.0f * apq);
        float sign = (tau >= 0.0f) ? 1.0f : -1.0f;
        float t    = sign / (fabsf(tau) + sqrtf(1.0f + tau * tau));
        float cs   = 1.0f / sqrtf(1.0f + t * t);
        float sn   = t * cs;

        ELEM(a, p, p, n) -= t * apq;
        ELEM(a, q, q, n) += t * apq;
        ELEM(a, p, q, n) = ELEM(a, q, p, n) = 0.0f;
        for (int r = 0; r < n; r++) {
            if (r == p || r == q) continue;
            float arp = ELEM(a, r, p, n), arq = ELEM(a, r, q, n);
            ELEM(a, r, p, n) = ELEM(a, p, r, n) = cs * arp - sn * arq;
            ELEM(a, r, q, n) = ELEM(a, q, r, n) = cs * arq + sn * arp;
        }
        for (int r = 0; r < n; r++) {
            float vrp = ELEM(v, r, p, n), vrq = ELEM(v, r, q, n);
            ELEM(v, r, p, n) = cs * vrp - sn * vrq;
            ELEM(v, r, q, n) = cs * vrq + sn * vrp;
        }
    }

    for (int i = 0; i < n; i++) evals[i] = ELEM(a, i, i, n);
    memcpy(a, v, (size_t)n * n * sizeof(float));
    free(v);
    return converged ? 0 : 2;
}

/* Selection sort on eigenvalues, mirroring the column swaps in evecs. */
static void sort_eigs_d(int n, double *evals, double *evecs)
{
    for (int i = 0; i < n - 1; i++) {
        int min_j = i;
        for (int j = i + 1; j < n; j++)
            if (evals[j] < evals[min_j]) min_j = j;
        if (min_j == i) continue;
        double tmp = evals[i]; evals[i] = evals[min_j]; evals[min_j] = tmp;
        for (int r = 0; r < n; r++) {
            double tv = evecs[r + (size_t)i * n];
            evecs[r + (size_t)i * n]     = evecs[r + (size_t)min_j * n];
            evecs[r + (size_t)min_j * n] = tv;
        }
    }
}

static void sort_eigs_f(int n, float *evals, float *evecs)
{
    for (int i = 0; i < n - 1; i++) {
        int min_j = i;
        for (int j = i + 1; j < n; j++)
            if (evals[j] < evals[min_j]) min_j = j;
        if (min_j == i) continue;
        float tmp = evals[i]; evals[i] = evals[min_j]; evals[min_j] = tmp;
        for (int r = 0; r < n; r++) {
            float tv = evecs[r + (size_t)i * n];
            evecs[r + (size_t)i * n]     = evecs[r + (size_t)min_j * n];
            evecs[r + (size_t)min_j * n] = tv;
        }
    }
}

/* ══════════════════════════════════════════════════════════════════════════
 * CPU / OpenCL fallback: copy to host, Jacobi, copy back.
 * ══════════════════════════════════════════════════════════════════════════*/
static af_err eigsh_cpu(af_array *evals_out, af_array *evecs_out,
                        const af_array input)
{
    af_dtype dtype;
    af_err err;
    if ((err = af_get_type(&dtype, input)) != AF_SUCCESS) return err;

    dim_t d0, d1, d2, d3;
    if ((err = af_get_dims(&d0, &d1, &d2, &d3, input)) != AF_SUCCESS) return err;
    int n = (int)d0;

    size_t elem_size = (dtype == f64) ? sizeof(double) : sizeof(float);

    void *A = malloc((size_t)n * n * elem_size);
    if (!A) return AF_ERR_NO_MEM;
    void *W = malloc((size_t)n * elem_size);
    if (!W) { free(A); return AF_ERR_NO_MEM; }

    if ((err = af_get_data_ptr(A, input)) != AF_SUCCESS) {
        free(A); free(W); return err;
    }

    int ret = (dtype == f64) ? jacobi_d(n, (double *)A, (double *)W)
                             : jacobi_f(n, (float  *)A, (float  *)W);
    if (ret != 0) {
        free(A); free(W);
        return (ret == 1) ? AF_ERR_NO_MEM : AF_ERR_RUNTIME;
    }

    if (dtype == f64) sort_eigs_d(n, (double *)W, (double *)A);
    else              sort_eigs_f(n, (float  *)W, (float  *)A);

    dim_t dims_eval    = (dim_t)n;
    dim_t dims_evec[2] = { (dim_t)n, (dim_t)n };
    af_array evals = NULL, evecs = NULL;
    if ((err = af_create_array(&evals, W, 1, &dims_eval,  dtype)) != AF_SUCCESS)
        goto cleanup;
    if ((err = af_create_array(&evecs, A, 2,  dims_evec,  dtype)) != AF_SUCCESS) {
        af_release_array(evals);
        goto cleanup;
    }
    free(A); free(W);
    *evals_out = evals;
    *evecs_out = evecs;
    return AF_SUCCESS;

cleanup:
    free(A); free(W);
    return err;
}

/* ══════════════════════════════════════════════════════════════════════════
 * cuSOLVER GPU path (CUDA only).
 * ══════════════════════════════════════════════════════════════════════════*/

/* ── minimal cuSOLVER types (avoids CUDA toolkit headers) ── */
typedef void *cusolverDnHandle_t;
typedef void *af_cuda_stream_t;
typedef int   cusolverStatus_t;

#define CUSOLVER_STATUS_SUCCESS   0
#define CUBLAS_FILL_MODE_LOWER    0
#define CUSOLVER_EIG_MODE_VECTOR  1

typedef cusolverStatus_t (*pfn_Create)    (cusolverDnHandle_t *);
typedef cusolverStatus_t (*pfn_SetStream) (cusolverDnHandle_t, af_cuda_stream_t);
typedef cusolverStatus_t (*pfn_DsyevdBuf)(cusolverDnHandle_t, int, int, int,
    const double *, int, const double *, int *);
typedef cusolverStatus_t (*pfn_Dsyevd)   (cusolverDnHandle_t, int, int, int,
    double *, int, double *, double *, int, int *);
typedef cusolverStatus_t (*pfn_SsyevdBuf)(cusolverDnHandle_t, int, int, int,
    const float *, int, const float *, int *);
typedef cusolverStatus_t (*pfn_Ssyevd)   (cusolverDnHandle_t, int, int, int,
    float *, int, float *, float *, int, int *);
typedef af_err (*pfn_GetStream) (af_cuda_stream_t *, int);

static cusolverDnHandle_t g_handle  = NULL;
static pfn_Create         fn_Create = NULL;
static pfn_SetStream      fn_SetStr = NULL;
static pfn_DsyevdBuf      fn_DsyBuf = NULL;
static pfn_Dsyevd         fn_Dsyevd = NULL;
static pfn_SsyevdBuf      fn_SsyBuf = NULL;
static pfn_Ssyevd         fn_Ssyevd = NULL;
static int                g_init    = 0;

static af_err load_and_init(void)
{
    /* Try versioned sonames (CUDA 11 then 12) then the unversioned symlink. */
    void *lib = dlopen("libcusolver.so.11", RTLD_NOW | RTLD_NOLOAD);
    if (!lib) lib = dlopen("libcusolver.so.11", RTLD_NOW | RTLD_GLOBAL);
    if (!lib) lib = dlopen("libcusolver.so.12", RTLD_NOW | RTLD_GLOBAL);
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

    /* Bind cuSOLVER to AF's CUDA stream so calls are ordered with AF ops. */
    pfn_GetStream fn_GetStr =
        (pfn_GetStream) dlsym(RTLD_DEFAULT, "afcu_get_stream");
    if (fn_GetStr) {
        af_cuda_stream_t stream = NULL;
        if (fn_GetStr(&stream, 0) == AF_SUCCESS && stream)
            fn_SetStr(g_handle, stream);
    }
    return AF_SUCCESS;
}

static af_err ensure_init(void)
{
    if (g_init) return g_handle ? AF_SUCCESS : AF_ERR_RUNTIME;
    g_init = 1;
    return load_and_init();
}

/*
 * run_syevd — call cuSOLVER in-place; overwrites d_A with eigenvectors.
 *
 * devInfo is placed in AF pinned host memory so it is readable from the
 * host after af_sync without a separate cudaMemcpy.  Passing pinned host
 * memory to cuSOLVER is valid under CUDA UVA (CUDA 4.0+ / CC 2.0+).
 * Returns AF_ERR_INTERNAL if the solver signals non-convergence (devInfo != 0).
 */
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
    void *d_work = NULL;
    af_err err;
    if ((err = af_alloc_device_v2(&d_work, wsz)) != AF_SUCCESS) return err;

    /* Pinned host memory — accessible from device via UVA. */
    int *h_info = NULL;
    if ((err = af_alloc_pinned((void **)&h_info, sizeof(int))) != AF_SUCCESS) {
        af_free_device_v2(d_work);
        return err;
    }
    *h_info = 0;

    if (is_double) {
        st = fn_Dsyevd(g_handle, CUSOLVER_EIG_MODE_VECTOR, CUBLAS_FILL_MODE_LOWER,
                       n, (double *)d_A, n, (double *)d_W,
                       (double *)d_work, lwork, h_info);
    } else {
        st = fn_Ssyevd(g_handle, CUSOLVER_EIG_MODE_VECTOR, CUBLAS_FILL_MODE_LOWER,
                       n, (float  *)d_A, n, (float  *)d_W,
                       (float  *)d_work, lwork, h_info);
    }
    af_free_device_v2(d_work);

    if (st != CUSOLVER_STATUS_SUCCESS) {
        af_free_pinned(h_info);
        return AF_ERR_INTERNAL;
    }

    /* Sync so the cuSOLVER kernel's write to h_info is visible on the host. */
    int cur_dev = 0;
    af_get_device(&cur_dev);
    af_sync(cur_dev);

    int devInfo = *h_info;
    af_free_pinned(h_info);
    return (devInfo == 0) ? AF_SUCCESS : AF_ERR_INTERNAL;
}

/* ── public entry point ── */
af_err af_eigsh(af_array *evals_out, af_array *evecs_out, const af_array input)
{
    af_err err;

    af_dtype dtype;
    if ((err = af_get_type(&dtype, input)) != AF_SUCCESS) return err;
    if (dtype != f64 && dtype != f32) return AF_ERR_TYPE;

    dim_t d0, d1, d2, d3;
    if ((err = af_get_dims(&d0, &d1, &d2, &d3, input)) != AF_SUCCESS) return err;
    if (d0 < 1 || d0 != d1 || d2 != 1 || d3 != 1 || d0 > 0x7fffffff)
        return AF_ERR_SIZE;

    af_backend backend;
    if ((err = af_get_active_backend(&backend)) != AF_SUCCESS) return err;

    if (backend != AF_BACKEND_CUDA)
        return eigsh_cpu(evals_out, evecs_out, input);

    if (ensure_init() != AF_SUCCESS)
        return eigsh_cpu(evals_out, evecs_out, input);

    int n = (int)d0;

    af_array evecs;
    if ((err = af_copy_array(&evecs, input)) != AF_SUCCESS) return err;

    af_array evals;
    dim_t n_dim = (dim_t)n;
    if ((err = af_constant(&evals, 0.0, 1, &n_dim, dtype)) != AF_SUCCESS) {
        af_release_array(evecs);
        return err;
    }

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

    af_unlock_array(evecs);
    af_unlock_array(evals);

    if (err != AF_SUCCESS) {
        af_release_array(evecs); af_release_array(evals);
        return eigsh_cpu(evals_out, evecs_out, input);
    }

    *evals_out = evals;
    *evecs_out = evecs;
    return AF_SUCCESS;
}
