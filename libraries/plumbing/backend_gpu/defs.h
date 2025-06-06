#ifndef GPU_DEFS_H
#define GPU_DEFS_H

// On Puhti, use UCX_MEMTYPE_CACHE=n with
// GPU_AWARE_MPI

#include <sstream>
#include <iostream>

// Prototypes for memory pool ops
void gpu_memory_pool_alloc(void **p, size_t req_size);
void gpu_memory_pool_free(void *ptr);
void gpu_memory_pool_purge();
void gpu_memory_pool_report();

////////////////////////////////////////////////////////////////////////////////////
// some device rng headers
////////////////////////////////////////////////////////////////////////////////////
namespace hila {
// double random();  // defined in random.h
void seed_device_rng(unsigned long long seed);
} // namespace hila

namespace hila {
void free_device_rng();
} // namespace hila


#ifndef HILAPP

// GPU specific definitions

////////////////////////////////////////////////////////////////////////////////////
// Some cuda-specific definitions
////////////////////////////////////////////////////////////////////////////////////
#if defined(CUDA)

#include <cuda.h>
#include <cuda_runtime.h>
#include <cub/cub.cuh>

using gpuError = cudaError;
#define gpuSuccess cudaSuccess

/////////////////////////////////////////////
// If gpu memory pool in use, the interface to memory
#ifdef GPU_MEMORY_POOL
#define gpuMalloc(a, b) gpu_memory_pool_alloc((void **)a, b)
#define gpuFree(a) gpu_memory_pool_free(a)
#define gpuMemPoolPurge() gpu_memory_pool_purge()
#define gpuMemPoolReport() gpu_memory_pool_report()

#else
// here std interfaces

// clang-format off
#define gpuMemPoolPurge()  do { } while (0)
#define gpuMemPoolReport() do { } while (0)
// clang-format on

#ifdef CUDA_MALLOC_ASYNC
#define gpuMalloc(a, b) GPU_CHECK(cudaMallocAsync(a, b, 0))
#define gpuFree(a) GPU_CHECK(cudaFreeAsync(a, 0))

#else
#define gpuMalloc(a, b) GPU_CHECK(cudaMalloc((void **)a, b))
#define gpuFree(a) GPU_CHECK(cudaFree(a))

#endif

#endif // gpu memory pool
/////////////////////////////////////////////


#define gpuGetLastError cudaGetLastError
#define gpuMemcpy(a, b, c, d) GPU_CHECK(cudaMemcpy(a, b, c, d))
#define gpuMemcpyHostToDevice cudaMemcpyHostToDevice
#define gpuMemcpyDeviceToHost cudaMemcpyDeviceToHost
#define gpuMemcpyDeviceToDevice cudaMemcpyDeviceToDevice
#define gpuDeviceSynchronize() GPU_CHECK(cudaDeviceSynchronize())
#define gpuStreamSynchronize(a) GPU_CHECK(cudaStreamSynchronize(a))
#define gpuStreamCreate(a) GPU_CHECK(cudaStreamCreate(a))
#define gpuStreamDestroy(a) GPU_CHECK(cudaStreamDestroy(a))
#define gpuMemset(a, b, c) GPU_CHECK(cudaMemset(a, b, c))
#define gpuMemcpyToSymbol(a, b, size, c, dir) GPU_CHECK(cudaMemcpyToSymbol(a, b, size, c, dir))
#define gpuFuncAttributes cudaFuncAttributes
#define gpuFuncGetAttributes cudaFuncGetAttributes

#define GPUTYPESTR "CUDA"

#ifdef __CUDA_ARCH__
#define _GPU_DEVICE_COMPILE_ __CUDA_ARCH__
#endif

////////////////////////////////////////////////////////////////////////////////////
// Same for HIP
////////////////////////////////////////////////////////////////////////////////////
#elif defined(HIP)

#include <hip/hip_runtime.h>
#include <hiprand/hiprand.h>

// #include <hipcub/hipcub.hpp>*

using gpuError = hipError_t;
#define gpuSuccess hipSuccess

/////////////////////////////////////////////
// If gpu memory pool in use, the interface to memory
#ifdef GPU_MEMORY_POOL
#define gpuMalloc(a, b) gpu_memory_pool_alloc((void **)a, b)
#define gpuFree(a) gpu_memory_pool_free(a)
#define gpuMemPoolPurge() gpu_memory_pool_purge()
#define gpuMemPoolReport() gpu_memory_pool_report()


#else
// here std interfaces

// clang-format off
#define gpuMemPoolPurge() do {} while (0)
#define gpuMemPoolReport() do {} while (0)
// clang-format on

#define gpuMalloc(a, b) GPU_CHECK(hipMalloc((void **)a, b))
#define gpuFree(a) GPU_CHECK(hipFree(a))

#endif // ifdef memory pool

#define gpuGetLastError hipGetLastError
#define gpuMemcpy(a, b, siz, d) GPU_CHECK(hipMemcpy(a, b, siz, d))
#define gpuMemcpyHostToDevice hipMemcpyHostToDevice
#define gpuMemcpyDeviceToHost hipMemcpyDeviceToHost
#define gpuMemcpyDeviceToDevice hipMemcpyDeviceToDevice
#define gpuDeviceSynchronize() GPU_CHECK(hipDeviceSynchronize())
#define gpuStreamSynchronize(a) GPU_CHECK(hipStreamSynchronize(a))
#define gpuStreamCreate(a) GPU_CHECK(hipStreamCreate(a))
#define gpuStreamDestroy(a) GPU_CHECK(hipStreamDestroy(a))
#define gpuMemset(a, b, c) GPU_CHECK(hipMemset(a, b, c))
#define gpuMemcpyToSymbol(a, b, size, c, dir)                                                      \
    GPU_CHECK(hipMemcpyToSymbol(HIP_SYMBOL(a), b, size, c, dir))
#define gpuFuncAttributes hipFuncAttributes
#define gpuFuncGetAttributes hipFuncGetAttributes


#define GPUTYPESTR "HIP"

#ifdef __HIP_DEVICE_COMPILE__
#define _GPU_DEVICE_COMPILE_ __HIP_DEVICE_COMPILE__
#endif

#endif
////////////////////////////////////////////////////////////////////////////////////
// General GPU (cuda/hip) definitions
////////////////////////////////////////////////////////////////////////////////////


#define GPU_CHECK(cmd)                                                                             \
    do {                                                                                           \
        auto code = cmd;                                                                           \
        gpu_exit_on_error(code, #cmd, __FILE__, __LINE__);                                         \
    } while (0)

#define check_device_error(msg) gpu_exit_on_error(msg, __FILE__, __LINE__)
#define check_device_error_code(code, msg) gpu_exit_on_error(code, msg, __FILE__, __LINE__)
void gpu_exit_on_error(const char *msg, const char *file, int line);
void gpu_exit_on_error(gpuError code, const char *msg, const char *file, int line);

namespace hila {
inline void synchronize_threads() {
    gpuDeviceSynchronize();
}
} // namespace hila

#else // NOW HILAPP

////////////////////////////////////////////////////////////////////////////////////
// Now not cuda or hip - hilapp stage scans this section
///////////////////////////////////////////////////////////////////////////////////


using gpuError = int;

// Define empty stubs - return 1 (true)
// clang-format off
#define gpuMalloc(a, b) do {} while(0)
#define gpuFree(a) do {} while(0)
#define gpuMemcpy(a, b, siz, d) do {} while(0)
#define gpuMemcpyHostToDevice 1
#define gpuMemcpyDeviceToHost 2
#define gpuMemset(a,b,c) do {} while(0)
#define gpuMemcpyToSymbol(a, b, size, c, dir) do {} while(0)

#define gpuMemPoolPurge() do {} while(0)
#define gpuMemPoolReport() do {} while(0)

#define check_device_error(msg) do {} while(0)
#define check_device_error_code(code, msg) do {} while(0)

#define gpuStreamSynchronize(a) do {} while(0)
#define gpuDeviceSynchronize() do {} while(0)

#define gpuGetLastError cudaGetLastError


// clang-format on


#define GPUTYPESTR "NONE"

namespace hila {
inline void synchronize_threads() {}
} // namespace hila

#endif
////////////////////////////////////////////////////////////////////////////////////

void initialize_gpu(int rank, int device);
void gpu_device_info();

// This is not the CUDA compiler
// Maybe hilapp?

namespace hila {

// Implements test for basic in types, similar to
/// std::is_arithmetic, but allows the backend to add
/// it's own basic tyes (such as AVX vectors)
template <class T>
struct is_arithmetic : std::integral_constant<bool, std::is_arithmetic<T>::value> {};

template <class T, class U>
struct is_assignable : std::integral_constant<bool, std::is_assignable<T, U>::value> {};

template <class T>
struct is_floating_point : std::integral_constant<bool, std::is_floating_point<T>::value> {};

} // namespace hila

#endif
