#ifndef KOKKOS_HURD_BRIDGE_H
#define KOKKOS_HURD_BRIDGE_H
#include <cstddef>
#ifdef __cplusplus
extern "C" {
#endif
int kokkos_hurd_initialize(void);
void kokkos_hurd_finalize(void);
int kokkos_hurd_test_parallel(void);
int kokkos_hurd_test_memory(void);
#ifdef __cplusplus
}
namespace HurdCog {
namespace Kokkos {
enum class HurdMemorySpace {
HOST_SPACE,
DEVICE_SPACE,
REMOTE_SPACE
};
enum class HurdExecutionSpace {
SERIAL,
OPENMP,
THREADS,
CUDA,
HIP
};
struct KokkosHurdConfig {
HurdMemorySpace default_memory_space;
HurdExecutionSpace default_execution_space;
bool enable_deep_copy;
bool enable_profiling;
size_t default_team_size;
KokkosHurdConfig() :
default_memory_space(HurdMemorySpace::HOST_SPACE),
default_execution_space(HurdExecutionSpace::SERIAL),
enable_deep_copy(true),
enable_profiling(false),
default_team_size(1) {}
};
}
}
#endif
#endif