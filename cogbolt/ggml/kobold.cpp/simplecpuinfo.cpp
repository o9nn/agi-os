#include <cstdio>
#if defined(_MSC_VER)
#include <intrin.h>
#elif defined(__GNUC__)
#include <cpuid.h>
#endif
bool check_avx_support() {
unsigned int cpuInfo[4] = {0, 0, 0, 0};
#if defined(_MSC_VER)
__cpuid(reinterpret_cast<int*>(cpuInfo), 1);
#elif defined(__GNUC__)
__cpuid(1, cpuInfo[0], cpuInfo[1], cpuInfo[2], cpuInfo[3]);
#endif
return (cpuInfo[2] & (1 << 28)) != 0;
}
bool check_avx2_support() {
unsigned int cpuInfo[4] = {0, 0, 0, 0};
#if defined(_MSC_VER)
__cpuidex(reinterpret_cast<int*>(cpuInfo), 7, 0);
#elif defined(__GNUC__)
__cpuid_count(7, 0, cpuInfo[0], cpuInfo[1], cpuInfo[2], cpuInfo[3]);
#endif
return (cpuInfo[1] & (1 << 5)) != 0;
}
int main() {
int avxSupported = check_avx_support()?1:0;
int avx2Supported = check_avx2_support()?1:0;
printf("{\"avx\":%d, \"avx2\":%d}",avxSupported,avx2Supported);
fflush(stdout);
return 0;
}