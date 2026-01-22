#ifndef _OPENCOG_OC_OMP_H
#define _OPENCOG_OC_OMP_H
#if defined(CYGWIN)
#elif defined(__APPLE__)
#elif HAVE_PARALLEL_STL
#define OC_OMP
#endif
#ifdef OC_OMP
#include <omp.h>
#include <parallel/algorithm>
#define OMP_ALGO __gnu_parallel
#else
#include <algorithm>
#define OMP_ALGO std
#endif
namespace opencog {
void setting_omp(unsigned num_threads, unsigned min_n = 50);
unsigned num_threads();
std::pair<unsigned, unsigned> split_jobs(unsigned n_jobs);
}
#endif