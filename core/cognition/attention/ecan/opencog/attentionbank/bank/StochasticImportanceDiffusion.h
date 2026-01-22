#ifndef _OPENCOG_STOCHASTIC_DIFFUSION_H
#define _OPENCOG_STOCHASTIC_DIFFUSION_H
#include <algorithm>
#include <chrono>
#include <vector>
using namespace std::chrono;
namespace opencog
{
class Handle;
class ImportantIndex;
namespace ecan
{
struct DiffusionRecordBin {
unsigned int count = 0;
unsigned int index = 0;
unsigned int size = 0;
float update_rate = 0;
time_point<high_resolution_clock> last_update = high_resolution_clock::now();
};
class StochasticDiffusionAmountCalculator
{
ImportanceIndex* _imidx;
std::vector<DiffusionRecordBin> _bins;
size_t bin_index(const Handle&);
size_t bin_size(unsigned int index);
void update_bin(const Handle&);
public:
StochasticDiffusionAmountCalculator(ImportanceIndex*);
std::vector<DiffusionRecordBin> merge_bins(
const std::vector<DiffusionRecordBin>& past,
std::vector<DiffusionRecordBin>& recent,
float bias);
float diffused_value(const Handle& h, float decay_rate);
float elapsed_time(const Handle& h);
};
}
}
#endif