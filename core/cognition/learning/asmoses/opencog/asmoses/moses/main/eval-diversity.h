#include <vector>
#include <boost/range/algorithm/transform.hpp>
#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics/stats.hpp>
#include <boost/accumulators/statistics/count.hpp>
#include <boost/accumulators/statistics/mean.hpp>
#include <boost/accumulators/statistics/variance.hpp>
#include <boost/accumulators/statistics/min.hpp>
#include <boost/accumulators/statistics/max.hpp>
#include <opencog/asmoses/utils/iostreamContainer.h>
namespace opencog { namespace moses {
using namespace boost::accumulators;
struct eval_diversity_params
{
std::vector<std::string> input_files,
moses_files;
std::string output_file,
target_feature;
bool display_stats,
display_values;
std::string diversity_dst;
double diversity_p_norm;
};
static const std::string p_norm = "p_norm";
static const std::string tanimoto = "tanimoto";
static const std::string angular = "angular";
typedef accumulator_set<double, stats<tag::count,
tag::mean,
tag::variance,
tag::min,
tag::max>> accumulator_t;
template<typename Out>
Out& ostream_results(Out& out, const eval_diversity_params& edp,
const std::vector<score_t>& dsts)
{
if (edp.display_values)
ostream_container(out, dsts, "\n") << std::endl;
if (edp.display_stats) {
accumulator_t acc;
for (score_t f : dsts) acc(f);
out << "count: " << count(acc) << std::endl;
out << "mean: " << mean(acc) << std::endl;
out << "std dev: " << sqrt(variance(acc)) << std::endl;
out << "min: " << boost::accumulators::min(acc) << std::endl;
out << "max: " << boost::accumulators::max(acc) << std::endl;
}
return out;
}
}
}