#ifndef _EDA_LOGGING_H
#define _EDA_LOGGING_H
#include <algorithm>
#include <iostream>
#include <sstream>
#include <opencog/util/Logger.h>
#include "../representation/field_set.h"
namespace opencog {
namespace moses {
struct cout_log_best_and_gen
{
template<typename It>
void operator()(It from, It to, const field_set& fs, int gen) const
{
if (!logger().is_debug_enabled())
return;
if (from == to)
return;
It best = std::max_element(from, to);
logger().debug("Generation: %d", gen);
std::stringstream ss;
ss << "Best instance: " << best->second << " "
<< fs.to_string(best->first);
logger().debug(ss.str());
}
};
}
}
#endif