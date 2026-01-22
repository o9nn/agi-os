#ifndef _MOSES_LOCAL_MOSES_H
#define _MOSES_LOCAL_MOSES_H
#include "../deme/deme_expander.h"
#include "../metapopulation/metapopulation.h"
#include "moses_params.h"
namespace opencog {
namespace moses {
using namespace combo;
void local_moses(metapopulation&,
deme_expander&,
const moses_parameters&,
moses_statistics&);
}
}
#endif