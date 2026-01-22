#include <opencog/util/Logger.h>
#include <opencog/util/mt19937ar.h>
#include <opencog/util/selection.h>
#include <moses/moses/eda/initialization.h>
#include <moses/moses/eda/local_structure.h>
#include <moses/moses/eda/logging.h>
#include <moses/moses/eda/optimize.h>
#include <moses/moses/eda/replacement.h>
#include <moses/moses/eda/termination.h>
#include "edaopt.h"
#include "scoring_functions.h"
using namespace opencog;
using namespace moses;