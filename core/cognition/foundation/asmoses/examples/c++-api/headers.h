#include <opencog/util/Logger.h>
#include <opencog/util/mt19937ar.h>
#include <opencog/util/selection.h>
#include <opencog/asmoses/moses/eda/initialization.h>
#include <opencog/asmoses/moses/eda/local_structure.h>
#include <opencog/asmoses/moses/eda/logging.h>
#include <opencog/asmoses/moses/eda/optimize.h>
#include <opencog/asmoses/moses/eda/replacement.h>
#include <opencog/asmoses/moses/eda/termination.h>
#include "edaopt.h"
#include "scoring_functions.h"
using namespace opencog;
using namespace moses;