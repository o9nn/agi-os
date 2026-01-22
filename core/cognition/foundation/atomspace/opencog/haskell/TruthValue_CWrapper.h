#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include <opencog/atoms/truthvalue/CountTruthValue.h>
#include <opencog/atoms/truthvalue/IndefiniteTruthValue.h>
#include <opencog/atoms/truthvalue/FuzzyTruthValue.h>
#include <opencog/atoms/truthvalue/ProbabilisticTruthValue.h>
extern "C"
{
using namespace opencog;
int TruthValue_getFromAtom( Handle* handle
, char** tv_type
, double* parameters );
int TruthValue_setOnAtom( Handle* handle
, const char* type
, double* parameters );
TruthValuePtr TruthValuePtr_fromRaw(const char* type, double* parameters);
TruthValuePtr* PTruthValuePtr_fromRaw(const char* type, double* parameters);
}