#include <opencog/atomspace/AtomSpace.h>
extern "C"
{
using namespace opencog;
int FloatValue_toRaw(FloatValuePtr ptr
, char** valuetype
, double* parameters);
int FloatValue_getFromAtom( Handle* atom
, Handle* key
, char** valuetype
, double* parameters );
int FloatValue_setOnAtom( Handle* atom
, Handle* key
, const char* valuetype
, double* parameters
, int length);
}