#ifndef _OPENCOG_SCHEME_MODULE_H
#define _OPENCOG_SCHEME_MODULE_H
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
namespace opencog {
class AtomSpace;
class FunctionWrap
{
private:
Handle (*_func_h_ah)(AtomSpace*, const Handle&);
Handle (*_func_h_ahz)(AtomSpace*, const Handle&, size_t);
Handle as_wrapper_h_h(Handle);
Handle as_wrapper_h_hz(Handle, size_t);
TruthValuePtr (*_pred_ah)(AtomSpace*, const Handle&);
TruthValuePtr as_wrapper_p_h(Handle);
ValuePtr (*_proto_ah)(AtomSpace*, const Handle&);
ValuePtr as_wrapper_v_h(Handle);
const char *_name;
public:
FunctionWrap(Handle (*)(AtomSpace*, const Handle&),
const char*, const char*);
FunctionWrap(Handle (*)(AtomSpace*, const Handle&, size_t),
const char*, const char*);
FunctionWrap(TruthValuePtr (*)(AtomSpace*, const Handle&),
const char*, const char*);
FunctionWrap(ValuePtr (*)(AtomSpace*, const Handle&),
const char*, const char*);
};
class ModuleWrap
{
private:
static void* init_in_guile(void*);
static void init_in_module(void*);
const char* _modname;
protected:
virtual void init(void) = 0;
public:
ModuleWrap(const char*);
void module_init(void);
virtual ~ModuleWrap();
};
}
#endif