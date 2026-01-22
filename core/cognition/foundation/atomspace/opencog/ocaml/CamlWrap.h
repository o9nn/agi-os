#ifndef _CAML_WRAP_H_
#define _CAML_WRAP_H_
#include <caml/mlvalues.h>
#undef Atom
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/base/Handle.h>
using namespace opencog;
extern "C" {
CAMLprim value NewNode(value, Type);
CAMLprim value NewLink(value, Type);
CAMLprim void print_atomspace(void);
CAMLprim value atom_to_sexpr(value);
CAMLprim value atom_string_printer(value);
CAMLprim value execute(value);
CAMLprim value evaluate(value);
}
value tag_to_value(const ValuePtr& pa);
ValuePtr value_to_tag(value);
std::string oc_to_caml_str(const Handle& h,
const std::string& indent=empty_string);
std::string oc_to_caml_str(const ValuePtr& vp,
const std::string& indent=empty_string);
#endif