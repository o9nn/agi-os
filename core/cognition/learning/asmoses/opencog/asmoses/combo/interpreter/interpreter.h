#ifndef _OPENCOG_INTERPRETER_H
#define _OPENCOG_INTERPRETER_H
#include "opencog/asmoses/combo/combo/vertex.h"
namespace opencog { namespace combo {
struct boolean_interpreter
{
boolean_interpreter(const builtin_seq& inputs=builtin_seq());
builtin operator()(const combo_tree& tr) const;
builtin operator()(const combo_tree::iterator it) const;
virtual builtin boolean_eval(combo_tree::iterator it) const;
protected:
const builtin_seq& boolean_inputs;
};
struct contin_interpreter
{
contin_interpreter(const contin_seq& inputs=contin_seq());
contin_t operator()(const combo_tree& tr) const;
contin_t operator()(const combo_tree::iterator it) const;
virtual contin_t contin_eval(combo_tree::iterator it) const;
protected:
const contin_seq& contin_inputs;
};
struct mixed_interpreter : public boolean_interpreter, public contin_interpreter
{
mixed_interpreter(const vertex_seq& inputs=empty_vertex_seq);
mixed_interpreter(const contin_seq& inputs);
mixed_interpreter(const builtin_seq& inputs);
vertex operator()(const combo_tree& tr) const;
vertex operator()(const combo_tree::iterator it) const;
virtual builtin boolean_eval(combo_tree::iterator it) const;
virtual contin_t contin_eval(combo_tree::iterator it) const;
virtual vertex mixed_eval(combo_tree::iterator it) const;
protected:
bool _use_boolean_inputs;
bool _use_contin_inputs;
const vertex_seq& _mixed_inputs;
};
}}
#endif