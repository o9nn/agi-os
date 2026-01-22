#ifndef _OPENCOG_INTERPRETER_H
#define _OPENCOG_INTERPRETER_H
#include "../combo/vertex.h"
namespace opencog { namespace combo {
struct boolean_interpreter
{
boolean_interpreter(const std::vector<builtin>& inputs = std::vector<builtin>());
builtin operator()(const combo_tree& tr) const;
builtin operator()(const combo_tree::iterator it) const;
virtual builtin boolean_eval(combo_tree::iterator it) const;
protected:
const std::vector<builtin>& boolean_inputs;
};
struct contin_interpreter
{
contin_interpreter(const std::vector<contin_t>& inputs = std::vector<contin_t>());
contin_t operator()(const combo_tree& tr) const;
contin_t operator()(const combo_tree::iterator it) const;
virtual contin_t contin_eval(combo_tree::iterator it) const;
protected:
const std::vector<contin_t>& contin_inputs;
};
struct mixed_interpreter : public boolean_interpreter, public contin_interpreter
{
mixed_interpreter(const std::vector<vertex>& inputs=empty_vertex_seq);
mixed_interpreter(const std::vector<contin_t>& inputs);
mixed_interpreter(const std::vector<builtin>& inputs);
vertex operator()(const combo_tree& tr) const;
vertex operator()(const combo_tree::iterator it) const;
virtual builtin boolean_eval(combo_tree::iterator it) const;
virtual contin_t contin_eval(combo_tree::iterator it) const;
virtual vertex mixed_eval(combo_tree::iterator it) const;
protected:
bool _use_boolean_inputs;
bool _use_contin_inputs;
const std::vector<vertex>& _mixed_inputs;
};
}}
#endif