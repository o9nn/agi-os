#ifndef _COMBO_BUILTIN_ACTION_H
#define _COMBO_BUILTIN_ACTION_H
#include <iostream>
#include <vector>
#include <opencog/util/exceptions.h>
#include <opencog/asmoses/combo/type_checker/type_tree_def.h>
#include <opencog/asmoses/combo/combo/common_def.h>
#include <opencog/asmoses/combo/combo/operator_base.h>
#define NO_DEFAULT_ACTION_PROPERTY_METHODS
namespace opencog { namespace combo {
class builtin_action_base : public operator_base {
public:
virtual ~builtin_action_base() {}
static const builtin_action_base* get_instance(const std::string& name) {
return NULL;
}
#ifdef NO_DEFAULT_ACTION_PROPERTY_METHODS
virtual bool is_reversible() const = 0;
virtual bool always_succeeds() const = 0;
virtual const builtin_action_base* get_reversal() const = 0;
virtual bool is_idempotent() const = 0;
virtual bool is_additive(arity_t index) const = 0;
virtual bool exists_additive_argument() const = 0;
virtual bool is_zero_neutral(arity_t index) const = 0;
virtual bool exists_zero_neutral_argument() const = 0;
virtual bool is_modulo(arity_t index) const = 0;
virtual double modulo_min(arity_t index) const = 0;
virtual double modulo_max(arity_t index) const = 0;
virtual const std::set<const builtin_action_base*> preconditions() const = 0;
#else
virtual bool is_reversible() const {
return false;
}
virtual bool always_succeeds() const {
return false;
}
virtual const builtin_action_base* get_reversal() const {
return this;
}
virtual bool is_idempotent() const {
return false;
}
virtual bool is_additive(arity_t index) const {
return false;
}
virtual bool is_additive() const {
return false;
}
virtual bool is_zero_neutral(arity_t index) const {
return false;
}
virtual bool is_zero_neutral() const {
return false;
}
virtual bool is_modulo(arity_t index) const {
return false;
}
virtual double modulo_min(arity_t index) const {
return 0.0;
}
virtual double modulo_max(arity_t index) const {
return 0.0;
}
virtual const std::set<const builtin_action_base*> preconditions() const {
static const std::set<const builtin_action_base*> tmp;
return tmp;
}
#endif
};
typedef const builtin_action_base* builtin_action;
typedef std::set<builtin_action> builtin_action_set;
typedef builtin_action_set::iterator builtin_action_set_it;
typedef builtin_action_set::const_iterator builtin_action_set_const_it;
std::ostream& operator<<(std::ostream&, combo::builtin_action);
}
}
#endif