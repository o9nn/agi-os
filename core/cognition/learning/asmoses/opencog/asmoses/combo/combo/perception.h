#ifndef _COMBO_PERCEPTION_H
#define _COMBO_PERCEPTION_H
#include <opencog/util/exceptions.h>
#include <opencog/asmoses/combo/type_checker/type_tree_def.h>
#include <opencog/asmoses/combo/combo/operator_base.h>
#define NO_DEFAULT_PERCEPTION_PROPERTY_METHODS
namespace opencog { namespace combo {
class perception_base : public operator_base {
public:
virtual ~perception_base() {}
static const perception_base* get_instance(const std::string& name) {
return NULL;
}
#ifdef NO_DEFAULT_ACTION_PROPERTY_METHODS
virtual bool is_ultrametric() const = 0;
virtual bool is_transitive() const = 0;
virtual bool is_irreflexive() const = 0;
virtual bool is_reflexive() const = 0;
virtual bool is_symmetric() const = 0;
virtual bool is_identity_of_indiscernibles() const = 0;
#else
virtual bool is_ultrametric() const {
return false;
}
virtual bool is_transitive() const {
return false;
}
virtual bool is_irreflexive() const {
return false;
}
virtual bool is_reflexive() const {
return false;
}
virtual bool is_symmetric() const {
return false;
}
virtual bool is_identity_of_indiscernibles() const {
return false;
}
#endif
};
typedef const perception_base* perception;
typedef std::set<perception> perception_set;
typedef perception_set::iterator perception_set_it;
typedef perception_set::const_iterator perception_set_const_it;
std::ostream& operator<<(std::ostream&, perception);
}
}
#endif