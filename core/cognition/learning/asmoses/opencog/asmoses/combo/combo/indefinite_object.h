#ifndef _COMBO_INDEFINITE_OBJECT_H
#define _COMBO_INDEFINITE_OBJECT_H
#include <opencog/util/exceptions.h>
#include <opencog/asmoses/combo/type_checker/type_tree_def.h>
#include <opencog/asmoses/combo/combo/operator_base.h>
namespace opencog { namespace combo {
class indefinite_object_base : public operator_base {
public:
virtual ~indefinite_object_base() {}
static const indefinite_object_base* get_instance(const std::string& name) {
return NULL;
}
};
typedef const indefinite_object_base* indefinite_object;
typedef std::set<indefinite_object> indefinite_object_set;
typedef indefinite_object_set::iterator indefinite_object_set_it;
typedef indefinite_object_set::const_iterator indefinite_object_set_const_it;
std::ostream& operator<<(std::ostream&, indefinite_object);
}
}
#endif