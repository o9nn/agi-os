#ifndef _COMBO_ACTION_SYMBOL_H
#define _COMBO_ACTION_SYMBOL_H
#include <opencog/asmoses/combo/combo/operator_base.h>
#include <opencog/asmoses/combo/type_checker/type_tree_def.h>
namespace opencog { namespace combo {
class action_symbol_base : public operator_base {
public:
virtual ~action_symbol_base() {}
static const action_symbol_base* get_instance(const std::string& name) {
return NULL;
}
};
typedef const action_symbol_base* action_symbol;
std::ostream& operator<<(std::ostream&, combo::action_symbol);
}
}
#endif