#ifndef _COMBO_PROCEDURE_REPOSITORY_H
#define _COMBO_PROCEDURE_REPOSITORY_H
#include <opencog/util/exceptions.h>
#include <opencog/util/Logger.h>
#include <opencog/asmoses/combo/combo/procedure_call.h>
#include <opencog/asmoses/combo/combo/vertex.h>
#include <set>
#include <map>
#define LINE_CHAR_MAX 4096
namespace opencog { namespace combo {
typedef std::set<const procedure_call_base*> procedure_call_set;
typedef procedure_call_set::const_iterator procedure_call_set_const_it;
typedef procedure_call_set::iterator procedure_call_set_it;
typedef std::map<std::string, procedure_call_base*> str_proc_map;
typedef str_proc_map::const_iterator str_proc_map_const_it;
typedef str_proc_map::iterator str_proc_map_it;
typedef std::vector<procedure_call_set> strongly_connected_components;
typedef strongly_connected_components::iterator
strongly_connected_components_it;
typedef strongly_connected_components::const_iterator
strongly_connected_components_const_it;
class procedure_repository {
protected:
str_proc_map _repo;
strongly_connected_components _ordered_scc;
std::set<const procedure_call_base*> procedure_call_dependencies(const procedure_call_base* pc) const;
std::set<const procedure_call_base*> procedure_call_dependencies(const std::set<const procedure_call_base*>& pcs) const;
void generate_and_order_strongly_connected_components();
public:
void add(procedure_call_base* pc);
void remove(const std::string& name);
procedure_call instance(const std::string& name) const;
bool does_contain(const std::string& name) const;
void instantiate_procedure_calls(bool warn_on_definite_object=false);
void instantiate_procedure_calls(combo_tree& tr,
bool warn_on_definite_object=false) const;
bool infer_types_repo();
void clear();
std::ostream& toStream(std::ostream& out, bool with_type = false) const;
void print(bool with_type = false) const;
};
template<class BUILTIN_ACTION,
class PERCEPTION,
class ACTION_SYMBOL,
class INDEFINITE_OBJECT>
unsigned int load_procedure_repository(std::istream& in,
combo::procedure_repository& pr,
bool type_checking = false) {
unsigned int n = 0;
while (in.good()) {
while (in.peek()==' ' || in.peek()=='\n' || in.peek()=='\t')
in.get();
if(in.peek()=='#') {
char tmp[LINE_CHAR_MAX];
in.getline(tmp,LINE_CHAR_MAX);
continue;
}
procedure_call pc = load_procedure_call<BUILTIN_ACTION, PERCEPTION,
ACTION_SYMBOL, INDEFINITE_OBJECT>(in, false);
if (!in.good()){
break;
}
if(pc) {
pr.add(const_cast<procedure_call_base*>(pc));
++n;
logger().fine("procedure_repository - Loaded '%s' with arity '%d'.",
pc->get_name().c_str(), pc->arity());
} else {
logger().error("procedure_repository - Error parsing combo function.");
}
}
pr.instantiate_procedure_calls(true);
if(type_checking) {
bool type_check_success = pr.infer_types_repo();
if(!type_check_success) {
logger().error("procedure_repository - Error type checking.");
}
}
return n;
}
std::ostream& operator<<(std::ostream& out, procedure_repository pr);
}}
#endif