#ifndef _COMBO_PROCEDURE_CALL_H
#define _COMBO_PROCEDURE_CALL_H
#include <iostream>
#include <vector>
#include <cassert>
#include <opencog/util/exceptions.h>
#include <opencog/util/Logger.h>
#include <moses/comboreduct/combo/common_def.h>
#include <moses/comboreduct/combo/operator_base.h>
#include <moses/comboreduct/combo/vertex.h>
#include <moses/comboreduct/combo/iostream_combo.h>
#include <moses/comboreduct/combo/../type_checker/type_tree_def.h>
namespace opencog { namespace combo {
class procedure_call_base : public operator_base
{
protected:
std::string _name;
type_tree _type_tree;
arity_t _arity;
type_tree _output_type;
type_tree_seq _arg_types;
combo_tree _body;
public:
procedure_call_base(const std::string& name,
arity_t arity,
const combo_tree& tr,
bool infer_type = false);
virtual ~procedure_call_base();
const std::string& get_name() const;
const type_tree& get_type_tree() const;
void set_type_tree(const type_tree& tt);
arity_t arity() const;
type_tree get_output_type_tree() const;
const type_tree& get_input_type_tree(arity_t i) const;
const combo_tree& get_body() const;
combo_tree& get_mutable_body();
std::ostream& toStream(std::ostream& out, bool complete = false) const;
};
bool operator==(const procedure_call_base& pc1,
const procedure_call_base& pc2);
bool operator!=(const procedure_call_base& pc1,
const procedure_call_base& pc2);
template < class BUILTIN_ACTION,
class PERCEPTION,
class ACTION_SYMBOL,
class INDEFINITE_OBJECT >
procedure_call load_procedure_call(std::istream& in,
bool infer_type = false)
{
using namespace std;
string str, tmp;
int nparen = 0;
do {
in >> tmp;
nparen += count(tmp.begin(), tmp.end(), '(')
- count(tmp.begin(), tmp.end(), ')');
str += tmp + ' ';
tmp.assign("");
} while (in.good() && nparen > 0);
if (nparen != 0) {
logger().error("procedure_call - Mismatched parenthesis in the arity definition procedure '%s'",
str.c_str());
return NULL;
}
if (!in.good()) return NULL;
in >> tmp;
if (tmp != ":=" || !in.good()) {
logger().error("procedure_call - Wrong procedure definition operator '%s' in procedure definition '%s' should be ':=' instead",
tmp.c_str(), str.c_str());
return NULL;
}
string body;
do {
in >> tmp;
nparen += count(tmp.begin(), tmp.end(), '(')
- count(tmp.begin(), tmp.end(), ')');
body += tmp + ' ';
tmp.assign("");
} while (in.good() && nparen > 0);
if (nparen != 0) {
logger().error("procedure_call - Mismatched parenthesis in the body of procedure '%s'. The total of parenthesis, with '(' counting for 1 and ')' counting for -1, sums up to %d",
str.c_str(), nparen);
return NULL;
}
string::size_type lparen = str.find('('), rparen = str.find(')');
if (lparen == string::npos || rparen == string::npos || lparen > rparen) {
return NULL;
}
string name = str.substr(0, lparen);
int arity;
string arity_str = str.substr(lparen + 1, rparen - lparen - 1);
try {
arity = boost::lexical_cast<int>(arity_str);
} catch (...) {
logger().error("procedure_call - Lexical error: '%s'"
" supposed to be an arity in procedure"
" definition '%s' does not correspond to"
" a number",
arity_str.c_str(), str.c_str());
return NULL;
}
combo_tree tr;
stringstream ss(body);
stream_to_combo_tree< BUILTIN_ACTION, PERCEPTION,
ACTION_SYMBOL, INDEFINITE_OBJECT > (ss, tr);
for(combo_tree::iterator it = tr.begin(); it != tr.end(); ++it) {
if(is_argument(*it)) {
const argument& arg = get_argument(*it);
if(!arg.is_idx_valid(arity)) {
stringstream arg_ss;
arg_ss << arg;
logger().error("procedure_call - Semantic error:"
" the procedure '%s' has arity '%d'"
" but contains variable argument '%s'"
" out of range",
str.c_str(), arity, arg_ss.str().c_str());
return NULL;
}
}
}
return new procedure_call_base(name, arity, tr, infer_type);
}
std::ostream& operator<<(std::ostream&, const procedure_call_base&);
std::ostream& operator<<(std::ostream&, procedure_call);
}
}
#endif