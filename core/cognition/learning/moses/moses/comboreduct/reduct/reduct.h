#ifndef _REDUCT_RULE_H
#define _REDUCT_RULE_H
#include <opencog/util/RandGen.h>
#include <moses/comboreduct/combo/vertex.h>
namespace opencog { namespace reduct {
using namespace opencog::combo;
struct rule
{
typedef combo_tree argument_type;
rule(std::string _name) : name(_name) {}
virtual ~rule() {}
virtual void operator()(combo_tree&, combo_tree::iterator) const=0;
virtual rule* clone() const=0;
void operator()(combo_tree& tr) const
{
if (!tr.empty())
(*this)(tr, tr.begin());
}
std::string get_name() const
{
return name;
}
protected:
std::string name;
};
reduct::rule* new_clone(const reduct::rule& r);
template<typename T>
struct crule : public rule
{
crule(std::string _name) : rule(_name) {}
rule* clone() const { return new T(*((T*) this)); }
};
const rule& ann_reduction();
struct logical_reduction
{
logical_reduction();
logical_reduction(const logical_reduction&);
logical_reduction(const vertex_set& ignore_ops);
logical_reduction& operator=(const logical_reduction&);
~logical_reduction();
const rule& operator()(int effort = 2);
private:
void do_init();
const rule* p_medium;
const rule* p_complexe;
public:
static rule* p_extra_simple;
static rule* p_simple;
};
const rule& contin_reduction(int reduct_effort,
const vertex_set& ignore_ops);
const rule& fold_reduction();
const rule& mixed_reduction();
const rule& full_reduction();
const rule& action_reduction();
const rule& perception_reduction();
const rule& clean_reduction();
inline void logical_reduce(int effort, combo_tree& tr,
combo_tree::iterator it,
const vertex_set& ignore_ops)
{
logical_reduction r(ignore_ops);
r(effort)(tr, it);
}
inline void logical_reduce(int effort, combo_tree& tr,
const vertex_set& ignore_ops)
{
logical_reduction r(ignore_ops);
r(effort)(tr);
}
inline void logical_reduce(int effort, combo_tree& tr)
{
logical_reduction r;
r(effort)(tr);
}
inline void contin_reduce(combo_tree& tr, combo_tree::iterator it,
int reduct_effort,
const vertex_set& ignore_ops)
{
contin_reduction(reduct_effort, ignore_ops)(tr, it);
}
inline void contin_reduce(combo_tree& tr,
int reduct_effort,
const vertex_set& ignore_ops)
{
contin_reduction(reduct_effort, ignore_ops)(tr);
}
inline void fold_reduce(combo_tree& tr, combo_tree::iterator it)
{
fold_reduction()(tr, it);
}
inline void fold_reduce(combo_tree& tr)
{
fold_reduction()(tr);
}
inline void mixed_reduce(combo_tree& tr, combo_tree::iterator it)
{
mixed_reduction()(tr, it);
}
inline void mixed_reduce(combo_tree& tr)
{
mixed_reduction()(tr);
}
inline void full_reduce(combo_tree& tr, combo_tree::iterator it)
{
full_reduction()(tr, it);
}
inline void full_reduce(combo_tree& tr)
{
full_reduction()(tr);
}
inline void ann_reduce(combo_tree& tr)
{
ann_reduction()(tr);
}
inline void clean_reduce(combo_tree& tr,combo_tree::iterator it)
{
clean_reduction()(tr, it);
}
inline void clean_reduce(combo_tree& tr)
{
clean_reduction()(tr);
}
inline void clean_and_full_reduce(combo_tree& tr,
combo_tree::iterator it)
{
clean_reduce(tr, it);
full_reduce(tr, it);
}
inline void clean_and_full_reduce(combo_tree& tr)
{
clean_reduce(tr);
full_reduce(tr);
}
inline void action_reduce(combo_tree& tr, combo_tree::iterator it)
{
action_reduction()(tr, it);
}
inline void action_reduce(combo_tree& tr)
{
action_reduction()(tr);
}
inline void perception_reduce(combo_tree& tr, combo_tree::iterator it)
{
perception_reduction()(tr, it);
}
inline void perception_reduce(combo_tree& tr)
{
perception_reduction()(tr);
}
inline void replace_without_changing_it(combo_tree& tr,
combo_tree::iterator dst,
combo_tree::iterator src)
{
*dst = *src;
if (src.is_childless())
tr.erase_children(dst);
else {
tr.replace(dst.begin(), dst.end(), src.begin(), src.end());
}
}
}
}
#endif