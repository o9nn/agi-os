#ifndef _OPENCOG_REPRESENTATION_PARAMETERS_H
#define _OPENCOG_REPRESENTATION_PARAMETERS_H
#include <opencog/asmoses/combo/combo/vertex.h>
namespace opencog { namespace moses {
static const combo::operator_set empty_ignore_ops = combo::operator_set();
enum class knob_probing_enum {
kp_auto,
kp_on,
kp_off
};
static inline knob_probing_enum parse_knob_probing(std::string& kp_str)
{
knob_probing_enum kp = knob_probing_enum::kp_auto;
if (kp_str == "auto")
kp = knob_probing_enum::kp_auto;
else if (kp_str == "1" or kp_str == "on")
kp = knob_probing_enum::kp_on;
else if (kp_str == "0" or kp_str == "off")
kp = knob_probing_enum::kp_off;
else
OC_ASSERT(false, "Knob probing option %s not supported",
kp_str.c_str());
return kp;
}
struct representation_parameters
{
representation_parameters(reduct::rule* opt_red=NULL,
reduct::rule* rep_red=NULL,
const combo::operator_set& igops=empty_ignore_ops,
knob_probing_enum kp=knob_probing_enum::kp_auto,
bool linc=false,
float permr=0.0,
const combo::combo_tree_ns_set* prcts=nullptr,
const combo::combo_tree_ns_set* acts=nullptr)
: opt_reduct(opt_red),
rep_reduct(rep_red),
ignore_ops(igops),
knob_probing(kp),
linear_contin(linc),
perm_ratio(permr),
perceptions(prcts),
actions(acts)
{}
const reduct::rule* opt_reduct;
const reduct::rule* rep_reduct;
combo::operator_set ignore_ops;
knob_probing_enum knob_probing;
bool linear_contin;
float perm_ratio;
const combo::combo_tree_ns_set* perceptions;
const combo::combo_tree_ns_set* actions;
};
}
}
#endif