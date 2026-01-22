#ifndef ASMOSES_CONDLINK_INTERPRETER_H
#define ASMOSES_CONDLINK_INTERPRETER_H
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/asmoses/utils/valueUtils.h>
#include <boost/iterator/zip_iterator.hpp>
namespace opencog { namespace atomese {
struct zip_cond
{
ValueSeq _result;
void operator()(const boost::tuple<const ValuePtr &,
const ValuePtr &, const ValuePtr &> &t);
};
struct zip_cond2
{
std::vector<double> _result;
void operator()(const boost::tuple<const ValuePtr &,
const double &, const double &> &t);
};
ValueSeq condlink_exec_linkvalue(const LinkValuePtr &conds,
const LinkValuePtr &exps,
const LinkValuePtr &default_exp);
std::vector<double> condlink_exec_floatvalue(const LinkValuePtr &conds,
const FloatValuePtr &exps,
const FloatValuePtr &default_exp);
}}
#endif