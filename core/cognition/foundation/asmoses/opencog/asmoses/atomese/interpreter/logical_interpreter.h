#ifndef MOSES_LOGICAL_INTERPRETER_H
#define MOSES_LOGICAL_INTERPRETER_H
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/asmoses/utils/valueUtils.h>
#include <boost/iterator/zip_iterator.hpp>
namespace opencog { namespace atomese {
struct zip_and
{
ValueSeq _result;
void operator()(const boost::tuple<const ValuePtr&, const ValuePtr&>& t);
};
struct zip_or
{
ValueSeq _result;
void operator()(const boost::tuple<const ValuePtr&, const ValuePtr&>& t);
};
struct zip_greater_than
{
ValueSeq _result;
void operator()(const boost::tuple<const double&, const double&>& t);
};
LinkValuePtr logical_and(const LinkValuePtr& p1, const LinkValuePtr& p2);
LinkValuePtr logical_or(const LinkValuePtr& p1, const LinkValuePtr& p2);
bool logical_compare(const LinkValuePtr& p1, const LinkValuePtr& p2);
LinkValuePtr logical_not(const LinkValuePtr& p);
LinkValuePtr greater_than(const FloatValuePtr& p1, const FloatValuePtr& p2);
}}
#endif