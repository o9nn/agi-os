#ifndef _OPENCOG_COMPREHENSION_H
#define _OPENCOG_COMPREHENSION_H
#include <vector>
#include <list>
#include <set>
#include <boost/range/algorithm/transform.hpp>
#include <boost/range/adaptor/filtered.hpp>
#include <opencog/asmoses/utils/functional.h>
namespace opencog {
typedef const_function<bool> const_bool;
static const const_bool default_filter(true);
template<typename Container, typename Function, typename Filter=const_bool>
auto vector_comp(const Container& c, const Function& func,
const Filter& filter=default_filter)
-> std::vector<typename Function::result_type>
{
std::vector<typename Function::result_type> v;
boost::transform(c | boost::adaptors::filtered(filter),
std::back_inserter(v), func);
return v;
}
template<typename Container, typename Function, typename Filter=const_bool>
auto vector_comp(const Container& c, const Function& func,
const Filter& filter=default_filter)
-> std::vector<decltype(func(std::declval<typename Container::value_type>()))>
{
std::vector<decltype(func(std::declval<typename Container::value_type>()))> v;
boost::transform(c | boost::adaptors::filtered(filter),
std::back_inserter(v), func);
return v;
}
template<typename Container, typename Function, typename Filter=const_bool>
auto list_comp(const Container& c, const Function& func,
const Filter& filter=default_filter)
-> std::list<typename Function::result_type>
{
std::list<typename Function::result_type> l;
boost::transform(c | boost::adaptors::filtered(filter),
std::back_inserter(l), func);
return l;
}
template<typename Container, typename Function, typename Filter=const_bool>
auto list_comp(const Container& c, const Function& func,
const Filter& filter=default_filter)
-> std::list<decltype(func(std::declval<typename Container::value_type>()))>
{
std::list<decltype(func(std::declval<typename Container::value_type>()))> l;
boost::transform(c | boost::adaptors::filtered(filter),
std::back_inserter(l), func);
return l;
}
template<typename Container, typename Function, typename Filter = const_bool>
auto set_comp(const Container& c, const Function& func,
const Filter& filter = default_filter)
-> std::set<typename Function::result_type>
{
std::set<typename Function::result_type> s;
boost::transform(c | boost::adaptors::filtered(filter),
std::inserter(s, s.end()), func);
return s;
}
template<typename Container, typename Function, typename Filter = const_bool>
auto set_comp(const Container& c, const Function& func,
const Filter& filter = default_filter)
-> std::set<decltype(func(std::declval<typename Container::value_type>()))>
{
std::set<decltype(func(std::declval<typename Container::value_type>()))> v;
boost::transform(c | boost::adaptors::filtered(filter),
std::inserter(v, v.end()), func);
return v;
}
}
#endif