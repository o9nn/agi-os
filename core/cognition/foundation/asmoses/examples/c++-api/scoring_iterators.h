#ifndef _MOSES_SCORING_ITERATORS_H
#define _MOSES_SCORING_ITERATORS_H
#include <opencog/util/dorepeat.h>
#include <opencog/util/numeric.h>
#include <opencog/asmoses/combo/combo/vertex.h>
namespace opencog { namespace moses {
using namespace opencog::combo;
template<typename T>
struct iterator_function {
typedef T argument_type;
typedef T result_type;
};
struct even_parity : public iterator_function<bool>
{
template<typename It>
bool operator()(It from,It to) const {
bool parity = true;
while (from != to)
parity ^= *from++;
return parity;
}
};
struct disjunction : public iterator_function<bool>
{
template<typename It>
bool operator()(It from,It to) const {
while (from != to)
if (*from++)
return true;
return false;
}
};
struct multiplex : public iterator_function<bool>
{
multiplex(unsigned int n) : arity(n) { }
unsigned int arity;
template<typename It>
bool operator()(It from, It to) const {
unsigned int addr = 0;
for(unsigned int i = 0; i < arity; ++i)
if(*from++)
addr += pow2(i);
return *(from+addr);
}
};
struct majority : public iterator_function<bool>
{
majority(unsigned int n) : arity(n) { }
unsigned int arity;
template<typename It>
bool operator()(It from, It to) const {
return (unsigned int)std::count(from, to, true) > arity / 2;
}
};
struct simple_symbolic_regression : public iterator_function<contin_t>
{
simple_symbolic_regression(int o = 4) : order(o) { }
int order;
template<typename It>
contin_t operator()(It from, It to) const {
contin_t res = 0;
dorepeat(order)
res = (res + 1) * get_contin(*from);
return res;
}
};
}
}
#endif