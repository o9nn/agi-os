#ifndef _REPRESENTATION_INSTANCE_H
#define _REPRESENTATION_INSTANCE_H
#include <opencog/asmoses/utils/tree.h>
namespace opencog {
namespace moses {
typedef unsigned long int packed_t;
#define bits_per_packed_t (8*sizeof(packed_t))
typedef double       contin_t;
typedef unsigned     disc_t;
typedef std::string  term_t;
typedef tree<term_t> term_tree;
typedef std::vector<packed_t> instance;
}
}
namespace std
{
template<>
struct hash<opencog::moses::instance>
{
size_t operator()(const opencog::moses::instance& nstc) const noexcept
{
size_t hsh = 0;
for (unsigned long int bs: nstc)
hsh ^= std::hash<unsigned long int>{}(bs)
+ 0x9e3779b9 + (hsh << 6) + (hsh >> 2);
return hsh;
}
};
}
#endif