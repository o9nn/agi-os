#ifndef _SCORED_INSTANCE_H
#define _SCORED_INSTANCE_H
#include "instance.h"
#include <opencog/util/functional.h>
namespace opencog {
namespace moses {
template<typename ScoreT>
struct scored_instance : public tagged_item<instance, ScoreT>
{
typedef tagged_item<instance, ScoreT> super;
scored_instance(const instance& i, const ScoreT& s) : super(i, s) { }
scored_instance(const instance& i) : super(i) { }
scored_instance() { }
template<class T1, class T2>
scored_instance(const std::pair<T1, T2>& p) : super(p) { }
bool operator<(const scored_instance& other) const
{
return this->second < other.second;
}
bool operator>(const scored_instance& other) const
{
return this->second > other.second;
}
};
}
}
#endif