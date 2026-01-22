#ifndef _OPENCOG_RAND_GEN_H
#define _OPENCOG_RAND_GEN_H
#include <set>
#include <vector>
#include <opencog/util/exceptions.h>
#include <random>
namespace opencog
{
class RandGen : public std::mt19937
{
public:
virtual ~RandGen() {}
virtual int randint() = 0;
virtual float randfloat() = 0;
virtual double randdouble() = 0;
virtual double randdouble_one_excluded() = 0;
virtual int randint(int n) = 0;
virtual int rand_positive_negative() = 0;
virtual bool randbool() = 0;
virtual int rand_discrete(const std::vector<double>&) = 0;
};
}
#endif