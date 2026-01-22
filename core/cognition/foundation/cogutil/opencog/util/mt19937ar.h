#ifndef _OPENCOG_MT19937AR_H
#define _OPENCOG_MT19937AR_H
#include <opencog/util/RandGen.h>
#include <random>
namespace opencog
{
class MT19937RandGen : public RandGen
{
public:
MT19937RandGen(result_type s);
int randint();
float randfloat();
double randdouble();
double randdouble_one_excluded();
int randint(int n);
int rand_positive_negative();
bool randbool();
int rand_discrete(const std::vector<double>&);
};
RandGen& randGen();
}
#endif