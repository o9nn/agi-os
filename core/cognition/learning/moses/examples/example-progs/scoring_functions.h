#ifndef _EXAMPLE_SCORING_FUNCTIONS_H
#define _EXAMPLE_SCORING_FUNCTIONS_H
#include <bitset>
#include <cmath>
#include <boost/lexical_cast.hpp>
#include <opencog/util/exceptions.h>
#include <opencog/util/numeric.h>
#include <opencog/util/RandGen.h>
#include <opencog/util/oc_assert.h>
#include <moses/moses/representation/field_set.h>
using namespace opencog;
using namespace moses;
unsigned int count_bitz(packed_t pack)
{
std::bitset<sizeof(packed_t)> bits(pack);
return bits.count();
}
struct one_max
{
int operator()(const instance& inst) const
{
return accumulate
(make_transform_iterator(inst.begin(),
count_bitz),
make_transform_iterator(inst.end(),
count_bitz), 0);
}
};
struct n_max
{
n_max(const field_set& fs) : fields(fs) {}
int operator()(const instance& inst) const
{
return accumulate(fields.begin_disc(inst), fields.end_disc(inst), 0);
}
const field_set& fields;
};
struct contin_max
{
contin_max(const field_set& fs) : fields(fs) {}
contin_t operator()(const instance& inst) const
{
return accumulate(fields.begin_contin(inst), fields.end_contin(inst),
contin_t(0));
}
const field_set& fields;
};
struct contin_uniform
{
contin_uniform(const field_set& fs, contin_t minval, contin_t maxval)
: fields(fs), target(fs.n_contin_fields())
{
generate(target.begin(), target.end(),
bind(std::plus<contin_t>(),
bind(std::multiplies<contin_t>(),
bind(&RandGen::randdouble, boost::ref(randGen())),
maxval - minval), minval));
}
contin_t operator()(const instance& inst) const
{
contin_t res = 0;
field_set::const_contin_iterator it1 = fields.begin_contin(inst);
for (vector<contin_t>::const_iterator it2 = target.begin();
it2 != target.end();++it1, ++it2)
res -= fabs((*it1) - (*it2));
return res;
}
const field_set& fields;
vector<contin_t> target;
};
struct sphere
{
sphere(const field_set& fs) : fields(fs) {}
contin_t operator()(const instance& inst) const {
contin_t res = 0;
for (field_set::const_contin_iterator it = fields.begin_contin(inst);
it != fields.end_contin(inst);++it) {
contin_t v = *it;
res -= (v * v);
}
return res;
}
const field_set& fields;
};
struct termmax
{
termmax(const field_set& fs) : fields(fs) {}
contin_t operator()(const instance& inst) const
{
contin_t res = 0;
for (field_set::const_term_iterator it = fields.begin_term(inst);
it != fields.end_term(inst);++it) {
term_t s = *it;
OC_ASSERT(s.length() == 2,
"term_t length should be equals to two");
int a = boost::lexical_cast<int>(s[0]);
int b = boost::lexical_cast<int>(s[1]);
res += a + b;
}
return res;
}
const field_set& fields;
};
#endif