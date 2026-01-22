#ifndef _OPENCOG_LAZY_RANDOM_SELECTOR_H
#define _OPENCOG_LAZY_RANDOM_SELECTOR_H
#include <opencog/util/lazy_selector.h>
#include <opencog/util/RandGen.h>
#include <opencog/util/mt19937ar.h>
namespace opencog
{
struct lazy_random_selector : public lazy_selector {
lazy_random_selector(unsigned int u,
opencog::RandGen& _rng = randGen());
lazy_random_selector(unsigned int u, unsigned int l,
opencog::RandGen& _rng = randGen());
protected:
unsigned int select();
private:
opencog::RandGen& rng;
};
}
#endif