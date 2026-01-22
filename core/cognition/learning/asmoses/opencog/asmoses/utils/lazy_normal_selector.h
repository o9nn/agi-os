#ifndef _OPENCOG_LAZY_NORMAL_SELECTOR_H
#define _OPENCOG_LAZY_NORMAL_SELECTOR_H
#include <opencog/util/lazy_selector.h>
#include <opencog/util/oc_assert.h>
namespace opencog
{
struct lazy_normal_selector : public lazy_selector {
lazy_normal_selector(unsigned int n, unsigned int s = 0) :
lazy_selector(n), _s(s) {
OC_ASSERT(s < n);
}
protected:
unsigned int select();
private:
unsigned int _s;
};
}
#endif