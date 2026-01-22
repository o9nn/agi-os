#ifndef _OPENCOG_EXAMPLE_SCM_H
#define _OPENCOG_EXAMPLE_SCM_H
#include <opencog/guile/SchemeModule.h>
namespace opencog {
class ExampleSCM : public ModuleWrap
{
protected:
virtual void init(void);
public:
ExampleSCM(void);
};
}
extern "C" {
void opencog_example_init(void);
};
#endif