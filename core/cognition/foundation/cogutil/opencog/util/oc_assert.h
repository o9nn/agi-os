#ifndef _OPENCOG_ASSERT_H
#define _OPENCOG_ASSERT_H
#include <opencog/util/exceptions.h>
#ifndef IGNORE_OC_ASSERT
#define OC_ASSERT(cond,...) \
\
\
\
{ bool test = (cond); \
if (not test) opencog::cassert(TRACE_INFO, test, ##__VA_ARGS__); }
#else
#define OC_ASSERT(...) \
((void)0)
#endif
namespace opencog {
void cassert(const char * trace,  bool condition, const char * msg, ...);
void cassert(const char* trace, bool condition, const std::string& msg);
void cassert(const char * trace, bool condition);
}
#endif