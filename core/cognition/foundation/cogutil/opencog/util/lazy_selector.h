#ifndef _OPENCOG_LAZY_SELECTOR_H
#define _OPENCOG_LAZY_SELECTOR_H
#include <unordered_set>
namespace opencog
{
class lazy_selector
{
public:
lazy_selector(unsigned int u, unsigned int l = 0);
virtual ~lazy_selector() {}
bool empty() const;
unsigned int count_n_free() const;
unsigned int operator()();
void reset_range(unsigned int new_u);
void reset_range(unsigned int new_u, unsigned int new_l);
protected:
unsigned int _u;
unsigned int _l;
virtual unsigned int select() = 0;
private:
std::unordered_set<unsigned int> _picked;
inline bool is_free(unsigned int idx) const;
inline void increase_l_till_free();
inline void modify_target(unsigned int src_to, unsigned int dst_to);
};
}
#endif