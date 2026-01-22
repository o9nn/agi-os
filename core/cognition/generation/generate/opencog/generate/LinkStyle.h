#ifndef _OPENCOG_LINK_STYLE_H
#define _OPENCOG_LINK_STYLE_H
#include <opencog/atomspace/AtomSpace.h>
namespace opencog
{
class LinkStyle
{
protected:
AtomSpace* _scratch;
Handle _point_set;
HandleSeq _mempoints;
HandleSeq _inhsects;
public:
LinkStyle(void);
void clear(void);
Handle create_unique_section(const Handle&);
Handle create_undirected_link(const Handle&, const Handle&,
const Handle&, const Handle&);
size_t num_undirected_links(const Handle&, const Handle&,
const Handle&);
size_t num_any_links(const Handle&, const Handle&);
void save_work(AtomSpace*);
};
}
#endif