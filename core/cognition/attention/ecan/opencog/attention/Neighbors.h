#ifndef _OPENCOG_NEIGHBORS_H
#define _OPENCOG_NEIGHBORS_H
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/atom_types/types.h>
namespace opencog
{
HandleSeq get_target_neighbors(const Handle& h, Type desiredLinkType,
bool match_subtype = false);
HandleSeq get_source_neighbors(const Handle& h, Type desiredLinkType,
bool match_subtype = false);
}
#endif