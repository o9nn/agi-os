#ifndef _LG_DICT_ATOMESE_H_
#define _LG_DICT_ATOMESE_H_
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/persist/api/StorageNode.h>
extern "C" {
#include <link-grammar/link-includes.h>
};
using namespace opencog;
link_public_api(void)
lg_config_atomspace(AtomSpacePtr asp, StorageNodePtr stnp);
#endif