#include <mutex>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/persist/api/StorageNode.h>
using namespace opencog;
class Local
{
public:
std::mutex dict_mutex;
std::mutex pair_mutex;
bool using_external_as;
AtomSpacePtr asp;
StorageNodePtr stnp;
Handle idanch;
uint64_t last_id;
Handle bany;
Handle prk;
bool enable_sections;
int extra_pairs;
bool extra_any;
Handle miks;
int cost_index;
double cost_cutoff;
double cost_default;
double cost_scale;
double cost_offset;
int pair_disjuncts;
bool pair_with_any;
Dictionary pair_dict;
std::unordered_map<std::string, bool> have_pword;
Handle prp;
Handle mikey;
Handle miformula;
int pair_index;
double pair_cutoff;
double pair_default;
double pair_scale;
double pair_offset;
bool any_disjuncts;
double any_default;
Exp* any_expr;
bool enable_unknown_word;
};
Dictionary create_pair_cache_dict(Dictionary);
const char* ss_add(const char *, Dictionary);
double total_usage_time(void);
bool section_boolean_lookup(Dictionary, const char*);
bool pair_boolean_lookup(Dictionary, const char*);
Dict_node * lookup_section(Dictionary, const Handle& germ);
Exp* make_sect_exprs(Dictionary, const Handle& germ);
Exp* make_cart_pairs(Dictionary, const Handle& germ, Pool_desc*,
const HandleSeq&, int arity, bool any);
Exp* make_any_exprs(Dictionary, Pool_desc*);
void make_dn(Dictionary, Exp*, const char*);
void or_enchain(Pool_desc*, Exp* &orhead, Exp*);
void and_enchain_left(Pool_desc*, Exp* &orhead, Exp* &ortail, Exp*);
void and_enchain_right(Pool_desc*, Exp* &orhead, Exp* &ortail, Exp*);
Handle get_lg_conn(Local*, const Handle& pair);
std::string cached_linkname(Local*, const Handle& pair);
void fetch_link_id(Local*);
void store_link_id(Local*);