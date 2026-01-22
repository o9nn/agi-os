#ifndef ATTENTION_PARAM_CONFIG_H
#define ATTENTION_PARAM_CONFIG_H
#include <sstream>
#include <string>
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atomspace/AtomSpace.h>
namespace opencog
{
class AttentionParamQuery
{
private:
AtomSpace * _as;
Handle parent_param;
Handle hget_params;
public:
static const std::string af_size;
static const std::string af_decay;
static const std::string af_bottom;
static const std::string af_min_size;
static const std::string af_max_size;
static const std::string af_rent_update_freq;
static const std::string forg_forgetting_threshold;
static const std::string heb_maxlink;
static const std::string heb_max_alloc_percentage;
static const std::string heb_local_farlink_ratio;
static const std::string dif_spread_percentage;
static const std::string dif_spread_hebonly;
static const std::string dif_tournament_size;
static const std::string spreading_filter;
static const std::string rent_starting_sti_rent;
static const std::string rent_starting_lti_rent;
static const std::string rent_target_sti_funds;
static const std::string rent_sti_funds_buffer;
static const std::string rent_target_lti_funds;
static const std::string rent_lti_funds_buffer;
static const std::string rent_tournament_size;
AttentionParamQuery(AtomSpace* as);
void load_default_values(void);
std::string get_param_value(std::string param);
Handle get_param_hvalue(std::string param);
HandleSeq get_params(void);
template<class T>
void set_param(std::string param_name, T value)
{
Handle param = _as->add_node(CONCEPT_NODE,
std::move(param_name));
Handle member_link = _as->add_link(MEMBER_LINK,
param, parent_param);
std::ostringstream sstream;
sstream << value;
Handle hvalue = _as->add_node(NUMBER_NODE,
sstream.str());
_as->add_link(STATE_LINK, param, hvalue);
}
void set_param(std::string param_name, const Handle& hvalue)
{
Handle param = _as->add_node(CONCEPT_NODE,
std::move(param_name));
Handle member_link = _as->add_link(MEMBER_LINK,
param, parent_param);
_as->add_link(STATE_LINK, param, hvalue);
}
};
}
#endif