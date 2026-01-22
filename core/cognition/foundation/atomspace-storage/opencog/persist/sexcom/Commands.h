#ifndef _COMMANDS_H
#define _COMMANDS_H
#include <map>
#include <string>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/persist/proxy/ProxyNode.h>
namespace opencog
{
class Commands
{
protected:
ProxyNodePtr _proxy;
Handle _truth_key;
bool _multi_space;
std::unordered_map<std::string, Handle> _space_map;
AtomSpace* get_opt_as(const std::string&, size_t&);
AtomSpacePtr _base_space;
AtomSpacePtr _top_space;
public:
Commands(void);
~Commands();
void set_base_space(const AtomSpacePtr&);
std::string cog_atomspace(const std::string&);
std::string cog_atomspace_clear(const std::string&);
std::string cog_set_proxy(const std::string&);
std::string cog_proxy_open(const std::string&);
std::string cog_proxy_close(const std::string&);
std::string cog_execute_cache(const std::string&);
std::string cog_get_atoms(const std::string&);
std::string cog_incoming_by_type(const std::string&);
std::string cog_incoming_set(const std::string&);
std::string cog_keys_alist(const std::string&);
std::string cog_link(const std::string&);
std::string cog_node(const std::string&);
std::string cog_value(const std::string&);
std::string cog_extract(const std::string&);
std::string cog_extract_recursive(const std::string&);
std::string cog_set_value(const std::string&);
std::string cog_set_values(const std::string&);
std::string cog_set_tv(const std::string&);
std::string cog_update_value(const std::string&);
std::string cog_define(const std::string&);
std::string cog_ping(const std::string&);
std::string cog_version(const std::string&);
};
}
#endif