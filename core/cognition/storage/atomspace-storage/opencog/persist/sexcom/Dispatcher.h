#ifndef _DISPATCHER_H
#define _DISPATCHER_H
#include <functional>
#include <string>
#include <opencog/persist/sexcom/Commands.h>
namespace opencog
{
class AtomSpace;
class Dispatcher
{
public:
typedef std::function<std::string (const std::string&)> Meth;
protected:
Commands _default;
std::unordered_map<size_t, Meth> _dispatch_map;
public:
Dispatcher(void);
~Dispatcher();
void set_base_space(const AtomSpacePtr& asp) {
_default.set_base_space(asp); }
std::string interpret_command(const std::string&);
void install_handler(const std::string&, Meth);
};
}
#endif