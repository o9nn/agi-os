#ifndef _JS_COMMANDS_H
#define _JS_COMMANDS_H
#include <string>
namespace opencog
{
class AtomSpace;
class JSCommands
{
public:
static std::string interpret_command(AtomSpace*, const std::string&);
};
}
#endif