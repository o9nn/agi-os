#ifdef HAVE_CYTHON
#ifndef PYTHONSHELLMODULE_H
#define PYTHONSHELLMODULE_H
#include <opencog/cogserver/shell/PythonShell.h>
#include <opencog/cogserver/server/Request.h>
#include <opencog/cogserver/server/CogServer.h>
namespace opencog
{
class PythonShellModule : public Module
{
private:
DECLARE_CMD_REQUEST(PythonShellModule, "py", shellout,
"Enter the python shell",
"Usage: py [hush|quiet]\n\n"
"Enter the python interpreter shell. This shell provides a rich\n"
"and easy-to-use environment for creating, deleting and manipulating\n"
"OpenCog atoms and truth values.\n\n"
"If 'hush' or 'quiet' is specified after the command, then the prompt\n"
"will not be returned.  This is nice when catting large scripts using\n"
"netcat, as it avoids printing garbage when the scripts work well.\n",
true, false)
DECLARE_CMD_REQUEST(PythonShellModule, "py-eval", do_eval,
"Run a block of python code, and return immediately",
"Usage: py-eval <python code>\n\n"
"Evaluate the specified Python code. It does not need to be quoted.",
false, false)
public:
PythonShellModule(CogServer&);
~PythonShellModule();
static const char *id(void);
virtual void init(void);
virtual bool config(const char*) { return false; }
};
}
#endif
#endif