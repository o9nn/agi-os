#ifndef _OPENCOG_MOSES_EXEC_H
#define _OPENCOG_MOSES_EXEC_H
#include <string>
#include <vector>
namespace opencog { namespace moses {
int moses_exec(int argc, char** argv);
int moses_exec(const std::vector<std::string>& argv);
int moses_exec(const std::string& argvs);
}
}
#endif