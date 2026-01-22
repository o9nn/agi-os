#ifndef _OPENCOG_LIBRARAY_MANAGER_H
#define _OPENCOG_LIBRARAY_MANAGER_H
#include <opencog/atoms/base/Handle.h>
#include <opencog/atomspace/AtomSpace.h>
class LibraryManager
{
private:
static std::unordered_map<std::string, void*> _librarys;
static std::unordered_map<std::string, void*> _functions;
public:
static void* getFunc(std::string libName,std::string funcName);
static void setLocalFunc(std::string libName, std::string funcName, void* func);
static void parse_schema(const std::string& schema,
std::string& lang,
std::string& lib,
std::string& fun);
};
namespace opencog
{
void setLocalPredicate(std::string funcName,
TruthValuePtr* (*func)(AtomSpace *, Handle*));
void setLocalSchema(std::string funcName,
Handle* (*func)(AtomSpace *, Handle*));
};
#endif