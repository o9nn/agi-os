#ifndef _OPENCOG_MODULE_MANAGER_H
#define _OPENCOG_MODULE_MANAGER_H
#include <map>
#include <string>
#include <vector>
#include <opencog/cogserver/server/Module.h>
namespace opencog
{
class ModuleManager
{
protected:
typedef struct {
Module* module;
std::string id;
std::string filename;
std::string dirpath;
Module::LoadFunction* loadFunction;
Module::UnloadFunction* unloadFunction;
Module::ConfigFunction* configFunction;
void* handle;
} ModuleData;
typedef std::map<const std::string, ModuleData> ModuleMap;
ModuleMap modules;
ModuleData getModuleData(const std::string& id);
std::vector<std::string> module_paths;
bool loadAbsPath(const std::string& filepath, CogServer&);
public:
ModuleManager(void);
~ModuleManager();
bool loadModule(const std::string& filename, CogServer&);
bool unloadModule(const std::string& id);
bool configModule(const std::string& id, const std::string& cfg);
std::string listModules(void);
Module* getModule(const std::string& id);
void loadModules(CogServer&);
};
}
#endif