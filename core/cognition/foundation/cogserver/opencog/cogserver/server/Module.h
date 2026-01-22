#ifndef _OPENCOG_MODULE_H
#define _OPENCOG_MODULE_H
namespace opencog
{
#define DECLARE_MODULE(MODNAME) \
\
extern "C" const char* opencog_module_id(void) { \
return #MODNAME; \
} \
extern "C" Module * opencog_module_load(CogServer& cogserver) { \
return new MODNAME(cogserver); \
} \
extern "C" void opencog_module_unload(Module* m) { \
delete m; \
} \
extern "C" bool opencog_module_config(Module* m, const char* s) { \
return m->config(s); \
} \
inline const char * MODNAME::id(void) { \
return #MODNAME; \
}
class CogServer;
class Module
{
public:
static const char* id_function_name(void)
{
static const char* s = "opencog_module_id";
return s;
}
static const char* load_function_name(void)
{
static const char* s = "opencog_module_load";
return s;
}
static const char* unload_function_name(void)
{
static const char* s = "opencog_module_unload";
return s;
}
static const char* config_function_name(void)
{
static const char* s = "opencog_module_config";
return s;
}
typedef const char* IdFunction (void);
typedef Module* LoadFunction (CogServer&);
typedef void UnloadFunction(Module*);
typedef bool ConfigFunction(Module*, const char*);
Module(CogServer& cs) : _cogserver(cs) {}
virtual ~Module() {}
virtual void init() = 0;
virtual bool config(const char *) = 0;
protected:
CogServer& _cogserver;
};
}
#endif