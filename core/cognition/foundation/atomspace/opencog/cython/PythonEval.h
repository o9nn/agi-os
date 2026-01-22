#ifndef OPENCOG_PYTHON_EVAL_H
#define OPENCOG_PYTHON_EVAL_H
#ifdef HAVE_CYTHON
#include "PyIncludeWrapper.h"
#include <condition_variable>
#include <filesystem>
#include <map>
#include <mutex>
#include <string>
#include <vector>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/eval/GenericEval.h>
namespace opencog {
class AtomSpace;
class PythonEval : public GenericEval
{
private:
void initialize_python_objects_and_imports(void);
void import_module(const std::filesystem::path &file,
PyObject* pyFromList);
void add_module_directory(const std::filesystem::path &directory);
void add_module_file(const std::filesystem::path &file);
void add_modules_from_path(std::string path);
void add_modules_from_abspath(std::string path);
void add_to_sys_path(std::string path);
PyObject * atomspace_py_object(AtomSpacePtr);
void print_dictionary(PyObject*);
PyObject* find_object(PyObject* pyModule,
const std::string& objectName);
PyObject* get_function(const std::string& moduleFunction);
PyObject* do_call_user_function(const std::string& moduleFunction,
PyObject* pyArguments);
PyObject* call_user_function(const std::string& func,
Handle varargs);
std::string build_python_error_message(const std::string&);
std::string execute_string(const char*);
std::string execute_script(const std::string&);
std::string exec_wrap_stdout(const std::string&);
static PythonEval* singletonInstance;
static std::recursive_mutex _mtx;
bool _eval_done;
std::mutex _poll_mtx;
std::mutex _eval_mutex;
std::condition_variable _wait_done;
PyObject* _pyGlobal;
PyObject* _pyLocal;
PyObject* _pyRootModule;
PyObject* _pySysPath;
std::map <std::string, PyObject*> _modules;
std::string _result;
std::string _capture_stdout;
int _paren_count;
void eval_expr_line(const std::string&);
bool check_for_error();
public:
PythonEval();
~PythonEval();
virtual std::string get_name(void) const { return "PythonEval"; }
static void create_singleton_instance();
static void delete_singleton_instance();
static PythonEval & instance();
virtual void begin_eval(void);
virtual void eval_expr(const std::string&);
virtual std::string poll_result(void);
virtual void interrupt(void);
std::string eval(const std::string& expr)
{
std::lock_guard<std::mutex> lock(_eval_mutex);
begin_eval(); eval_expr(expr); return poll_result();
}
virtual ValuePtr apply_v(AtomSpace * as, const std::string& func,
Handle varargs);
Handle apply(AtomSpace * as, const std::string& func,
Handle varargs)
{ return HandleCast(apply_v(as, func, varargs)); }
TruthValuePtr apply_tv(AtomSpace *as,
const std::string& func, Handle varargs)
{ return TruthValueCast(apply_v(as, func, varargs)); }
void apply_as(const std::string& func, AtomSpace*);
#if 0
void print_root_dictionary()
{
printf("The root dictionary is:\n");
this->print_dictionary(PyModule_GetDict(_pyRootModule));
}
#endif
};
void global_python_initialize();
void global_python_finalize();
extern "C" {
opencog::PythonEval* get_python_evaluator(opencog::AtomSpace*);
};
}
#endif
#endif