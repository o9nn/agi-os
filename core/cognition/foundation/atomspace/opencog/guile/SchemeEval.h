#ifndef OPENCOG_SCHEME_EVAL_H
#define OPENCOG_SCHEME_EVAL_H
#ifdef HAVE_GUILE
#include <condition_variable>
#include <mutex>
#include <string>
#include <sstream>
#include <cstddef>
#include <libguile.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/eval/GenericEval.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
namespace opencog {
class AtomSpace;
class SchemeEval : public GenericEval
{
private:
void init(void);
static void * c_wrap_init(void *);
void per_thread_init(void);
void finish(void);
static void * c_wrap_finish(void *);
const std::string *_pexpr;
std::string _answer;
void save_rc(SCM);
SCM _rc;
bool _eval_done;
bool _poll_done;
std::mutex _poll_mtx;
std::condition_variable _wait_done;
SCM _pipe;
int _pipeno;
void do_eval(const std::string &);
std::string do_poll_result();
std::string poll_port();
static void * c_wrap_eval(void *);
static void * c_wrap_poll(void *);
SCM _eval_thread;
static void * c_wrap_interrupt(void *);
SCM _outport;
SCM _saved_outport;
bool _in_shell;
bool _in_server;
int _in_redirect;
void capture_port();
void redirect_output();
void restore_output();
void drain_output();
SCM do_scm_eval(SCM, SCM (*)(void *));
static void * c_wrap_eval_v(void *);
static void * c_wrap_eval_as(void *);
ValuePtr _hargs;
ValuePtr _retval;
AtomSpacePtr _retas;
SCM do_apply_scm(const std::string& func, const ValuePtr& varargs);
static void * c_wrap_apply_v(void *);
SCM _scm_error_string;
std::string _error_msg;
SCM _captured_stack;
void set_error_string(SCM);
void set_captured_stack(SCM);
static SCM preunwind_handler_wrapper(void *, SCM, SCM);
static SCM catch_handler_wrapper(void *, SCM, SCM);
SCM preunwind_handler(SCM, SCM);
SCM catch_handler(SCM, SCM);
static std::string prt(SCM);
static void * c_wrap_set_atomspace(void *);
static void * c_wrap_get_atomspace(void *);
AtomSpacePtr _atomspace;
bool _in_eval;
public:
static void init_scheme(void);
static void set_scheme_as(AtomSpace*);
virtual void set_scheme_as(const AtomSpacePtr&);
virtual AtomSpacePtr get_scheme_as(void);
SchemeEval(AtomSpace* = NULL);
SchemeEval(AtomSpacePtr&);
~SchemeEval();
virtual std::string get_name(void) const { return "SchemeEval"; }
static SchemeEval* get_evaluator(AtomSpace*);
static SchemeEval* get_evaluator(const AtomSpacePtr&);
void begin_eval(void);
void eval_expr(const std::string&);
std::string poll_result(void);
void interrupt(void);
std::string eval(const std::string& expr)
{ begin_eval(); eval_expr(expr); return poll_result(); }
std::string eval(const std::stringstream& ss)
{ return eval(ss.str()); }
ValuePtr eval_v(const std::string&);
ValuePtr eval_v(const std::stringstream& ss) { return eval_v(ss.str()); }
Handle eval_h(const std::string& str) { return HandleCast(eval_v(str)); }
Handle eval_h(const std::stringstream& ss) { return eval_h(ss.str()); }
TruthValuePtr eval_tv(const std::string& str) { return TruthValueCast(eval_v(str)); }
TruthValuePtr eval_tv(const std::stringstream& ss) { return eval_tv(ss.str()); }
AtomSpacePtr eval_as(const std::string&);
virtual ValuePtr apply_v(const std::string& func, ValuePtr varargs);
Handle apply(const std::string& func, Handle varargs) {
return HandleCast(apply_v(func, varargs)); }
bool recursing(void) { return _in_eval; }
};
}
extern "C" {
opencog::SchemeEval* get_scheme_evaluator(opencog::AtomSpace*);
};
#endif
#endif