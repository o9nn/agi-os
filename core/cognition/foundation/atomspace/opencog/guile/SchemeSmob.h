#ifdef HAVE_GUILE
#ifndef _OPENCOG_SCHEME_SMOB_H
#define _OPENCOG_SCHEME_SMOB_H
#include <atomic>
#include <map>
#include <string>
#include <vector>
#include <libguile.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
#include <opencog/atomspace/AtomSpace.h>
namespace opencog {
class Atom;
class SchemeSmob
{
friend class SchemeEval;
friend class PrimitiveEnviron;
template<class... Args> friend class SchemeArgConverters;
friend class SchemeReturnConverters;
friend class LoggerSCM;
private:
enum {
COG_PROTOM = 1,
COG_LOGGER,
COG_EXTEND
};
static bool server_mode;
static std::atomic_flag is_inited;
static void module_init(void*);
static void register_procs();
static void register_proc(const char*, int, int, int, scm_t_subr);
static scm_t_bits cog_misc_tag;
static void init_smob_type(void);
static SCM ss_set_server_mode(SCM);
static SCM ss_version(void);
static int print_misc(SCM, SCM, scm_print_state *);
static SCM equalp_misc(SCM, SCM);
static size_t free_misc(SCM);
static bool scm_is_protom(SCM);
static SCM handle_to_scm(const Handle&);
static SCM protom_to_scm(const ValuePtr&);
static Handle scm_to_handle(SCM);
static ValuePtr scm_to_protom(SCM);
static ValuePtr make_value(Type, SCM);
static std::vector<bool> scm_to_bool_list (SCM);
static std::vector<double> scm_to_float_list (SCM);
static std::vector<ValuePtr> scm_to_protom_list (SCM);
static std::vector<std::string> scm_to_string_list (SCM);
static SCM ss_new_value(SCM, SCM);
static SCM ss_new_atom(SCM, SCM);
static SCM ss_new_ast(SCM, SCM);
static Handle h_from_ast(Type, bool, SCM);
static SCM ss_atom(SCM, SCM);
static SCM ss_new_node(SCM, SCM, SCM);
static SCM ss_node(SCM, SCM, SCM);
static SCM ss_new_link(SCM, SCM);
static SCM ss_link(SCM, SCM);
static SCM ss_extract(SCM, SCM);
static SCM ss_extract_recursive(SCM, SCM);
static SCM ss_value_p(SCM);
static SCM ss_atom_p(SCM);
static SCM ss_node_p(SCM);
static SCM ss_link_p(SCM);
static SCM _radix_ten;
static SCM _alist;
static SCM ss_handle(SCM);
static SCM ss_atom_less_p(SCM, SCM);
static SCM ss_equal_p(SCM, SCM);
static SCM ss_value_to_list(SCM);
static SCM ss_value_ref(SCM, SCM, SCM);
static SCM value_ref(const ValuePtr&, size_t);
static SCM ss_set_tv(SCM, SCM);
static SCM ss_set_value(SCM, SCM, SCM);
static SCM set_value(const Handle&, const Handle&,
const ValuePtr&, SCM, const char*);
static SCM ss_set_values(SCM, SCM);
static SCM ss_inc_count(SCM, SCM);
static SCM ss_inc_value(SCM, SCM, SCM, SCM);
static SCM ss_update_value(SCM, SCM, SCM);
static SCM ss_set_value_ref(SCM, SCM, SCM, SCM);
static SCM ss_name(SCM);
static SCM ss_number(SCM);
static SCM from_type(const ValuePtr&);
static SCM ss_type(SCM);
static SCM ss_arity(SCM);
static SCM ss_tv(SCM);
static SCM ss_get_mean(SCM);
static SCM ss_get_confidence(SCM);
static SCM ss_get_count(SCM);
static SCM ss_keys(SCM);
static SCM ss_keys_alist(SCM);
static SCM ss_value(SCM, SCM);
static SCM ss_value_type(SCM, SCM);
static SCM ss_incoming_set(SCM, SCM);
static SCM ss_incoming_size(SCM, SCM);
static SCM ss_incoming_by_type(SCM, SCM, SCM);
static SCM ss_incoming_size_by_type(SCM, SCM, SCM);
static SCM ss_outgoing_set(SCM);
static SCM ss_outgoing_by_type(SCM, SCM);
static SCM ss_outgoing_atom(SCM, SCM);
static SCM ss_map_type(SCM, SCM, SCM);
static SCM ss_get_types(void);
static SCM ss_get_type(SCM);
static SCM ss_get_subtypes(SCM);
static SCM ss_subtype_p(SCM, SCM);
static SCM ss_count(SCM, SCM);
static SCM ss_tv_get_mean(SCM);
static SCM ss_tv_get_confidence(SCM);
static SCM ss_tv_get_count(SCM);
static SCM ss_new_as(SCM);
static SCM ss_add_as(SCM);
static SCM ss_as_p(SCM);
static SCM ss_as(SCM);
static SCM ss_set_as(SCM);
static SCM ss_as_env(SCM);
static SCM ss_as_uuid(SCM);
static SCM ss_as_clear(SCM);
static SCM ss_as_mark_readonly(SCM);
static SCM ss_as_mark_readwrite(SCM);
static SCM ss_as_readonly_p(SCM);
static SCM ss_as_mark_cow(SCM, SCM);
static SCM ss_as_cow_p(SCM);
static SCM make_as(const AtomSpacePtr&);
static const AtomSpacePtr& ss_to_atomspace(SCM);
static SCM ss_get_free_variables(SCM);
static SCM ss_is_closed(SCM);
static SCM convert_to_utf8(void *, SCM, SCM);
static std::string to_string(SCM);
static std::string protom_to_string(SCM);
static std::string protom_to_server_string(SCM);
static std::string misc_to_string(SCM);
static TruthValuePtr get_tv_from_list(SCM);
static const AtomSpacePtr& get_as_from_list(SCM);
static Handle set_values(const Handle&, const AtomSpacePtr&, SCM);
static SCM logger_to_scm(Logger*);
static Logger* ss_to_logger(SCM);
static std::string logger_to_string(const Logger *);
static void release_logger(Logger*);
static Logger* new_logger();
static std::mutex lgr_mtx;
static std::set<Logger*> deleteable_lgr;
[[ noreturn ]] static void throw_exception(const std::exception&,
const char *, SCM);
static AtomSpace* verify_atomspace(SCM, const char *, int pos = 1);
static Type verify_type(SCM, const char *, int pos = 1);
static Handle verify_handle(SCM, const char *, int pos = 1);
static ValuePtr verify_protom(SCM, const char *, int pos = 1);
static TruthValuePtr verify_tv(SCM, const char *, int pos = 1);
static HandleSeq verify_handle_list_msg (SCM, const char*,
int, const char*,  const char*);
static HandleSeq verify_handle_list (SCM, const char *,
int pos = 1);
static std::vector<bool> verify_bool_list (SCM, const char *,
int pos = 1);
static std::vector<double> verify_float_list (SCM, const char *,
int pos = 1);
static std::vector<ValuePtr> verify_protom_list (SCM, const char *,
int pos = 1);
static std::vector<std::string> verify_string_list (SCM, const char *,
int pos = 1);
static std::string verify_string (SCM, const char *, int pos = 1,
const char *msg = "string");
static int verify_int (SCM, const char *, int pos = 1,
const char *msg = "integer");
static size_t verify_size_t (SCM, const char *, int pos = 1,
const char *msg = "integer size_t");
static bool verify_bool (SCM, const char *, int pos = 1,
const char *msg = "boolean");
static double verify_real (SCM, const char *, int pos = 1,
const char *msg = "real number");
static Logger* verify_logger(SCM, const char *, int pos = 1);
static SCM atomspace_fluid;
static void ss_set_env_as(const AtomSpacePtr&);
SchemeSmob();
public:
static void init();
static const AtomSpacePtr& ss_get_env_as(const char *);
};
#define SCM_SMOB_VALUE_PTR_LOC(x) ((ValuePtr*) SCM_SMOB_OBJECT_2_LOC(x))
#define SCM_SMOB_AS_PTR_LOC(x) ((AtomSpacePtr*) SCM_SMOB_OBJECT_2_LOC(x))
}
extern "C" {
void opencog_guile_init(void);
};
#endif
#endif