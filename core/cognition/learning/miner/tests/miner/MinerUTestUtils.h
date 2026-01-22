#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemeEval.h>
#include <opencog/miner/HandleTree.h>
#include <opencog/miner/Miner.h>
namespace opencog {
class MinerUTestUtils
{
public:
static Handle add_db_cpt(AtomSpace& as);
static Handle add_minsup_prd(AtomSpace& as);
static Handle add_surp_prd(AtomSpace& as, std::string mode);
static Handle add_top(AtomSpace& as);
static Handle add_minsup_eval(AtomSpace& as,
const Handle& pattern,
int minsup,
TruthValuePtr tv=TruthValue::DEFAULT_TV());
static Handle add_minsup_evals(AtomSpace& as,
const HandleSeq& patterns,
int minsup,
TruthValuePtr tv=TruthValue::DEFAULT_TV());
static Handle add_surp_eval(AtomSpace& as,
const std::string& mode,
const Handle& pattern);
static Handle get_pattern(const Handle& minsup_eval);
static HandleSeq get_patterns(const HandleSeq& minsup_evals);
static Handle add_abs_true_eval(AtomSpace& as, const Handle& h);
static Handle add_nconjunct(AtomSpace& as, unsigned n);
static Handle add_variable(AtomSpace& as,
const std::string& prefix,
int i);
static HandleSeq add_variables(AtomSpace& as,
const std::string& prefix,
int n);
static Handle ure_pm(AtomSpace& as,
SchemeEval& scm,
const Handle& pm_rb,
const AtomSpace& db_as,
int minsup,
int max_iter=-1,
Handle initpat=Handle::UNDEFINED,
bool conjunction_expansion=false,
unsigned max_conjuncts=UINT_MAX,
unsigned max_variables=UINT_MAX,
unsigned max_spcial_conjuncts=1,
unsigned max_cnjexp_variables=UINT_MAX,
bool enforce_specialization=true,
double complexity_penalty=0.0,
bool enable_type=false,
bool enable_glob=false,
std::vector<std::string> ignore_vars={});
static Handle ure_pm(AtomSpace& as,
SchemeEval& scm,
const Handle& pm_rb,
const HandleSeq& db, int minsup,
int max_iter=-1,
Handle initpat=Handle::UNDEFINED,
bool conjunction_expansion=false,
unsigned max_conjuncts=UINT_MAX,
unsigned max_variables=UINT_MAX,
unsigned max_spcial_conjuncts=1,
unsigned max_cnjexp_variables=UINT_MAX,
bool enforce_specialization=true,
double complexity_penalty=0.0,
bool enable_type=false,
bool enable_glob=false,
std::vector<std::string> ignore_vars={});
static HandleTree cpp_pm(const AtomSpace& db_as,
int minsup=1,
int conjuncts=1,
const Handle& initpat=Handle::UNDEFINED,
int maxdepth=-1);
static HandleTree cpp_pm(const HandleSeq& db,
int minsup=1,
int conjuncts=1,
const Handle& initpat=Handle::UNDEFINED,
int maxdepth=-1);
static Handle add_is_cpt_pattern(AtomSpace& as, const Handle& cpt);
static Handle add_ugly_pattern(AtomSpace& as);
static Handle add_man_pattern(AtomSpace& as);
static Handle add_soda_drinker_pattern(AtomSpace& as);
static Handle add_ugly_man_pattern(AtomSpace& as);
static Handle add_ugly_man_soda_drinker_pattern(AtomSpace& as);
static void configure_mandatory_rules(SchemeEval& scm);
static void configure_optional_rules(SchemeEval& scm,
bool conjunction_expansion,
unsigned max_conjuncts=UINT_MAX,
unsigned max_variables=UINT_MAX,
unsigned max_spcial_conjuncts=1,
unsigned max_cnjexp_variables=UINT_MAX,
bool enforce_specialization=false,
bool enable_type=false,
bool enable_glob=false,
std::vector<std::string> ignore_vars={});
static void configure_surprisingness(SchemeEval& scm,
const Handle& surp_rb,
const std::string& mode,
unsigned max_conjuncts,
double db_ratio);
static HandleSeq ure_surp(AtomSpace& as,
SchemeEval& scm,
const Handle& surp_rb,
const std::string& mode,
unsigned max_conjuncts,
double db_ratio=1.0);
static HandleSeq populate_nodes(AtomSpace& as,
unsigned n,
Type type,
const std::string& prefix);
static HandleSeq populate_links(AtomSpace& as,
const HandleSeq& hs,
Type type,
unsigned arity,
double p);
static Handle add_default_vardecl(const Handle& pattern);
};
}