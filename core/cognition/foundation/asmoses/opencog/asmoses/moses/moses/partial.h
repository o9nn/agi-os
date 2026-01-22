#include <vector>
#include <opencog/asmoses/data/table/table.h>
#include <opencog/asmoses/combo/type_checker/type_tree.h>
#include <opencog/asmoses/combo/combo/vertex.h>
#include <opencog/asmoses/reduct/reduct/reduct.h>
#include "opencog/asmoses/moses/optimization/hill-climbing.h"
#include "opencog/asmoses/moses/metapopulation/metapopulation.h"
#include "opencog/asmoses/moses/scoring/behave_cscore.h"
#include "opencog/asmoses/moses/scoring/bscores.h"
#include "moses_main.h"
namespace opencog { namespace moses {
using namespace combo;
using namespace reduct;
class partial_solver
{
public:
partial_solver(const CompressedTable &ctable,
const combo_tree_seq& exemplars,
const optim_parameters& opt_params,
const hc_parameters& hc_params,
const ps_parameters& ps_params,
const representation_parameters& rep_params,
const deme_parameters& deme_params,
const subsample_deme_filter_parameters&,
const metapop_parameters& meta_params,
const moses_parameters& moses_params,
const metapop_printer& mmr_pa);
~partial_solver();
void solve();
void operator()(metapopulation &metapop,
deme_expander& dex,
moses_statistics& stats)
{
_num_evals = stats.n_evals;
if (0 == _most_good)
_done = true;
if (_moses_params.max_evals <= _num_evals)
_done = true;
if (_done)
final_cleanup(metapop);
else
refresh(metapop);
}
static bool check_candidates(scored_combo_tree_set& cands, void *ud)
{
partial_solver *ps = (partial_solver *) ud;
return ps->eval_candidates(cands);
}
protected:
bool eval_candidates(const scored_combo_tree_set&);
void eval_candidate(const combo_tree&);
void record_prefix();
void effective(combo_tree::iterator,
unsigned& good_count,
unsigned& fail_count);
void trim_table(CompressedTable&,
const combo_tree::iterator,
unsigned& deleted,
unsigned& total);
void refresh(const metapopulation&);
void final_cleanup(const metapopulation&);
private:
CompressedTable _ctable;
const CompressedTable& _orig_ctable;
const type_tree& _table_type_signature;
combo_tree_seq _exemplars;
combo_tree _leader;
unsigned _prefix_count;
optim_parameters _opt_params;
hc_parameters _hc_params;
ps_parameters _ps_params;
representation_parameters _rep_params;
deme_parameters _deme_params;
subsample_deme_filter_parameters _filter_params;
metapop_parameters _meta_params;
moses_parameters _moses_params;
const metapop_printer& _printer;
typedef enum_effective_bscore BScore;
enum_effective_bscore *_bscore;
behave_cscore *_cscore;
typedef enum_table_bscore StraightBScore;
enum_table_bscore *_straight_bscore;
behave_cscore *_straight_cscore;
int _num_evals;
int _num_gens;
bool _done;
unsigned _most_good;
combo_tree::iterator _best_predicate;
};
};};