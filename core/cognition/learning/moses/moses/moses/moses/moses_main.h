#ifndef _OPENCOG_MOSES_MAIN_H
#define _OPENCOG_MOSES_MAIN_H
#include <iostream>
#include <fstream>
#include <opencog/util/Logger.h>
#include <moses/comboreduct/combo/combo.h>
#include "../deme/deme_expander.h"
#include "../metapopulation/metapopulation.h"
#include "../optimization/hill-climbing.h"
#include "../optimization/star-anneal.h"
#include "../optimization/univariate.h"
#include "../optimization/particle-swarm.h"
#include "../scoring/behave_cscore.h"
#include "../scoring/ss_bscore.h"
#include "distributed_moses.h"
#include "moses_params.h"
namespace opencog { namespace moses {
using namespace std;
extern const char * version_string;
void run_moses(metapopulation&,
deme_expander&,
const moses_parameters&,
moses_statistics&);
struct metapop_printer
{
metapop_printer() {}
metapop_printer(long _result_count,
bool _output_score,
bool _output_cscore,
bool _output_bscore,
bool _output_only_best,
bool _output_ensemble,
bool _output_eval_number,
bool _output_with_labels,
bool _output_demeID,
const std::vector<std::string>& _ilabels,
const std::string& _output_file,
combo::output_format _fmt,
bool _is_mpi) :
result_count(_result_count),
output_score(_output_score),
output_cscore(_output_cscore),
output_bscore(_output_bscore),
output_only_best(_output_only_best),
output_ensemble(_output_ensemble),
output_eval_number(_output_eval_number),
output_with_labels(_output_with_labels),
output_demeID(_output_demeID),
ilabels(_ilabels),
output_file(_output_file),
fmt(_fmt),
is_mpi(_is_mpi) {}
void operator()(metapopulation &metapop,
deme_expander& dex,
moses_statistics& stats) const
{
if (is_mpi && metapop.size() == 0)
return;
stringstream ss;
ss << std::setprecision(opencog::io_score_precision);
if (output_ensemble) {
const scored_combo_tree_set& tree_set =
metapop.get_ensemble().get_ensemble();
if (output_format::python == fmt or output_format::python3 == fmt) {
if (output_format::python == fmt)
ss << "#!/usr/bin/env python\n";
else
ss << "#!/usr/bin/env python3\n";
ss << "#score: " << metapop.best_score() << std::endl
<< "def moses_eval(i):\n"
<< "    sum = 0.0 \\\n";
for (const scored_combo_tree& sct : tree_set)
ostream_combo_tree(ss << "      + " << sct.get_weight()
<< " * ", sct.get_tree(),
output_with_labels? ilabels :
std::vector<std::string>(),
fmt) << "\\\n";
ss << "\n    return (0.0 < val)\n";
} else {
const ensemble& ensm(metapop.get_ensemble());
ss << ensm.flat_score() << " "
<< ensm.get_weighted_tree();
ss << std::endl;
}
} else {
scored_combo_tree_ptr_set tree_set;
if (output_only_best) {
for (const scored_combo_tree& sct : metapop.best_candidates())
tree_set.insert(new scored_combo_tree(sct));
} else {
tree_set = metapop.get_trees();
}
long cnt = 0;
for (const scored_combo_tree& sct : tree_set) {
if (result_count == cnt++) break;
if (output_format::python == fmt or output_format::python3 == fmt) {
if (output_format::python == fmt)
ss << "#!/usr/bin/env python\n";
else
ss << "#!/usr/bin/env python3\n";
ss << "#score: " << sct.get_score() << std::endl
<< "import operator as op\n"
<< "from functools import reduce\n"
<< "from math import log, exp, sin\n"
<< "def l0(i): return 0 < i\n"
<< "def adds(*args): return sum(args)\n"
<< "def muls(*args): return reduce(op.mul, args)\n"
<< "def pdiv(a, b): return a / (b + 0.000001)\n"
<< "def impulse(a): return 1.0 if a else 0.0\n"
<< "def moses_eval(i):\n"
<< "    return ";
ostream_combo_tree(ss, sct.get_tree(),
output_with_labels? ilabels :
std::vector<std::string>(),
fmt);
ss << std::endl;
} else {
ostream_scored_combo_tree(ss, sct, output_score,
output_cscore, output_demeID,
output_bscore,
output_with_labels? ilabels :
std::vector<std::string>(),
fmt);
}
}
}
if (output_eval_number)
ss << number_of_evals_str << ": " << stats.n_evals << std::endl;;
if (output_file.empty())
std::cout << ss.str();
else {
ofstream of(output_file.c_str());
of << ss.str();
of.close();
}
if (logger().is_info_enabled()) {
if (output_ensemble) {
stringstream ssb;
for (const auto& cand : metapop.get_ensemble().get_ensemble()) {
ssb << cand.get_weight() << " " << cand.get_tree();
}
if (ssb.str().empty())
logger().warn("Ensemble was empty!");
else
logger().info("Final ensemble, consisting of %d members:\n%s",
metapop.get_ensemble().get_ensemble().size(),
ssb.str().c_str());
} else {
stringstream ssb;
metapop.ostream_metapop(ssb, 1);
if (ssb.str().empty())
logger().warn("No candidate is good enough to be returned. "
"Yeah that's bad!");
else
logger().info("Best candidate:\n%s", ssb.str().c_str());
}
}
#ifdef GATHER_STATS
dex._optimize.hiscore /= dex._optimize.hicount;
dex._optimize.num_improved /= dex._optimize.count_improved;
logger().info() << "Avg number of improved scores = "
<< dex._optimize.num_improved;
logger().info() << "Avg improved as percentage= "
<< 100.0 * dex._optimize.num_improved /
dex._optimize.scores.size();
for (unsigned i=0; i< dex._optimize.scores.size(); i++) {
dex._optimize.scores[i] /= dex._optimize.counts[i];
logger().info() << "Avg Scores: "
<< i << "\t"
<< dex._optimize.hiscore << "\t"
<< dex._optimize.counts[i] << "\t"
<< dex._optimize.scores[i];
}
#endif
}
private:
long result_count;
bool output_score;
bool output_cscore;
bool output_bscore;
bool output_only_best;
bool output_ensemble;
bool output_eval_number;
bool output_with_labels;
bool output_demeID;
public:
std::vector<std::string> ilabels;
private:
string output_file;
output_format fmt;
bool is_mpi;
};
template<typename Printer>
void metapop_moses_results_b(const std::vector<combo_tree>& bases,
const opencog::combo::type_tree& tt,
const reduct::rule& si_ca,
const reduct::rule& si_kb,
behave_cscore& sc,
const optim_parameters& opt_params,
const hc_parameters& hc_params,
const ps_parameters& ps_params,
const deme_parameters& deme_params,
const subsample_deme_filter_parameters& filter_params,
const metapop_parameters& meta_params,
const moses_parameters& moses_params,
Printer& printer)
{
moses_statistics stats;
optimizer_base* optimizer = nullptr;
if (opt_params.opt_algo == hc) {
optimizer = new hill_climbing(opt_params, hc_params);
}
else if (opt_params.opt_algo == sa) {
optimizer = new simulated_annealing(opt_params);
}
else if (opt_params.opt_algo == un) {
optimizer = new univariate_optimization(opt_params);
}
else if (opt_params.opt_algo == ps) {
optimizer = new particle_swarm(opt_params, ps_params);
}
else {
std::cerr << "Unknown optimization algo " << opt_params.opt_algo
<< ". Supported algorithms are un (for univariate),"
<< " sa (for star-shaped search), hc (for local search)"
<< " and ps (for particle swarm)"
<< std::endl;
exit(1);
}
std::vector<combo_tree> simple_bases;
for (const combo_tree& xmplr: bases) {
combo_tree siba(xmplr);
si_ca(siba);
simple_bases.push_back(siba);
}
deme_expander dex(tt, si_ca, si_kb, sc, *optimizer, deme_params, filter_params);
metapopulation metapop(simple_bases, sc, meta_params, filter_params);
run_moses(metapop, dex, moses_params, stats);
printer(metapop, dex, stats);
delete optimizer;
}
void adjust_termination_criteria(const behave_cscore& sc,
optim_parameters& opt_params,
moses_parameters& moses_params);
void autoscale_diversity(const behave_cscore& sc,
metapop_parameters& meta_params);
template<typename Printer>
void metapop_moses_results(const std::vector<combo_tree>& bases,
const opencog::combo::type_tree& type_sig,
const reduct::rule& si_ca,
const reduct::rule& si_kb,
behave_cscore& c_scorer,
const optim_parameters opt_params,
const hc_parameters hc_params,
const ps_parameters ps_params,
const deme_parameters& deme_params,
const subsample_deme_filter_parameters& filter_params,
const metapop_parameters& meta_params,
const moses_parameters moses_params,
Printer& printer)
{
optim_parameters twk_opt_params(opt_params);
metapop_parameters twk_meta_params(meta_params);
moses_parameters twk_moses_params(moses_params);
adjust_termination_criteria(c_scorer, twk_opt_params, twk_moses_params);
autoscale_diversity(c_scorer, twk_meta_params);
if (filter_params.n_subsample_fitnesses > 1
and filter_params.low_dev_pressure > 0.0)
{
const bscore_base& bscorer = c_scorer.get_bscorer();
ss_bscore ss_bscorer(bscorer,
filter_params.n_subsample_fitnesses,
filter_params.low_dev_pressure,
filter_params.by_time);
behave_cscore ss_cscorer(ss_bscorer);
metapop_moses_results_b(bases, type_sig, si_ca, si_kb,
ss_cscorer,
twk_opt_params, hc_params, ps_params,
deme_params, filter_params, twk_meta_params,
twk_moses_params, printer);
} else {
metapop_moses_results_b(bases, type_sig, si_ca, si_kb,
c_scorer,
twk_opt_params, hc_params, ps_params,
deme_params, filter_params, twk_meta_params,
twk_moses_params, printer);
}
}
}
}
#endif