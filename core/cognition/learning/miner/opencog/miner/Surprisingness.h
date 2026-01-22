#ifndef OPENCOG_SURPRISINGNESS_H_
#define OPENCOG_SURPRISINGNESS_H_
#include <opencog/util/Counter.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/core/LambdaLink.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/ure/BetaDistribution.h>
namespace opencog
{
typedef Counter<Handle, double> HandleCounter;
typedef Counter<Handle, unsigned> HandleUCounter;
class Surprisingness {
public:
static double isurp_old(const Handle& pattern,
const HandleSeq& db,
bool normalize=true);
static double isurp(const Handle& pattern,
const HandleSeq& db,
bool normalize=true,
double db_ratio=1.0);
static double dst_from_interval(double l, double u, double v);
static Handle add_pattern(const HandleSeq& block, AtomSpace& as);
static HandleSeq add_subpatterns(const HandleSeqSeq& partition,
const Handle& pattern,
AtomSpace& as);
static HandleSeq joint_variables(const Handle& pattern,
const HandleSeqSeq& partition);
static unsigned value_count(const HandleSeq& block,
const Handle& var,
const HandleSeq& db);
static HandleCounter value_distribution(const HandleSeq& block,
const Handle& var,
const HandleSeq& db);
static double inner_product(const std::vector<HandleCounter>& dists);
static double universe_count(const Handle& pattern, const HandleSeq& db);
static double prob_to_support(const Handle& pattern,
const HandleSeq& db,
double prob);
static double emp_prob(const Handle& pattern, const HandleSeq& db);
static double emp_prob_mem(const Handle& pattern,
const HandleSeq& db);
static double emp_prob_subsmp(const Handle& pattern,
const HandleSeq& db,
unsigned subsize=UINT_MAX);
static double emp_prob_bs(const Handle& pattern,
const HandleSeq& db,
unsigned n_resample,
unsigned subsize);
static double emp_prob_pbs(const Handle& pattern,
const HandleSeq& db,
double db_ratio);
static double emp_prob_pbs(const Handle& pattern,
const HandleSeq& db,
double prob_estimate,
double db_ratio);
static double emp_prob_pbs_mem(const Handle& pattern,
const HandleSeq& db,
double db_ratio);
static double emp_prob_pbs_mem(const Handle& pattern,
const HandleSeq& db,
double prob_estimate,
double db_ratio);
static TruthValuePtr emp_tv(const Handle& pattern, const HandleSeq& db);
static TruthValuePtr emp_tv_mem(const Handle& pattern,
const HandleSeq& db);
static TruthValuePtr emp_tv_subsmp(const Handle& pattern,
const HandleSeq& db,
unsigned subsize=UINT_MAX);
static TruthValuePtr emp_tv_bs(const Handle& pattern,
const HandleSeq& db,
unsigned n_resample,
unsigned subsize);
static TruthValuePtr emp_tv_pbs(const Handle& pattern,
const HandleSeq& db,
double prob_estimate,
double db_ratio);
static TruthValuePtr emp_tv_pbs_mem(const Handle& pattern,
const HandleSeq& db,
double prob_estimate,
double db_ratio);
static HandleSeq subsmp(const HandleSeq& db, unsigned subsize);
static unsigned subsmp_size(const Handle& pattern,
double db_size,
double support_estimate,
unsigned min_subsize=10U);
static std::pair<double, double> ji_prob_est_interval(const Handle& pattern,
const HandleSeq& db,
double db_ratio);
static double ji_prob_est(const HandleSeqSeq& partition,
const Handle& pattern,
const HandleSeq& db,
double db_ratio);
static TruthValuePtr ji_tv_est(const HandleSeqSeq& partition,
const Handle& pattern,
const HandleSeq& db);
static TruthValuePtr ji_tv_est(const Handle& pattern,
const HandleSeq& db);
static TruthValuePtr ji_tv_est_mem(const Handle& pattern,
const HandleSeq& db);
static bool has_same_index(const Handle& l_pat,
const Handle& r_pat,
const Handle& var);
static bool is_equivalent(const HandleSeq& l_blk,
const HandleSeq& r_blk,
const Handle& var);
static bool is_equivalent(const Handle& l_pat,
const Handle& r_pat,
const Handle& var);
static bool is_strictly_more_abstract(const HandleSeq& l_blk,
const HandleSeq& r_blk,
const Handle& var);
static void rank_by_abstraction(HandleSeqSeq& partition, const Handle& var);
static double eq_prob(const HandleSeqSeq& partition,
const Handle& pattern,
const HandleSeq& db);
static const Handle& emp_tv_key();
static TruthValuePtr get_emp_tv(const Handle& pattern);
static void set_emp_tv(const Handle& pattern, TruthValuePtr etv);
static void set_emp_prob(const Handle& pattern, double ep);
static const Handle& ji_tv_est_key();
static TruthValuePtr get_ji_tv_est(const Handle& pattern);
static void set_ji_tv_est(const Handle& pattern, TruthValuePtr etv);
static double jsd(TruthValuePtr l_tv, TruthValuePtr r_tv);
static double kld(const std::vector<double>& l_cdf,
const std::vector<double>& r_cdf);
static double avrg(double l, double r);
static double avrg(std::vector<double>& vs);
static TruthValuePtr avrg_tv(const TruthValueSeq& tvs);
static std::vector<double> avrg_cdf(const std::vector<double>& l_cdf,
const std::vector<double>& r_cdf);
static count_t confidence_to_count(confidence_t cfd);
static confidence_t count_to_confidence(count_t cnt);
static void log_pdf(const BetaDistribution& bd, int bins);
};
}
#endif