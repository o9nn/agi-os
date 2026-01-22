#ifndef _OPENCOG_URE_CONFIG_H
#define _OPENCOG_URE_CONFIG_H
#include "Rule.h"
#include <opencog/atomspace/AtomSpace.h>
#include "URELogger.h"
namespace opencog {
class UREConfig
{
public:
UREConfig(AtomSpace& as, const Handle& rbs);
const RuleSet& get_rules() const;
RuleSet& get_rules();
int get_maximum_iterations() const;
double get_complexity_penalty() const;
int get_jobs() const;
int get_expansion_pool_size() const;
bool get_retry_exhausted_sources() const;
bool get_full_rule_application() const;
double get_max_bit_size() const;
double get_mm_complexity_penalty() const;
double get_mm_compressiveness() const;
std::string get_maximum_iterations_str() const;
void set_maximum_iterations(int);
void set_complexity_penalty(double);
void set_jobs(int);
void set_expansion_pool_size(int);
void set_retry_exhausted_sources(bool);
void set_full_rule_application(bool);
void set_mm_complexity_penalty(double);
void set_mm_compressiveness(double);
static const std::string top_rbs_name;
static const std::string max_iter_name;
static const std::string complexity_penalty_name;
static const std::string jobs_name;
static const std::string expansion_pool_size_name;
static const std::string fc_retry_exhausted_sources_name;
static const std::string fc_full_rule_application_name;
static const std::string bc_max_bit_size_name;
static const std::string bc_mm_complexity_penalty_name;
static const std::string bc_mm_compressiveness_name;
private:
AtomSpace& _as;
struct CommonParameters {
RuleSet rules;
int max_iter;
double complexity_penalty;
int jobs;
int expansion_pool_size;
};
CommonParameters _common_params;
struct FCParameters {
bool retry_exhausted_sources;
bool full_rule_application;
};
FCParameters _fc_params;
struct BCParameters {
int max_bit_size;
double mm_complexity_penalty;
double mm_compressiveness;
};
BCParameters _bc_params;
HandleSeq fetch_rule_names(const Handle& rbs);
void fetch_common_parameters(const Handle& rbs);
void fetch_fc_parameters(const Handle& rbs);
void fetch_bc_parameters(const Handle& rbs);
HandleSeq fetch_execution_outputs(const Handle& schema,
const Handle& input,
Type type=ATOM);
double fetch_num_param(const std::string& schema_name,
const Handle& input,
double default_value=0.0);
bool fetch_bool_param(const std::string& pred_name,
const Handle& input,
bool default_value=false);
template<typename T>
void log_param_value(const Handle& rbs_input,
const std::string& param_name,
const T& value, bool is_default=false) const
{
ure_logger().debug() << "Rule-base " << rbs_input->get_name()
<< ", set parameter " << param_name
<< " to " << value
<< (is_default ? " [default]" : "");
}
};
}
#endif