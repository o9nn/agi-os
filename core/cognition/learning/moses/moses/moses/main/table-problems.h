#ifndef _OPENCOG_MOSES_TABLE_PROBLEMS_H
#define _OPENCOG_MOSES_TABLE_PROBLEMS_H
#include <boost/ptr_container/ptr_vector.hpp>
#include <moses/moses/scoring/scoring_base.h>
#include "problem-params.h"
namespace opencog { namespace moses {
void register_table_problems(problem_manager&, option_manager&);
struct table_problem_params : public option_base
{
void add_options(boost::program_options::options_description&);
std::vector<std::string> input_data_files;
std::string target_feature;
std::string timestamp_feature;
std::string weighting_feature;
std::vector<std::string> ignore_features_str;
};
class table_problem_base : public problem_base
{
public:
table_problem_base(table_problem_params& tp) : _tpp(tp) {}
protected:
table_problem_params& _tpp;
typedef boost::ptr_vector<bscore_base> BScorerSeq;
void common_setup(problem_params&);
void common_type_setup(problem_params&, type_node = id::unknown_type);
std::vector<Table> _tables;
std::vector<CTable> _ctables;
Table table;
CTable ctable;
std::vector<std::string> ilabels;
combo::arity_t arity;
type_tree table_type_signature;
type_tree cand_type_signature;
type_node output_type;
};
struct ip_problem_params : public option_base
{
void add_options(boost::program_options::options_description&);
double ip_kld_weight;
double ip_skewness_weight;
double ip_stdU_weight;
double ip_skew_U_weight;
};
class ip_problem : public table_problem_base
{
public:
ip_problem(table_problem_params& tp, ip_problem_params& ip)
: table_problem_base(tp), _ippp(ip) {}
virtual const std::string name() const { return "ip"; }
virtual const std::string description() const {
return "Find interesting patterns"; }
virtual void run(option_base*);
protected:
ip_problem_params& _ippp;
};
class ann_table_problem : public table_problem_base
{
public:
ann_table_problem(table_problem_params& tp)
: table_problem_base(tp) {}
virtual const std::string name() const { return "ann-it"; }
virtual const std::string description() const {
return "ANN-based regression on input table"; }
virtual void run(option_base*);
};
class pre_table_problem : public table_problem_base
{
public:
pre_table_problem(table_problem_params& tp)
: table_problem_base(tp) {}
virtual const std::string name() const { return "pre"; }
virtual const std::string description() const {
return "Precision-Activation scoring"; }
virtual void run(option_base*);
};
class pre_conj_table_problem : public table_problem_base
{
public:
pre_conj_table_problem(table_problem_params& tp)
: table_problem_base(tp) {}
virtual const std::string name() const { return "pre-conj"; }
virtual const std::string description() const {
return "Precision-Conjunction-Maximization"; }
virtual void run(option_base*);
};
class prerec_table_problem : public table_problem_base
{
public:
prerec_table_problem(table_problem_params& tp)
: table_problem_base(tp) {}
virtual const std::string name() const { return "prerec"; }
virtual const std::string description() const {
return "Precision Maximization (holding recall constant)"; }
virtual void run(option_base*);
};
class recall_table_problem : public table_problem_base
{
public:
recall_table_problem(table_problem_params& tp)
: table_problem_base(tp) {}
virtual const std::string name() const { return "recall"; }
virtual const std::string description() const {
return "Recall Maximization (holding precision constant)"; }
virtual void run(option_base*);
};
class bep_table_problem : public table_problem_base
{
public:
bep_table_problem(table_problem_params& tp)
: table_problem_base(tp) {}
virtual const std::string name() const { return "bep"; }
virtual const std::string description() const {
return "Maximize Break-even Point"; }
virtual void run(option_base*);
};
class f_one_table_problem : public table_problem_base
{
public:
f_one_table_problem(table_problem_params& tp)
: table_problem_base(tp) {}
virtual const std::string name() const { return "f_one"; }
virtual const std::string description() const {
return "Maximize F_1 score"; }
virtual void run(option_base*);
};
class it_table_problem : public table_problem_base
{
public:
it_table_problem(table_problem_params& tp)
: table_problem_base(tp) {}
virtual const std::string name() const { return "it"; }
virtual const std::string description() const {
return "Maximize Accuracy"; }
virtual void run(option_base*);
};
class select_table_problem : public table_problem_base
{
public:
select_table_problem(table_problem_params& tp)
: table_problem_base(tp) {}
virtual const std::string name() const { return "select"; }
virtual const std::string description() const {
return "Select range of rows"; }
virtual void run(option_base*);
};
class cluster_table_problem : public table_problem_base
{
public:
cluster_table_problem(table_problem_params& tp)
: table_problem_base(tp) {}
virtual const std::string name() const { return "cluster"; }
virtual const std::string description() const {
return "Discover clustering function"; }
virtual void run(option_base*);
};
}
}
#endif