#ifndef _OPENCOG_DISTRIBUTED_MOSES_H
#define _OPENCOG_DISTRIBUTED_MOSES_H
#include <boost/program_options.hpp>
#include <boost/tuple/tuple.hpp>
#include <opencog/asmoses/utils/iostreamContainer.h>
#include <opencog/asmoses/utils/log_prog_name.h>
#include "opencog/asmoses/moses/deme/deme_expander.h"
#include "opencog/asmoses/moses/metapopulation/metapopulation.h"
#include "moses_params.h"
namespace opencog { namespace moses {
static const std::string number_of_evals_str = "n_evals";
pid_t get_parent_pid();
typedef boost::tuple<std::string, std::string, FILE*, unsigned> proc_info;
typedef std::map<int, proc_info> proc_map;
int get_pid(const proc_map::value_type& pmv);
std::string get_cmd(const proc_map::value_type& pmv);
std::string get_tmp(const proc_map::value_type& pmv);
FILE* get_file(const proc_map::value_type& pmv);
unsigned get_num_jobs(const proc_map::value_type& pmv);
typedef std::map<std::string, proc_map> host_proc_map;
const std::string& get_hostname(const host_proc_map::value_type& hp);
const proc_map& get_proc_map(const host_proc_map::value_type& hp);
const unsigned get_total_jobs(const host_proc_map::value_type& hp);
unsigned running_proc_count(const host_proc_map& hpm);
std::string build_cmdline(const boost::program_options::variables_map& vm,
const combo_tree& tr,
const std::string& host_name,
unsigned n_jobs,
unsigned max_evals,
unsigned gen_idx);
proc_map::value_type launch_cmd(std::string cmd, unsigned n_jobs);
bool is_being_written(const std::string& file_name, int pid);
bool is_running(const proc_map::value_type& pmv);
void parse_result(std::istream& in, scored_combo_tree_set& candidates, int& evals);
void parse_result(const proc_map::value_type& pmv,
scored_combo_tree_set& candidates, int& evals);
host_proc_map init(const jobs_t& jobs);
proc_map::iterator remove_proc(proc_map& pm,  proc_map::iterator it);
void killall(proc_map& pm);
void killall(host_proc_map& hpm);
host_proc_map::iterator find_free_resource(host_proc_map& hpm,
const jobs_t& jobs);
bool all_resources_free(const host_proc_map& hpm);
void distributed_moses(metapopulation&,
deme_expander&,
const moses_parameters&,
moses_statistics&);
}
}
#endif