#ifndef _OPENCOG_EVAL_TABLE_H
#define _OPENCOG_EVAL_TABLE_H
#include <boost/assign/std/vector.hpp>
#include <opencog/util/algorithm.h>
#include <opencog/util/numeric.h>
#include <opencog/util/Logger.h>
#include "../table/table_io.h"
#include "../table/table.h"
using namespace std;
using namespace boost::assign;
using namespace opencog;
using namespace combo;
string opt_desc_str(const pair<string, string>& opt) {
return string(opt.first).append(",").append(opt.second);
}
combo_tree str2combo_tree_label(const std::string& combo_prog_str,
bool has_labels,
const std::vector<std::string>& labels);
struct evalTableParameters
{
string input_table_file;
vector<string> combo_programs;
vector<string> combo_programs_files;
string target_feature_str;
string timestamp_feature_str;
vector<string> ignore_features_str;
bool has_labels;
vector<string> features;
string features_file;
bool display_inputs;
vector<string> output_files;
bool split_output;
string log_level;
string log_file;
};
evalTableParameters eval_table_program_args(int argc, char** argv);
template<typename Out>
Out& output_results(Out& out, const evalTableParameters& pa,
const Table& table, const OTable& ot_tr)
{
Table eval_table = table;
eval_table.otable = ot_tr;
if (!pa.display_inputs) {
eval_table.itable = ITable();
eval_table.target_pos = 0;
}
return ostreamTable(out, eval_table);
}
void output_results(const evalTableParameters& pa,
const Table& table, const OTable& ot_tr,
const string output_file = "");
void eval_output_results(const evalTableParameters& pa,
const Table& table, const vector<combo_tree>& trs);
vector<string> get_all_combo_tree_str(const evalTableParameters& pa);
void read_eval_output_results(evalTableParameters& pa);
#endif