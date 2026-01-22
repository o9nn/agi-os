#ifndef _OPENCOG_EVAL_CANDIDATE_H
#define _OPENCOG_EVAL_CANDIDATE_H
namespace opencog { namespace moses {
struct eval_candidate_params
{
std::string input_file;
string_seq combo_programs;
string_seq combo_program_files;
string_seq output_files;
bool output_with_labels;
std::string target_feature_str;
std::string problem;
unsigned jobs;
double activation_pressure;
double min_activation;
double max_activation;
bool pre_positive;
};
static const std::string f_one="f_one";
}
}
#endif