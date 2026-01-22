#ifndef _OPENCOG_EVAL_CANDIDATE_LIKELIHOOD_H
#define _OPENCOG_EVAL_CANDIDATE_LIKELIHOOD_H
namespace opencog { namespace moses {
struct eval_candidate_likelihood_params
{
std::string input_file;
std::vector<std::string> combo_program_files;
std::string output_file,
target_feature_str;
std::string problem;
double noise;
bool normalize;
double complexity_amplifier;
double prerec_min_recall;
bool prerec_simple_precision;
};
static const std::string it="it";
static const std::string prerec="prerec";
}
}
#endif