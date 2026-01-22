#ifndef _OPENCOG_GEN_TABLE_H
#define _OPENCOG_GEN_TABLE_H
#include <boost/program_options.hpp>
namespace opencog { namespace combo {
using std::pair;
using std::string;
static const pair<string, string> rand_seed_opt("random-seed", "r");
static const pair<string, string> combo_program_opt("combo-program", "y");
static const pair<string, string> combo_program_file_opt("combo-program-file", "f");
static const pair<string, string> nsamples_opt("nsamples", "n");
static const pair<string, string> min_contin_opt("min-contin", "m");
static const pair<string, string> max_contin_opt("max-contin", "M");
static const pair<string, string> header_opt("header", "H");
static const pair<string, string> output_file_opt("output-file", "o");
static const pair<string, string> target_index_opt("target_index", "t");
}
}
#endif