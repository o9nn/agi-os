#include <unistd.h>
#include <fstream>
#include <boost/algorithm/string/trim.hpp>
#include <boost/range/algorithm/find_if.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <opencog/util/oc_assert.h>
#include <opencog/asmoses/combo/combo/vertex.h>
#include <opencog/asmoses/moses/moses/types.h>
#include <opencog/asmoses/moses/main/moses_exec.h>
using namespace std;
using boost::trim_copy;
using namespace boost::posix_time;
using namespace opencog::moses;
using namespace opencog::combo;
string mkstemp_moses_output() {
char tempfile[] = "/tmp/mosesUTestXXXXXX";
int fd = mkstemp(tempfile);
OC_ASSERT (fd != -1);
return tempfile;
}
std::pair<vector<string>, string> build_cmd(const vector<string>& arguments);
std::pair<string, string> build_cmd(const string& arguments);
pair<score_t, combo_tree> parse_result(const string& tempfile);
pair<score_t, string> cheap_parse_result(const string& tempfile);
vector<scored_combo_tree> parse_scored_combo_trees(const string& tempfile);
void moses_test_score(vector<string> arguments, score_t expected_sc = 0);
void moses_test_good_enough_score(vector<string> arguments, score_t expected_sc);
void moses_test_combo(vector<string> arguments,
vector<string> expected_tr_strs);
void moses_test_atomese(const vector<string>& arguments,
const vector<string>& expected_atomese_strs);
void cheap_moses_test_combo(vector<string> arguments, vector<string> expected_tr_strs);
void cheap_moses_test_atomese(const vector<string>& arguments,
const vector<string>& expected_tr_strs);
void moses_test_scored_combo_trees(const vector<string>& arguments,
const vector<scored_combo_tree>& expected_scts);