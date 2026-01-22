#include <unistd.h>
#include <boost/algorithm/string/trim.hpp>
#include <boost/range/algorithm/find_if.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <opencog/util/oc_assert.h>
#include <moses/comboreduct/combo/vertex.h>
#include <moses/moses/moses/types.h>
#include <moses/moses/main/moses_exec.h>
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
std::pair<vector<string>, string> build_cmd(const vector<string>& arguments)
{
string tempfile = mkstemp_moses_output();
vector<string> cmd = arguments;
cmd.insert(cmd.begin(), string("moses-exec"));
cmd.push_back(string("-o").append(tempfile));
return {cmd, tempfile};
}
std::pair<string, string> build_cmd(const string& arguments)
{
string tempfile = mkstemp_moses_output();
string cmd = string("moses-exec ") + arguments + " -o" + tempfile;
return {cmd, tempfile};
}
pair<score_t, combo_tree> parse_result(const string& tempfile)
{
cout << "tempfile " << tempfile << endl;
ifstream in(tempfile);
score_t hiscore = -1.0e37;
combo_tree hitr;
while (not in.eof()) {
score_t score;
in >> score;
if (in.eof()) break;
if (in.fail()) {
std::cout << "Error: fail to read score and tree" << std::endl;
break;
}
combo_tree tr;
in >> tr;
if (hiscore < score) {
hiscore = score;
hitr = tr;
std::cout << score << " " << tr << std::endl;
}
}
return {hiscore, hitr};
}
pair<score_t, string> cheap_parse_result(const string& tempfile)
{
ifstream in(tempfile);
score_t hiscore = -1.0e37;
string hitr_str;
while (!in.eof()) {
score_t score;
in >> score;
if (in.eof()) break;
if (in.fail()) {
std::cout << "Error: fail to read score and tree" << std::endl;
break;
}
char tr_chars[4096];
in.getline(tr_chars, 4096);
char* junk = strstr(tr_chars, "[score");
if (junk) *junk = 0x0;
string tr_str(tr_chars);
if (hiscore < score) {
hiscore = score;
hitr_str = tr_str;
std::cout << "score = " << score << " tr_str = " << tr_str << std::endl;
}
}
return {hiscore, hitr_str};
}
vector<scored_combo_tree> parse_scored_combo_trees(const string& tempfile)
{
cout << "tempfile " << tempfile << endl;
ifstream in(tempfile);
vector<scored_combo_tree> scts;
istream_scored_combo_trees(in, scts);
return scts;
}
void moses_test_score(vector<string> arguments, score_t expected_sc = 0)
{
auto t1 = microsec_clock::local_time();
pair<vector<string>, string> cmd_tmp = build_cmd(arguments);
moses_exec(cmd_tmp.first);
auto result = parse_result(cmd_tmp.second);
TS_ASSERT_LESS_THAN(fabs(result.first - expected_sc), 1.0e-8);
auto t2 = microsec_clock::local_time();
std::cout << "Wallclock time: " << (t2 - t1) << std::endl;
if (fabs(result.first - expected_sc) < 1.0e-8)
unlink(cmd_tmp.second.c_str());
}
void moses_test_good_enough_score(vector<string> arguments, score_t expected_sc)
{
auto t1 = microsec_clock::local_time();
pair<vector<string>, string> cmd_tmp = build_cmd(arguments);
moses_exec(cmd_tmp.first);
auto result = parse_result(cmd_tmp.second);
TS_ASSERT_LESS_THAN(expected_sc, result.first);
auto t2 = microsec_clock::local_time();
std::cout << "Wallclock time: " << (t2 - t1) << std::endl;
if (expected_sc < result.first)
unlink(cmd_tmp.second.c_str());
}
void moses_test_combo(vector<string> arguments,
vector<string> expected_tr_strs)
{
auto t1 = microsec_clock::local_time();
pair<vector<string>, string> cmd_tmp = build_cmd(arguments);
moses_exec(cmd_tmp.first);
auto result = parse_result(cmd_tmp.second);
auto f_it = boost::find_if(expected_tr_strs,
[&](const string& tr_str) {
combo_tree tr;
stringstream(tr_str) >> tr;
return tr == result.second;});
TS_ASSERT(f_it != expected_tr_strs.end());
auto t2 = microsec_clock::local_time();
std::cout << "Wallclock time: " << (t2 - t1) << std::endl;
if (f_it != expected_tr_strs.end())
unlink(cmd_tmp.second.c_str());
}
void cheap_moses_test_combo(vector<string> arguments,
vector<string> expected_tr_strs)
{
auto t1 = microsec_clock::local_time();
pair<vector<string>, string> cmd_tmp = build_cmd(arguments);
moses_exec(cmd_tmp.first);
auto result = cheap_parse_result(cmd_tmp.second);
auto f_it = boost::find_if(expected_tr_strs, [&](const string& tr_str) {
return trim_copy(result.second) == trim_copy(tr_str); });
TS_ASSERT(f_it != expected_tr_strs.end());
auto t2 = microsec_clock::local_time();
std::cout << "Wallclock time: " << (t2 - t1) << std::endl;
if (f_it != expected_tr_strs.end())
unlink(cmd_tmp.second.c_str());
}
void moses_test_scored_combo_trees(const vector<string>& arguments,
const vector<scored_combo_tree>& expected_scts)
{
auto t1 = microsec_clock::local_time();
auto cmd_tmp = build_cmd(arguments);
moses_exec(cmd_tmp.first);
vector<scored_combo_tree> scts = parse_scored_combo_trees(cmd_tmp.second);
TS_ASSERT(scts == expected_scts);
auto t2 = microsec_clock::local_time();
std::cout << "Wallclock time: " << (t2 - t1) << std::endl;
if (scts == expected_scts)
unlink(cmd_tmp.second.c_str());
}