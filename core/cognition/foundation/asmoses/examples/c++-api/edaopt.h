#include <assert.h>
#include <iostream>
#include <vector>
#include <boost/lexical_cast.hpp>
using namespace std;
struct optargs
{
optargs(int argc, char** argv,
const vector<string>& additional_args = vector<string>()) {
if (argc != (5 + static_cast<int>(additional_args.size()))) {
cerr << "Error: wrong  number of args.\n"
<< "Usage: " << argv[0]
<< " <rand seed> <length> <population size> <num generations> "
<< usage(additional_args) << endl;
exit(1);
}
try {
assert(argc >= 5);
rand_seed = boost::lexical_cast<int>(argv[1]);
length = boost::lexical_cast<int>(argv[2]);
popsize = boost::lexical_cast<int>(argv[3]);
n_select = popsize;
n_generate = popsize/2;
max_gens = boost::lexical_cast<int>(argv[4]);
} catch (...) {
cerr << "Error: invalid argument\nUsage: " << argv[0]
<< " <rand seed> <length> <population size> <num generations> "
<< usage(additional_args) << endl;
exit(1);
}
}
int rand_seed;
int length;
int popsize;
int n_select;
int n_generate;
int max_gens;
private:
string usage(const vector<string>& args)
{
string res;
for (vector<string>::const_iterator i = args.begin();
i != args.end(); ++i) {
res += *i + string(" ");
}
return res;
}
};