#ifndef _OPENCOG_MOSES_PROBLEM_H
#define _OPENCOG_MOSES_PROBLEM_H
#include <string>
#include <boost/program_options.hpp>
#include <moses/comboreduct/combo/vertex.h>
namespace opencog { namespace moses {
class option_base
{
public:
virtual ~option_base() {}
virtual void add_options(boost::program_options::options_description&) = 0;
virtual void parse_options(boost::program_options::variables_map&) {};
};
class option_manager
{
public:
void register_options(option_base*);
void init_options();
void parse_options(int argc, char* argv[]);
private:
std::set<option_base*> _option_set;
boost::program_options::options_description _desc;
};
class problem_base
{
public:
virtual ~problem_base() {}
virtual const std::string name() const = 0;
virtual const std::string description() const = 0;
virtual void run(option_base*) = 0;
};
class problem_manager
{
public:
~problem_manager();
void register_problem(problem_base*);
problem_base* find_problem(const std::string&);
private:
std::map<std::string, problem_base*> _problem_set;
};
unsigned alphabet_size(const combo::type_tree& tt,
const combo::vertex_set ignore_ops);
}
}
#endif