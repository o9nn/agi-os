#ifndef _OPENCOG_LOG_PROG_NAME_H
#define _OPENCOG_LOG_PROG_NAME_H
#include <boost/program_options.hpp>
#include <opencog/asmoses/utils/iostreamContainer.h>
#include <sstream>
namespace opencog {
template<typename T>
bool to_string(const boost::program_options::variable_value& vv,
std::string& str,
std::string separator)
{
if(vv.value().type() == typeid(T)) {
std::stringstream ss;
ss << vv.as<T>();
str = ss.str();
return true;
} else if(vv.value().type() == typeid(std::vector<T>)) {
str = opencog::container_to_str(vv.as<std::vector<T> >(), separator.c_str());
return true;
}
return false;
}
std::string to_string(const boost::program_options::variable_value& vv,
std::string separator = "_");
std::string determine_log_name(const std::string& log_file_prefix,
const boost::program_options::variables_map& vm,
const std::set<std::string>& ignore_opt,
const std::string& log_file_suffix = ".log");
}
#endif