#ifndef _OPENCOG_PRINTCONTAINER_H
#define _OPENCOG_PRINTCONTAINER_H
#include <iterator>
#include <algorithm>
#include <iostream>
#include <ctype.h>
#include <boost/lexical_cast.hpp>
#include <opencog/util/oc_assert.h>
namespace opencog {
template<class Out, class It>
Out& ostream_container(Out& out,
It from,
It to,
const std::string& delimiter = " ",
const std::string& left = "",
const std::string& right = "",
bool empty_lr = true)
{
if(empty_lr || from!=to)
out << left;
if(from != to) {
while(from != to) {
out << *from;
if(++from != to)
out << delimiter;
}
}
if(empty_lr || from!=to)
out << right;
return out;
}
template<class Out, class Con>
Out& ostream_container(Out& out,
const Con& container,
const std::string& delimiter = " ",
const std::string& left = "",
const std::string& right = "",
bool empty_lr = true)
{
return ostream_container(out, container.begin(), container.end(),
delimiter, left, right, empty_lr);
}
template<class Out, class It>
Out& ostreamln_container(Out& out,
It from,
It to,
const std::string& delimiter = " ",
const std::string& left = "",
const std::string& right = "",
bool empty_lr = true)
{
ostream_container(out, from, to, delimiter, left, right, empty_lr);
out << std::endl;
return out;
}
template<class Out, class Con>
Out& ostreamln_container(Out& out,
const Con& container,
const std::string& delimiter = " ",
const std::string& left = "",
const std::string& right = "",
bool empty_lr = true)
{
ostream_container(out, container.begin(), container.end(),
delimiter, left, right, empty_lr);
out << std::endl;
return out;
}
template<class It>
void print_container(It from,
It to,
const std::string& delimiter = " ",
const std::string& left = "",
const std::string& right = "",
bool empty_lr = true)
{
ostream_container(std::cout, from, to,
delimiter, left, right, empty_lr);
}
template<class Con>
void print_container(const Con& container,
const std::string& delimiter = " ",
const std::string& left = "",
const std::string& right = "",
bool empty_lr = true)
{
ostream_container(std::cout, container,
delimiter, left, right, empty_lr);
}
template<class It>
void println_container(It from,
It to,
const std::string& delimiter = " ",
const std::string& left = "",
const std::string& right = "",
bool empty_lr = true)
{
ostreamln_container(std::cout, from, to,
delimiter, left, right, empty_lr);
}
template<class Con>
void println_container(const Con& container,
const std::string& delimiter = " ",
const std::string& left = "",
const std::string& right = "",
bool empty_lr = true)
{
ostreamln_container(std::cout, container,
delimiter, left, right, empty_lr);
}
template<class It>
std::string container_to_str(It from,
It to,
const std::string& delimiter = " ",
const std::string& left = "",
const std::string& right = "",
bool empty_lr = true)
{
std::stringstream ss;
return ostream_container(ss, from, to,
delimiter, left, right, empty_lr).str();
}
template<class Con>
std::string container_to_str(const Con& container,
const std::string& delimiter = " ",
const std::string& left = "",
const std::string& right = "",
bool empty_lr = true)
{
std::stringstream ss;
return ostream_container(ss, container,
delimiter, left, right, empty_lr).str();
}
inline bool exists_white_space(const std::string& str) {
for (const char& c : str) if(isspace(c)) return true;
return false;
}
inline bool all_white_space(const std::string& str) {
for (const char& c : str) if(!isspace(c)) return false;
return true;
}
inline bool in_well_form(const std::string& str) {
return !exists_white_space(str) || all_white_space(str);
}
template<class In, class OutIt>
In& istream_container(In& in,
OutIt out,
const std::string& left = "",
const std::string& right = "")
{
typedef typename OutIt::container_type::value_type T;
OC_ASSERT(in_well_form(left));
OC_ASSERT(in_well_form(right));
std::string s;
in >> s;
OC_ASSERT(s.substr(0, left.size()) == left,
"left = %s is not a substring of s = %s",
left.c_str(), s.c_str());
s = s.substr(left.size());
*out++ = boost::lexical_cast<T>(s);
while(!in.eof()) {
in >> s;
try {
*out++ = boost::lexical_cast<T>(s);
}
catch(boost::bad_lexical_cast &)
{
int appended_pos = s.size() - right.size();
OC_ASSERT(appended_pos > 0
&& s.rfind(right) == (size_t)appended_pos);
*out++ = boost::lexical_cast<T>(s.substr(0, appended_pos));
break;
}
}
return in;
}
}
#endif