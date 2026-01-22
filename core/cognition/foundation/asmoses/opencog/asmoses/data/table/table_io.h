#ifndef _OPENCOG_TABLE_IO_H
#define _OPENCOG_TABLE_IO_H
#include <fstream>
#include <string>
#include <vector>
#include <boost/algorithm/string.hpp>
#include <boost/range/algorithm/count.hpp>
#include <boost/range/algorithm/binary_search.hpp>
#include <boost/range/algorithm_ext/for_each.hpp>
#include <boost/tokenizer.hpp>
#include "table.h"
#include "opencog/asmoses/combo/type_checker/type_tree.h"
namespace opencog { namespace combo {
void removeCarriageReturn(std::string& str);
void removeNonASCII(std::string& str);
bool checkCarriageReturn(std::istream& in);
builtin token_to_boolean(const std::string& token);
contin_t token_to_contin(const std::string& token);
vertex token_to_vertex(const type_node &tipe, const std::string& token);
typedef boost::tokenizer<boost::escaped_list_separator<char>> table_tokenizer;
table_tokenizer get_row_tokenizer(const std::string& line);
template<typename T>
static std::vector<T> tokenizeRow(
const std::string& line,
const std::vector<unsigned>& ignored_indices=std::vector<unsigned>())
{
table_tokenizer tok = get_row_tokenizer(line);
std::vector<T> res;
unsigned i = 0;
for (const std::string& t : tok) {
std::string clean(t);
boost::trim(clean);
if (0 == clean.size()) continue;
if (!boost::binary_search(ignored_indices, i++))
res.push_back(boost::lexical_cast<T>(clean));
}
return res;
}
string_seq get_header(const std::string& input_file);
std::istream& istreamRawITable(
std::istream& in, ITable& tab,
const std::vector<unsigned>& ignored_indices=std::vector<unsigned>());
std::istream& istreamITable(std::istream& in, ITable& tab,
const string_seq& ignore_features);
std::istream& istreamTable(std::istream& in, Table& tab,
const std::string& target_feature,
const std::string& timestamp_feature,
const string_seq& ignore_features);
std::istream& istreamCompressedTable(std::istream& in, CompressedTable& ctable);
OTable loadOTable(const std::string& file_name,
const std::string& target_feature);
ITable loadITable(
const std::string& file_name,
const string_seq& ignore_features=string_seq());
ITable loadITable_optimized(
const std::string& file_name,
const string_seq& ignore_features=string_seq());
Table loadTable(
const std::string& file_name,
const std::string& target_feature=std::string(),
const std::string& timestamp_feature=std::string(),
const string_seq& ignore_features=string_seq());
std::istream& istreamDenseTable(std::istream& in, Table& tab,
const std::string& target_feature,
const std::string& timestamp_feature,
const string_seq& ignore_features,
const type_tree& tt, bool has_header);
CompressedTable loadCompressedTable(const std::string& file_name);
template<typename Out>
Out& ostreamTableHeader(Out& out, const Table& table)
{
string_seq header = table.itable.get_labels();
unsigned hsize = header.size();
const std::string& ol = table.otable.get_label();
header.insert(header.begin() + std::min(table.target_pos, hsize), ol);
if (!table.ttable.empty()) {
const std::string& tl = table.ttable.get_label();
header.insert(header.begin() + table.timestamp_pos, tl);
}
ostream_container(out, header, ",") << std::endl;
return out;
}
template<typename Out>
Out& ostreamTable(Out& out, const Table& table)
{
ostreamTableHeader(out, table);
unsigned isize = table.itable.size(), osize = table.otable.size();
OC_ASSERT(table.itable.empty() || isize == osize);
for (size_t row = 0; row < osize; ++row) {
string_seq content;
if (!table.itable.empty())
content = table.itable[row].to_strings();
unsigned csize = content.size();
std::string oc = table_fmt_vertex_to_str(table.otable[row]);
content.insert(content.begin() + std::min(table.target_pos, csize), oc);
if (!table.ttable.empty()) {
std::string tc = TTable::to_string(table.ttable[row]);
content.insert(content.begin() + table.timestamp_pos, tc);
}
ostream_container(out, content, ",") << std::endl;
}
return out;
}
void saveTable(const std::string& file_name, const Table& table);
std::ostream& ostreamCompressedTableRow(std::ostream& out, const CompressedTable::value_type& ctv);
std::ostream& ostreamCompressedTable(std::ostream& out, const CompressedTable& ct);
std::ostream& ostreamCompressedTableTime(std::ostream& out, const CompressedTableTime& ctt);
std::ostream& operator<<(std::ostream& out, const ITable& it);
std::ostream& operator<<(std::ostream& out, const OTable& ot);
std::ostream& operator<<(std::ostream& out, const Table& table);
std::ostream& operator<<(std::ostream& out, const CompressedTable& ct);
std::ostream& operator<<(std::ostream& out, const complete_truth_table& tt);
}
std::string oc_to_string(const combo::ITable& it,
const std::string& indent=empty_string);
std::string oc_to_string(const combo::OTable& ot,
const std::string& indent=empty_string);
std::string oc_to_string(const combo::Table& table,
const std::string& indent=empty_string);
std::string oc_to_string(const combo::CompressedTable& ct,
const std::string& indent=empty_string);
std::string oc_to_string(const combo::complete_truth_table& tt,
const std::string& indent=empty_string);
}
#endif