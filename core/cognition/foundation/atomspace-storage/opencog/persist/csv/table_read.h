#ifndef _ATOMESE_TABLE_READ_H
#define _ATOMESE_TABLE_READ_H
#include <fstream>
#include <string>
#include <vector>
#include <opencog/atomspace/AtomSpace.h>
namespace opencog {
typedef std::vector<std::string> string_seq;
void load_csv_table(const AtomSpacePtr&,
const Handle& anchor,
const std::string& file_name,
const string_seq& ignore_features=string_seq());
std::istream& istreamTable(const AtomSpacePtr&,
const Handle&,
std::istream&,
const string_seq& ignore_features);
}
#endif