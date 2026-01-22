#pragma once
#include <map>
#include <string>
#include <vector>
#include "Assignment.h"
#include "QueryAnswer.h"
#include "expression_hasher.h"
using namespace std;
using namespace commons;
#define MAX_NUMBER_OF_OPERATION_CLAUSES ((unsigned int) 100)
namespace query_engine {
class QueryAnswerElement {
public:
enum ElementType { UNDEFINED = 0, HANDLE, VARIABLE };
ElementType type;
unsigned int index;
string name;
QueryAnswerElement() : type(UNDEFINED) {}
QueryAnswerElement(unsigned int key) : type(HANDLE), index(key) {}
QueryAnswerElement(const string& key) : type(VARIABLE), name(key) {}
QueryAnswerElement(const QueryAnswerElement& other)
: type(other.type), index(other.index), name(other.name) {}
QueryAnswerElement& operator=(const QueryAnswerElement& other) {
this->type = other.type;
this->index = other.index;
this->name = other.name;
return *this;
}
string to_string() {
if (this->type == HANDLE) {
return "_" + std::to_string(this->index);
} else {
return "$" + this->name;
}
}
static QueryAnswerElement from_string(const string& string) {
if (string[0] == '_') {
return QueryAnswerElement(std::stoi(string.substr(1, string.size() - 1)));
} else {
return QueryAnswerElement(string.substr(1, string.size() - 1));
}
}
};
class QueryAnswer {
public:
vector<string> handles;
double importance;
double strength;
Assignment assignment;
map<string, string> metta_expression;
QueryAnswer(const string& handle, double importance);
QueryAnswer(double importance);
QueryAnswer();
~QueryAnswer();
void add_handle(const string& handle);
bool merge(QueryAnswer* other, bool merge_handles = true);
static QueryAnswer* copy(QueryAnswer* other);
const string& tokenize();
void untokenize(const string& tokens);
string to_string();
string get(const QueryAnswerElement& element_key, bool return_empty_when_not_found = false);
string get(const string& key, bool return_empty_when_not_found = false);
string get(unsigned int key, bool return_empty_when_not_found = false);
void rewrite_query(const vector<string>& original_query,
map<string, QueryAnswerElement>& replacements,
vector<string>& new_query);
private:
string token_representation;
};
}
template <>
struct std::hash<Assignment> {
std::size_t operator()(const Assignment& k) const {
if (k.table.size() == 0) {
return 0;
}
std::size_t hash_value = 1;
for (auto pair : k.table) {
hash_value =
hash_value ^
((std::hash<string>()(pair.first) ^ (std::hash<string>()(pair.second) << 1)) >> 1);
}
return hash_value;
}
};