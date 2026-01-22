#pragma once
#include <string>
#include <vector>
#include "Atom.h"
#include "QueryElement.h"
using namespace std;
using namespace query_engine;
using namespace atoms;
namespace query_element {
class Terminal : public QueryElement {
public:
bool is_variable;
bool is_link;
bool is_node;
bool is_atom;
string type;
string name;
string handle;
vector<shared_ptr<QueryElement>> targets;
virtual ~Terminal(){};
Terminal();
Terminal(const string& type, const string& name);
Terminal(const string& type, const vector<shared_ptr<QueryElement>>& targets);
Terminal(const string& name);
string to_string();
void setup_buffers() {}
void graceful_shutdown() {}
private:
void init();
};
}