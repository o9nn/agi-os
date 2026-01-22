#pragma once
#include <memory>
#include "AtomDBAPITypes.h"
#include "AtomDBSingleton.h"
#include "Context.h"
#include "Link.h"
#include "Node.h"
#include "PatternMatchingQueryProxy.h"
#include "ServiceBusSingleton.h"
#define IGNORE_ANSWER_COUNT 0
using namespace std;
using namespace atomdb;
using namespace atomdb_api_types;
using namespace commons;
using namespace service_bus;
using namespace query_engine;
namespace atom_space {
class AtomSpace {
public:
enum Scope { LOCAL_ONLY, REMOTE_ONLY, LOCAL_AND_REMOTE };
AtomSpace();
const Atom* get_atom(const char* handle, Scope scope = LOCAL_AND_REMOTE);
const Node* get_node(const string& type, const string& name, Scope scope = LOCAL_AND_REMOTE);
const Link* get_link(const string& type,
const vector<const Atom*>& targets,
Scope scope = LOCAL_AND_REMOTE);
shared_ptr<PatternMatchingQueryProxy> pattern_matching_query(
const vector<string>& query,
size_t answers_count = IGNORE_ANSWER_COUNT,
const string& context = "",
bool use_link_template_cache = false,
bool unique_assignment = true,
bool update_attention_broker = false,
bool positive_importance_only = false,
bool count_only = false);
size_t pattern_matching_count(const vector<string>& query,
const string& context = "",
bool unique_assignment = true,
bool update_attention_broker = false);
void pattern_matching_fetch(const vector<string>& query, size_t answers_count = IGNORE_ANSWER_COUNT);
char* add_node(const string& type,
const string& name,
bool is_toplevel = false,
const Properties& custom_attributes = {});
char* add_link(const string& type,
const vector<string>& targets,
bool is_toplevel = false,
const Properties& custom_attributes = {});
void commit_changes(Scope scope = LOCAL_AND_REMOTE);
shared_ptr<Context> create_context(const string& context_name, Atom& atom_key);
shared_ptr<Context> create_context(
const string& context_name,
const vector<string>& query,
const vector<pair<QueryAnswerElement, QueryAnswerElement>> determiner_schema,
vector<QueryAnswerElement> stimulus_schema);
shared_ptr<Context> create_context(const string& context_name);
protected:
shared_ptr<AtomDB> db;
Atom* atom_from_document(const shared_ptr<AtomDocument>& document);
private:
shared_ptr<ServiceBus> bus;
unique_ptr<HandleTrie> handle_trie;
};
}