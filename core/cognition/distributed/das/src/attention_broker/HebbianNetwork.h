#pragma once
#include <mutex>
#include <set>
#include <string>
#include "HandleTrie.h"
#include "Serializable.h"
#include "expression_hasher.h"
using namespace std;
using namespace commons;
namespace attention_broker {
typedef double ImportanceType;
class HebbianNetwork : public Serializable {
public:
HebbianNetwork();
~HebbianNetwork();
unsigned int largest_arity;
mutex largest_arity_mutex;
class Node : public HandleTrie::TrieValue {
public:
unsigned int arity;
unsigned int count;
ImportanceType importance;
ImportanceType stimuli_to_spread;
HandleTrie* neighbors;
set<Node*> determiners;
Node() {
arity = 0;
count = 1;
importance = 0.0;
stimuli_to_spread = 0.0;
neighbors = new HandleTrie(HANDLE_HASH_SIZE - 1);
}
inline void merge(HandleTrie::TrieValue* other) {
count += ((Node*) other)->count;
importance += ((Node*) other)->importance;
}
inline ImportanceType get_importance() {
ImportanceType answer = this->importance;
for (auto determiner : this->determiners) {
ImportanceType determiner_importance = determiner->get_importance();
if (determiner_importance > answer) {
answer = determiner_importance;
}
}
return answer;
}
string to_string();
};
class Edge : public HandleTrie::TrieValue {
public:
unsigned int count;
Node* node1;
Node* node2;
Edge() {
count = 1;
node1 = node2 = NULL;
}
inline void merge(HandleTrie::TrieValue* other) { count += ((Edge*) other)->count; }
string to_string();
};
Node* add_node(string handle);
Edge* add_asymmetric_edge(string handle1, string handle2, Node* node1, Node* node2);
void add_symmetric_edge(string handle1, string handle2, Node* node1, Node* node2);
Node* lookup_node(string handle);
unsigned int get_node_count(string handle);
ImportanceType get_node_importance(string handle);
unsigned int get_asymmetric_edge_count(string handle1, string handle2);
void visit_nodes(bool keep_root_locked,
bool (*visit_function)(HandleTrie::TrieNode* node, void* data),
void* data);
ImportanceType alienate_tokens();
void clear();
void serialize(ostream& os);
void deserialize(istream& is);
private:
void deserialize_node(istream& is, unsigned int& determiner_count, unsigned int& neighbors_count);
void init();
HandleTrie* nodes;
ImportanceType tokens_to_distribute;
string _handle;
mutex tokens_mutex;
};
}