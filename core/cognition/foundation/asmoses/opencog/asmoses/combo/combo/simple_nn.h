#ifndef _COMBO_SIMPLE_NN_H
#define _COMBO_SIMPLE_NN_H
#include <vector>
#include <utility>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <math.h>
#include <opencog/asmoses/utils/iostreamContainer.h>
#include <opencog/util/RandGen.h>
#include <opencog/asmoses/combo/combo/vertex.h>
namespace opencog { namespace combo {
class ann;
class ann_node;
class ann_connection;
typedef combo_tree::sibling_iterator sib_it;
typedef combo_tree::iterator pre_it;
struct compare_connection
{
bool operator() (ann_connection* lhs, ann_connection* rhs) const;
};
typedef std::vector<ann_node*>::iterator ann_node_it;
typedef std::vector<ann_connection*>::iterator ann_connection_it;
enum ann_nodetype { nodetype_input, nodetype_hidden, nodetype_output };
class ann_connection
{
public:
ann_connection(ann_node* s, ann_node* d, double w):
source(s), dest(d), weight(w) { }
ann_node* source;
ann_node* dest;
double weight;
friend std::ostream& operator<<(std::ostream& os, const ann_connection* a) {
os << "Connection with weight " << a->weight << std::endl;
return os;
}
};
class ann_node
{
public:
ann_node(ann_nodetype type, int _tag = 0, ann_node* _ptr=NULL)
: visited(false), memory_ptr(_ptr), activation(0.0), tag(_tag), nodetype(type) {}
bool visited;
int counter;
int id;
double sort_val;
ann_node* memory_ptr;
bool memory_neurone;
std::vector<ann_connection*> out_connections;
std::vector<ann_connection*> in_connections;
double activation;
double incoming;
int tag;
ann_nodetype nodetype;
void calculate_sort_value(void)
{
ann_connection_it it;
sort_val=0.0;
for(it=in_connections.begin();it!=in_connections.end();++it)
sort_val += fabs((*it)->weight);
for(it=out_connections.begin();it!=out_connections.end();++it)
sort_val += fabs((*it)->weight);
}
void sort_connections(void)
{
sort(in_connections.begin(),in_connections.end(),compare_connection());
}
friend std::ostream& operator<<(std::ostream& os, const ann_node* n) {
os << n->id << ":";
if (n->nodetype == nodetype_input) os << "input";
else if (n->nodetype == nodetype_hidden) os << "hidden";
else if (n->nodetype == nodetype_output) os << "output";
return os;
}
};
class ann
{
public:
std::vector<ann_node*> nodes;
std::vector<ann_node*> inputs;
std::vector<ann_node*> outputs;
std::vector<ann_node*> hidden;
std::vector<ann_connection*> connections;
ann() {}
~ann() {
ann_node_it iter;
for (iter = nodes.begin();iter != nodes.end();++iter)
delete (*iter);
}
void reduce()
{
ann_connection_it iter;
bool dirty=true;
while(dirty)
{
dirty = false;
for (iter = connections.begin();iter != connections.end(); ++iter)
{
if((*iter)->weight != 0.0)
continue;
if (!remove_connection(*iter))
continue;
iter = connections.begin();
if (iter==connections.end())
break;
}
ann_node_it node_iter;
for (node_iter = hidden.begin(); node_iter != hidden.end();
++node_iter)
{
if((*node_iter)->out_connections.size()==0
&& !(*node_iter)->memory_neurone)
{
remove_node(*node_iter);
node_iter = hidden.begin();
dirty=true;
if (node_iter==hidden.end())
break;
}
}
}
ann_node_it node_iter;
for (node_iter = nodes.begin(); node_iter != nodes.end();
++node_iter)
{
(*node_iter)->calculate_sort_value();
}
for (node_iter = nodes.begin(); node_iter != nodes.end();
++node_iter)
{
(*node_iter)->sort_connections();
}
}
void write_dot(const char* filename)
{
std::ofstream outfile(filename);
ann_connection_it it;
ann_node_it nit;
outfile << "digraph g { " << std::endl;
for(nit=inputs.begin();nit!=inputs.end();++nit)
{
outfile << "N" << (*nit)->tag << " [shape=box]" << std::endl;
}
for(nit=outputs.begin();nit!=outputs.end();++nit)
{
outfile << "N" << (*nit)->tag << " [shape=triangle]" << std::endl;
}
for(it=connections.begin();it!=connections.end();++it)
{
int n1 = (*it)->source->tag;
int n2 = (*it)->dest->tag;
outfile << "N" << n1 << " -> N" << n2 << " ";
if((*it)->weight > 0.3)
outfile << "[color=green] ";
else if ((*it)->weight < -0.3)
outfile << "[color=red] ";
outfile << std::endl;
}
for(nit=inputs.begin();nit!=inputs.end();++nit)
{
if((*nit)->memory_ptr)
{
int n1 = (*nit)->memory_ptr->tag;
int n2 = (*nit)->tag;
outfile << "N" << n1 << " -> N" << n2 << " [style=dotted] ";
outfile << std::endl;
}
}
outfile << " { rank=same; ";
for(nit=inputs.begin();nit!=inputs.end();++nit)
{
outfile << "N" << (*nit)->tag << " ";
}
outfile << " } " << std::endl;
outfile << " { rank=same; ";
for(nit=outputs.begin();nit!=outputs.end();++nit)
{
outfile << "N" << (*nit)->tag << " ";
}
outfile << " } " << std::endl;
outfile << "}" << std::endl;
}
bool add_new_hidden()
{
ann_node_it iter;
int tag = biggest_tag() + 1;
ann_node *new_hidden = new ann_node(nodetype_hidden,tag,NULL);
for(iter=outputs.begin();iter!=outputs.end();++iter)
{
add_connection(new_hidden,(*iter),0.0);
}
bool connected=false;
while(!connected)
{
int add_chance = 100;
for(iter=inputs.begin();iter!=inputs.end();++iter)
{
if (rand()%100 < add_chance)
{
connected=true;
add_connection((*iter),new_hidden,0.0);
}
}
for(iter=hidden.begin();iter!=hidden.end();++iter)
{
if (rand()%100 < add_chance)
{
connected=true;
add_connection((*iter),new_hidden,0.0);
}
}
}
add_node(new_hidden);
return true;
}
bool add_memory_input()
{
ann_node_it iter;
ann_node_it in_iter;
std::vector<ann_node*> possible;
for (iter = hidden.begin(); iter !=hidden.end();++iter)
{
bool memory=false;
for(in_iter = inputs.begin();in_iter!= inputs.end(); ++in_iter)
{
if(!(*in_iter)->memory_ptr)
continue;
if((*in_iter)->memory_ptr == (*iter)) {
memory=true;
break;
}
}
if (!memory)
{
possible.push_back(*iter);
}
}
if(possible.empty())
{
return false;
}
int selected = rand() % possible.size();
ann_node *hidden_neuron = possible[selected];
hidden_neuron->memory_neurone = true;
int tag = biggest_tag() + 2;
ann_node *new_input = new ann_node(nodetype_input,tag,hidden_neuron);
bool connected=false;
while(!connected)
{
int add_chance = 100;
for(iter=hidden.begin();iter!=hidden.end();++iter)
if (rand()%100 < add_chance)
{
connected=true;
add_connection(new_input,(*iter),0.0);
}
for(iter=outputs.begin();iter!=outputs.end();++iter)
if (rand()%100 < add_chance)
{
connected=true;
add_connection(new_input,(*iter),0.0);
}
}
add_node(new_input);
return true;
}
int biggest_tag()
{
int max = -1;
ann_node_it iter;
for(iter = nodes.begin();iter != nodes.end();++iter)
if ((*iter)->tag > max)
max = (*iter)->tag;
return max;
}
void reset_visited() {
ann_node_it iter;
for (iter = nodes.begin();iter != nodes.end();++iter)
(*iter)->visited=false;
}
ann_node* find_tag(int t) {
for(ann_node_it iter = nodes.begin(); iter != nodes.end(); ++iter)
if ((*iter)->tag == t)
return *iter;
return NULL;
}
double inline activation_fn_thresh(double incoming) const {
if (incoming >= 0.0)
return 1.0;
return -1.0;
}
double inline activation_fn(double incoming) const {
return 1.0 / (1.0 + exp(-incoming));
}
void load_inputs(double* vals) {
unsigned counter = 0;
for (unsigned x = 0;x < inputs.size();++x) {
if(inputs[x]->memory_ptr)
inputs[x]->activation = inputs[x]->memory_ptr->activation;
else
inputs[x]->activation = vals[counter++];
}
}
void load_inputs(const std::vector<double>& vals) {
unsigned counter = 0;
for (unsigned x = 0;x < inputs.size();++x) {
if(inputs[x]->memory_ptr)
inputs[x]->activation = inputs[x]->memory_ptr->activation;
else
inputs[x]->activation = vals[counter++];
}
}
void propagate() {
ann_node_it iter;
for (iter = nodes.begin();iter != nodes.end();++iter) {
if ((*iter)->nodetype == nodetype_input)
continue;
(*iter)->incoming = 0.0;
for (unsigned int y = 0;y < (*iter)->in_connections.size();++y)
(*iter)->incoming +=
(*iter)->in_connections[y]->weight *
(*iter)->in_connections[y]->source->activation;
(*iter)->activation = activation_fn((*iter)->incoming);
}
}
int feedforward_depth() {
for (unsigned int x = 0;x < nodes.size();++x)
nodes[x]->counter = 0;
for (unsigned int x = 0;x < inputs.size();++x)
feedforward_depth_recurse(inputs[x]);
int max_depth = 0;
for (unsigned int x = 0;x < outputs.size();++x)
if (outputs[x]->counter > max_depth)
max_depth = outputs[x]->counter;
return max_depth;
}
void feedforward_depth_recurse(ann_node* n) {
int depth = n->counter + 1;
for (unsigned int x = 0;x < n->out_connections.size();++x) {
ann_node* dest = n->out_connections[x]->dest;
int node_depth = dest->counter;
if (depth > node_depth) {
dest->counter = depth;
feedforward_depth_recurse(dest);
}
}
}
void add_connection(ann_node* s, ann_node* d, double weight) {
ann_connection* newconnection = new ann_connection(s, d, weight);
connections.push_back(newconnection);
s->out_connections.push_back(newconnection);
d->in_connections.push_back(newconnection);
}
void remove_from_vec(ann_node* n, std::vector<ann_node*> & l)
{
ann_node_it loc = find(l.begin(), l.end(), n);
if(loc!=l.end())
l.erase(loc);
}
void remove_from_vec(ann_connection* c, std::vector<ann_connection*>& l)
{
ann_connection_it loc = find(l.begin(), l.end(), c);
if(loc!=l.end())
l.erase(loc);
}
void delete_connections(std::vector<ann_connection*>& c)
{
for(ann_connection_it iter=c.begin(); iter!= c.end();)
{
remove_connection(*iter);
iter=c.begin();
}
}
bool remove_node(ann_node* node)
{
delete_connections(node->out_connections);
delete_connections(node->in_connections);
remove_from_vec(node,nodes);
remove_from_vec(node,inputs);
remove_from_vec(node,hidden);
remove_from_vec(node,outputs);
remove_from_memory_ptr(node);
delete node;
return true;
}
void remove_from_memory_ptr(ann_node* n)
{
for(ann_node_it it=inputs.begin(); it!=inputs.end(); ++it) {
if((*it)->memory_ptr == n)
(*it)->memory_ptr = NULL;
}
}
bool remove_connection(ann_connection* conn)
{
remove_from_vec(conn,connections);
remove_from_vec(conn,conn->source->out_connections);
remove_from_vec(conn,conn->dest->in_connections);
delete conn;
return true;
}
void add_node(ann_node* newnode) {
nodes.push_back(newnode);
if (newnode->nodetype == nodetype_input)
{
inputs.push_back(newnode);
}
else if (newnode->nodetype == nodetype_output)
{
outputs.push_back(newnode);
}
else if (newnode->nodetype == nodetype_hidden)
{
hidden.push_back(newnode);
}
}
friend std::ostream& operator<<(std::ostream& os, const ann *a) {
for (unsigned int x = 0;x < a->connections.size();++x)
std::cout << a->connections[x]->source->id << " -> " <<
a->connections[x]->dest->id << " : " <<
a->connections[x]->weight << std::endl;
return os;
}
};
}}
#endif