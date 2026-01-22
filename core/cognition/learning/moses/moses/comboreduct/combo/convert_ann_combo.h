#ifndef _COMBO_CONVERTER_ANN_COMBO_TREE_H
#define _COMBO_CONVERTER_ANN_COMBO_TREE_H
#include <moses/comboreduct/combo/simple_nn.h>
#include <moses/comboreduct/combo/vertex.h>
namespace opencog { namespace combo {
struct tree_transform {
tree_transform() {}
combo_tree encode_node(ann& the_ann, ann_node* node) const {
int tag = node->tag;
ann_id id;
if(node->nodetype == nodetype_input)
id=id::ann_input;
else
id=id::ann_node;
combo_tree tr(ann_type(tag, id));
if(node->visited)
{
return tr;
}
node->visited=true;
if(node->nodetype == nodetype_input)
{
if(!node->memory_ptr)
return tr;
bool been_visited = node->memory_ptr->visited;
node->memory_ptr->visited=true;
combo_tree encoded_tr = encode_node(the_ann, node->memory_ptr);
tr.insert_subtree(tr.begin().begin(), encoded_tr.begin());
node->memory_ptr->visited=been_visited;
return tr;
}
for(ann_connection_it cons = node->in_connections.begin();
cons != node->in_connections.end(); ++cons)
{
combo_tree tmp = encode_node(the_ann, (*cons)->source);
tr.insert_subtree(tr.begin().begin(), tmp.begin());
tr.insert_after(tr.begin().last_child(),(*cons)->weight);
}
return tr;
}
combo_tree encode_ann(ann& the_ann) const {
combo_tree tr(ann_type(0, id::ann));
the_ann.reset_visited();
for(ann_node_it node_it = the_ann.outputs.begin();
node_it != the_ann.outputs.end(); ++node_it)
{
combo_tree str = encode_node(the_ann, *node_it);
tr.insert_subtree(tr.begin().begin(),str.begin());
}
return tr;
}
ann decodify_tree(const combo_tree& tr) const {
OC_ASSERT(!tr.empty());
ann new_ann;
sib_it it = tr.begin();
OC_ASSERT(get_ann_type(*it).id == id::ann);
for (sib_it sib = it.begin(); sib != it.end(); ++sib) {
OC_ASSERT(get_ann_type(*sib).id == id::ann_node);
ann_node* newnode = new ann_node(nodetype_output,
get_ann_type(*sib).idx);
new_ann.add_node(newnode);
decodify_subtree(new_ann, newnode, sib);
}
return new_ann;
}
ann_node* decodify_node(ann& nn, sib_it sib) const {
ann_nodetype type;
ann_node* node = nn.find_tag(get_ann_type(*sib).idx);
if (get_ann_type(*sib).id == id::ann_node)
type = nodetype_hidden;
else
type = nodetype_input;
if (node == NULL) {
int id = get_ann_type(*sib).idx;
node = new ann_node(type, id);
nn.add_node(node);
}
if(type==nodetype_input)
{
ann_node* mem_ptr;
if (sib.has_one_child())
{
mem_ptr = decodify_node(nn,sib.begin());
node->memory_ptr = mem_ptr;
}
}
return node;
}
void decodify_subtree(ann& nn, ann_node* dest_node, sib_it it) const {
std::vector<ann_node*> sources;
sib_it sib;
int count = -1;
for (sib = it.begin(); sib != it.end(); ++sib) {
if (!is_ann_type(*sib))
break;
ann_node* node = decodify_node(nn, sib);
sources.push_back(node);
if (get_ann_type(*sib).id == id::ann_node)
decodify_subtree(nn, node, sib);
count++;
}
for ( ; sib != it.end(); ++sib) {
nn.add_connection(sources[count], dest_node, get_contin(*sib));
count--;
}
}
};
}}
#endif