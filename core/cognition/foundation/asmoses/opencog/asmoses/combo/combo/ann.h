#ifndef _COMBO_ANN_H
#define _COMBO_ANN_H
namespace opencog { namespace combo {
namespace id {
enum ann_id {
ann, ann_node, ann_input
};
}
typedef id::ann_id ann_id;
class ann_type {
public:
ann_type(unsigned int i, ann_id _id) : idx(i), id(_id) {}
unsigned int idx;
ann_id id;
bool operator<(ann_type rhs) const {
return idx < rhs.idx;
}
bool operator==(ann_type rhs) const {
return idx == rhs.idx;
}
bool operator!=(ann_type rhs) const {
return idx != rhs.idx;
}
};
}}
#endif