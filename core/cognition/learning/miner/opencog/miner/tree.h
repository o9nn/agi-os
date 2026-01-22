#ifndef _UTIL_TREE_H
#define _UTIL_TREE_H
#include <cassert>
#include <memory>
#include <stdexcept>
#include <iterator>
#include <set>
#include <queue>
#include <iostream>
#include <sstream>
#include <exception>
#include <boost/lexical_cast.hpp>
#include <boost/iterator/counting_iterator.hpp>
#include <opencog/util/oc_assert.h>
#include <opencog/util/exceptions.h>
#define tree_assert OC_ASSERT
namespace opencog {
namespace kp {
template <class T1, class T2>
void constructor(T1* p, T2& val)
{
new ((void *) p) T1(val);
}
template <class T1>
void constructor(T1* p)
{
new ((void *) p) T1;
}
template <class T1>
void destructor(T1* p)
{
p->~T1();
}
}
template<class T>
class tree_node_ {
public:
tree_node_<T> *parent;
tree_node_<T> *first_child, *last_child;
tree_node_<T> *prev_sibling, *next_sibling;
T data;
};
template <class T, class tree_node_allocator = std::allocator<tree_node_<T> > >
class tree {
protected:
typedef tree_node_<T> tree_node;
public:
typedef T value_type;
class iterator_base;
class pre_order_iterator;
class post_order_iterator;
class sibling_iterator;
class leaf_iterator;
class upwards_iterator;
tree();
tree(const T&);
explicit tree(const iterator_base&);
explicit tree(const std::initializer_list<T>&);
explicit tree(const T&, const std::initializer_list<tree<T, tree_node_allocator>>&);
explicit tree(const std::initializer_list<tree<T, tree_node_allocator>>&);
tree(const tree<T, tree_node_allocator>&);
~tree();
tree<T, tree_node_allocator>& operator=(const tree<T, tree_node_allocator>&);
#ifdef __SGI_STL_PORT
class iterator_base : public stlport::bidirectional_iterator<T, ptrdiff_t>
#else
class iterator_base
#endif
{
public:
typedef T value_type;
typedef T* pointer;
typedef T& reference;
typedef size_t size_type;
typedef ptrdiff_t difference_type;
typedef std::bidirectional_iterator_tag iterator_category;
typedef typename tree<T,tree_node_allocator>::sibling_iterator sibling_iterator;
iterator_base();
iterator_base(tree_node *);
T& operator*() const;
T* operator->() const;
void skip_children();
unsigned int number_of_children() const;
bool is_childless() const {
return (node->first_child==NULL);
}
bool has_one_child() const {
return (node->first_child!=NULL &&
node->first_child==node->last_child);
}
sibling_iterator begin() const;
sibling_iterator end() const;
sibling_iterator last_child() const;
tree_node *node;
sibling_iterator find_child(const T& t) const {
sibling_iterator sib=begin();
for (;sib!=end();++sib)
if (*sib==t)
break;
return sib;
}
protected:
bool skip_current_children_;
};
class pre_order_iterator : public iterator_base {
public:
pre_order_iterator();
pre_order_iterator(tree_node *);
pre_order_iterator(const iterator_base&);
pre_order_iterator(const sibling_iterator&);
bool operator==(const pre_order_iterator&) const;
bool operator!=(const pre_order_iterator&) const;
pre_order_iterator& operator++();
pre_order_iterator& operator--();
pre_order_iterator operator++(int);
pre_order_iterator operator--(int);
pre_order_iterator& operator+=(unsigned int);
pre_order_iterator& operator-=(unsigned int);
};
class post_order_iterator : public iterator_base {
public:
post_order_iterator();
post_order_iterator(tree_node *);
post_order_iterator(const iterator_base&);
post_order_iterator(const sibling_iterator&);
bool operator==(const post_order_iterator&) const;
bool operator!=(const post_order_iterator&) const;
post_order_iterator& operator++();
post_order_iterator& operator--();
post_order_iterator operator++(int);
post_order_iterator operator--(int);
post_order_iterator& operator+=(unsigned int);
post_order_iterator& operator-=(unsigned int);
void descend_all();
};
class breadth_first_queued_iterator : public iterator_base {
public:
breadth_first_queued_iterator();
breadth_first_queued_iterator(tree_node *);
breadth_first_queued_iterator(const iterator_base&);
bool operator==(const breadth_first_queued_iterator&) const;
bool operator!=(const breadth_first_queued_iterator&) const;
breadth_first_queued_iterator& operator++();
breadth_first_queued_iterator operator++(int);
breadth_first_queued_iterator& operator+=(unsigned int);
private:
std::queue<tree_node *> traversal_queue;
};
typedef pre_order_iterator iterator;
typedef breadth_first_queued_iterator breadth_first_iterator;
class fixed_depth_iterator : public iterator_base {
public:
fixed_depth_iterator();
fixed_depth_iterator(tree_node *);
fixed_depth_iterator(const iterator_base&);
fixed_depth_iterator(const sibling_iterator&);
fixed_depth_iterator(const fixed_depth_iterator&);
bool operator==(const fixed_depth_iterator&) const;
bool operator!=(const fixed_depth_iterator&) const;
fixed_depth_iterator& operator++();
fixed_depth_iterator& operator--();
fixed_depth_iterator operator++(int);
fixed_depth_iterator operator--(int);
fixed_depth_iterator& operator+=(unsigned int);
fixed_depth_iterator& operator-=(unsigned int);
tree_node *first_parent_;
private:
void set_first_parent_();
void find_leftmost_parent_();
};
class sibling_iterator : public iterator_base {
public:
sibling_iterator();
sibling_iterator(tree_node *);
sibling_iterator(const sibling_iterator&);
sibling_iterator(const iterator_base&);
bool operator==(const sibling_iterator&) const;
bool operator!=(const sibling_iterator&) const;
sibling_iterator& operator++();
sibling_iterator& operator--();
sibling_iterator operator++(int);
sibling_iterator operator--(int);
sibling_iterator& operator+=(unsigned int);
sibling_iterator& operator-=(unsigned int);
friend class tree;
friend class iterator_base;
friend class pre_order_iterator;
protected:
tree_node *range_first() const;
tree_node *range_last() const;
tree_node *parent_;
};
class leaf_iterator : public iterator_base {
public:
leaf_iterator();
leaf_iterator(tree_node *);
leaf_iterator(const sibling_iterator&);
leaf_iterator(const iterator_base&);
bool operator==(const leaf_iterator&) const;
bool operator!=(const leaf_iterator&) const;
leaf_iterator& operator++();
leaf_iterator& operator--();
leaf_iterator operator++(int);
leaf_iterator operator--(int);
leaf_iterator& operator+=(unsigned int);
leaf_iterator& operator-=(unsigned int);
};
class upwards_iterator : public iterator_base {
public:
upwards_iterator();
upwards_iterator(tree_node *);
upwards_iterator(const sibling_iterator&);
upwards_iterator(const iterator_base&);
bool operator==(const upwards_iterator&) const;
bool operator!=(const upwards_iterator&) const;
upwards_iterator& operator++();
upwards_iterator operator++(int);
upwards_iterator& operator+=(unsigned int);
upwards_iterator& operator-=(unsigned int);
};
inline pre_order_iterator begin() const;
inline pre_order_iterator end() const;
post_order_iterator begin_post() const;
post_order_iterator end_post() const;
fixed_depth_iterator begin_fixed(const iterator_base&, unsigned int) const;
fixed_depth_iterator end_fixed(const iterator_base&, unsigned int) const;
breadth_first_queued_iterator begin_breadth_first() const;
breadth_first_queued_iterator end_breadth_first() const;
sibling_iterator begin(const iterator_base&) const;
sibling_iterator end(const iterator_base&) const;
leaf_iterator begin_leaf() const;
leaf_iterator end_leaf() const;
template<typename iter>
upwards_iterator begin_upwards(iter it) const { return upwards_iterator(it); }
upwards_iterator end_upwards() const { return upwards_iterator(NULL); }
template<typename iter> iter parent(iter) const;
template<typename iter> iter last_child(iter) const;
template<typename iter> iter previous_sibling(iter) const;
template<typename iter> iter next_sibling(iter) const;
template<typename iter> iter next_at_same_depth(iter) const;
void clear();
template<typename iter> iter erase(iter);
void erase_children(const iterator_base&);
template<typename iter> iter append_child(iter position);
template<typename iter> iter prepend_child(iter position);
template<typename iter> iter append_children(iter position,int n);
template<typename iter> iter prepend_children(iter position,int n);
template<typename iter> iter append_child(iter position, const T& x);
template<typename iter> iter prepend_child(iter position, const T& x);
template<typename iter> iter append_children(iter position, const T& x,int n);
template<typename iter> iter prepend_children(iter position, const T& x,int n);
template<typename iter> iter append_children(iter position, const std::initializer_list<T>& il);
template<typename iter> iter prepend_children(iter position, const std::initializer_list<T>& il);
template<typename iter> iter append_child(iter position, iter other_position);
template<typename iter> iter prepend_child(iter position, iter other_position);
template<typename iter> iter append_children(iter position, sibling_iterator from, sibling_iterator to);
template<typename iter> iter prepend_children(iter position, sibling_iterator from, sibling_iterator to);
pre_order_iterator set_head(const T& x);
template<typename iter> iter insert(iter position, const T& x);
sibling_iterator insert(sibling_iterator position, const T& x);
template<typename iter> iter insert_subtree(iter position, const iterator_base& subtree);
template<typename iter> iter insert_subtree_after(iter position, const iterator_base& subtree);
template<typename iter> iter insert_after(iter position, const T& x);
template<typename iter> iter insert_above(iter position, const T& x);
template<typename iter> iter replace(iter position, const T& x);
template<typename iter> iter replace(iter position, const iterator_base& from);
sibling_iterator replace(sibling_iterator orig_begin, sibling_iterator orig_end,
sibling_iterator new_begin, sibling_iterator new_end);
template<typename iter> iter flatten(iter position);
template<typename iter> iter reparent(iter position, sibling_iterator begin, sibling_iterator end);
template<typename iter> iter reparent(iter position, iter from);
template<typename iter> iter wrap(iter position, const T& x);
template<typename iter> iter move_after(iter target, iter source);
template<typename iter> iter move_before(iter target, iter source);
sibling_iterator move_before(sibling_iterator target, sibling_iterator source);
template<typename iter> iter move_ontop(iter target, iter source);
void merge(sibling_iterator, sibling_iterator, sibling_iterator, sibling_iterator,
bool duplicate_leaves=false);
void sort(sibling_iterator from, sibling_iterator to, bool deep=false);
template<class StrictWeakOrdering>
void sort(sibling_iterator from, sibling_iterator to, StrictWeakOrdering comp, bool deep=false);
template<class Predicate>
sibling_iterator partition(sibling_iterator from, sibling_iterator to,Predicate comp);
template<class StrictWeakOrdering>
void sort_on_subtrees(sibling_iterator from, sibling_iterator to, StrictWeakOrdering comp, bool deep=false);
template<typename iter>
bool equal(const iter& one, const iter& two, const iter& three) const;
template<typename iter, class BinaryPredicate>
bool equal(const iter& one, const iter& two, const iter& three, BinaryPredicate) const;
template<typename iter>
bool equal_subtree(const iter& one, const iter& two) const;
template<typename iter, class BinaryPredicate>
bool equal_subtree(const iter& one, const iter& two, BinaryPredicate) const;
tree subtree(sibling_iterator from, sibling_iterator to) const;
void subtree(tree&, sibling_iterator from, sibling_iterator to) const;
void swap(sibling_iterator it);
void swap(iterator, iterator);
int size() const;
int subtree_size(const iterator_base& it) const;
bool empty() const;
int depth(const iterator_base&) const;
unsigned int number_of_children(const iterator_base&) const;
unsigned int number_of_siblings(const iterator_base&) const;
bool is_in_subtree(const iterator_base& position, const iterator_base& begin,
const iterator_base& end) const;
pre_order_iterator find_subtree(const iterator_base& it, const iterator_base& begin,
const iterator_base& end) const;
bool is_valid(const iterator_base&) const;
void validate(const iterator_base& it) const;
void validate() const { validate(begin()); }
int max_depth(const iterator_base& it) const {
int d=-1;
for (sibling_iterator sib=it.begin();sib!=it.end();++sib)
d=std::max(d,max_depth(sib));
return d+1;
}
int max_branching(const iterator_base& it) const {
int b=0,i=0;
for (sibling_iterator sib=it.begin();sib!=it.end();++sib,++i)
b=std::max(b,max_branching(sib));
return std::max(b,i);
}
unsigned int sibling_index(sibling_iterator it) const;
sibling_iterator child(const iterator_base& position, unsigned int) const;
class iterator_base_less {
public:
bool operator()(const typename tree<T, tree_node_allocator>::iterator_base& one,
const typename tree<T, tree_node_allocator>::iterator_base& two) const
{
return one.node < two.node;
}
};
tree_node *head, *feet;
private:
tree_node_allocator alloc_;
void head_initialise_();
void copy_(const tree<T, tree_node_allocator>& other);
template<class StrictWeakOrdering>
class compare_nodes {
public:
compare_nodes(StrictWeakOrdering comp) : comp_(comp) {};
bool operator()(const tree_node *a, const tree_node *b) const
{
return comp_(a->data, b->data);
}
private:
StrictWeakOrdering comp_;
};
template<class StrictWeakOrdering>
class compare_nodes_pre_it {
public:
compare_nodes_pre_it(StrictWeakOrdering comp) : comp_(comp) {};
bool operator()(const tree_node *a, const tree_node *b) const
{
return comp_(pre_order_iterator(const_cast<tree_node*>(a)),
pre_order_iterator(const_cast<tree_node*>(b)));
}
private:
StrictWeakOrdering comp_;
};
};
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::tree()
{
head_initialise_();
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::tree(const T& x)
{
head_initialise_();
set_head(x);
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::tree(const iterator_base& other)
{
head_initialise_();
set_head((*other));
replace(begin(), other);
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::tree(const std::initializer_list<T>& roots)
{
head_initialise_();
if (roots.size() == 0)
return;
auto it = roots.begin();
auto sib = set_head(*it);
++it;
for (; it != roots.end(); ++it)
sib = insert_after(sib, *it);
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::tree(const T& root, const std::initializer_list<tree<T, tree_node_allocator>>& subtrees)
{
head_initialise_();
iterator root_it = set_head(root);
for (const auto& subtree : subtrees) {
iterator it = subtree.begin();
while (subtree.is_valid(it)) {
append_child(root_it, it);
it = subtree.next_sibling(it);
}
}
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::tree(const std::initializer_list<tree<T, tree_node_allocator>>& trees)
{
head_initialise_();
if (trees.size() == 0)
return;
auto tree_it = trees.begin();
for (; tree_it != trees.end() and empty(); ++tree_it)
if (not tree_it->empty())
copy_(*tree_it);
auto root_it = begin();
for (; tree_it != trees.end(); ++tree_it) {
if (!tree_it->empty()) {
iterator it = tree_it->begin();
while (tree_it->is_valid(it)) {
root_it = insert_subtree_after(root_it, it);
it = tree_it->next_sibling(it);
}
}
}
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::~tree()
{
clear();
alloc_.deallocate(head,1);
alloc_.deallocate(feet,1);
}
template <class T, class tree_node_allocator>
void tree<T, tree_node_allocator>::head_initialise_()
{
head = alloc_.allocate(1,0);
feet = alloc_.allocate(1,0);
head->parent=0;
head->first_child=0;
head->last_child=0;
head->prev_sibling=0;
head->next_sibling=feet;
feet->parent=0;
feet->first_child=0;
feet->last_child=0;
feet->prev_sibling=head;
feet->next_sibling=0;
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>& tree<T, tree_node_allocator>::operator=(const tree<T, tree_node_allocator>& other)
{
if (this!=&other)
copy_(other);
return *this;
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::tree(const tree<T, tree_node_allocator>& other)
{
head_initialise_();
copy_(other);
}
template <class T, class tree_node_allocator>
void tree<T, tree_node_allocator>::copy_(const tree<T, tree_node_allocator>& other)
{
clear();
pre_order_iterator it=other.begin(), to=begin();
while(it!=other.end()) {
to=insert(to, (*it));
it.skip_children();
++it;
}
to=begin();
it=other.begin();
while(it!=other.end()) {
to=replace(to, it);
to.skip_children();
it.skip_children();
++to;
++it;
}
}
template <class T, class tree_node_allocator>
void tree<T, tree_node_allocator>::clear()
{
if(head)
while(head->next_sibling!=feet)
erase(pre_order_iterator(head->next_sibling));
}
template<class T, class tree_node_allocator>
void tree<T, tree_node_allocator>::erase_children(const iterator_base& it)
{
tree_node *cur=it.node->first_child;
tree_node *prev=0;
while(cur!=0) {
prev=cur;
cur=cur->next_sibling;
erase_children(pre_order_iterator(prev));
kp::destructor(&prev->data);
alloc_.deallocate(prev,1);
}
it.node->first_child=0;
it.node->last_child=0;
}
template<class T, class tree_node_allocator>
template<class iter>
iter tree<T, tree_node_allocator>::erase(iter it)
{
tree_node *cur=it.node;
tree_assert(cur!=head);
iter ret=it;
ret.skip_children();
++ret;
erase_children(it);
if(cur->prev_sibling==0) {
cur->parent->first_child=cur->next_sibling;
}
else {
cur->prev_sibling->next_sibling=cur->next_sibling;
}
if(cur->next_sibling==0) {
cur->parent->last_child=cur->prev_sibling;
}
else {
cur->next_sibling->prev_sibling=cur->prev_sibling;
}
kp::destructor(&cur->data);
alloc_.deallocate(cur,1);
return ret;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::pre_order_iterator tree<T, tree_node_allocator>::begin() const
{
return pre_order_iterator(head->next_sibling);
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::pre_order_iterator tree<T, tree_node_allocator>::end() const
{
return pre_order_iterator(feet);
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::breadth_first_queued_iterator tree<T, tree_node_allocator>::begin_breadth_first() const
{
return breadth_first_queued_iterator(head->next_sibling);
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::breadth_first_queued_iterator tree<T, tree_node_allocator>::end_breadth_first() const
{
return breadth_first_queued_iterator();
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::post_order_iterator tree<T, tree_node_allocator>::begin_post() const
{
tree_node *tmp=head->next_sibling;
if(tmp!=feet) {
while(tmp->first_child)
tmp=tmp->first_child;
}
return post_order_iterator(tmp);
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::post_order_iterator tree<T, tree_node_allocator>::end_post() const
{
return post_order_iterator(feet);
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::fixed_depth_iterator tree<T, tree_node_allocator>::begin_fixed(const iterator_base& pos, unsigned int dp) const
{
tree_node *tmp=pos.node;
unsigned int curdepth=0;
while(curdepth<dp) {
while(tmp->first_child==0) {
tmp=tmp->next_sibling;
if(tmp==0)
throw std::range_error("tree: begin_fixed out of range");
}
tmp=tmp->first_child;
++curdepth;
}
return tmp;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::fixed_depth_iterator tree<T, tree_node_allocator>::end_fixed(const iterator_base& pos, unsigned int dp) const
{
tree_assert(1==0);
tree_node *tmp=pos.node;
unsigned int curdepth=1;
while(curdepth<dp) {
while(tmp->first_child==0) {
tmp=tmp->next_sibling;
if(tmp==0)
throw std::range_error("tree: end_fixed out of range");
}
tmp=tmp->first_child;
++curdepth;
}
return tmp;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::sibling_iterator tree<T, tree_node_allocator>::begin(const iterator_base& pos) const
{
tree_assert(pos.node!=0);
if(pos.node->first_child==0) {
return end(pos);
}
return pos.node->first_child;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::sibling_iterator tree<T, tree_node_allocator>::end(const iterator_base& pos) const
{
sibling_iterator ret(0);
ret.parent_=pos.node;
return ret;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::leaf_iterator tree<T, tree_node_allocator>::begin_leaf() const
{
tree_node *tmp=head->next_sibling;
if(tmp!=feet) {
while(tmp->first_child)
tmp=tmp->first_child;
}
return leaf_iterator(tmp);
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::leaf_iterator tree<T, tree_node_allocator>::end_leaf() const
{
return leaf_iterator(feet);
}
template <class T, class tree_node_allocator>
template <typename iter>
iter tree<T, tree_node_allocator>::parent(iter position) const
{
tree_assert(position.node!=0);
return iter(position.node->parent);
}
template <class T, class tree_node_allocator>
template <typename iter>
iter tree<T, tree_node_allocator>::last_child(iter position) const
{
tree_assert(position.node!=0);
return iter(position.node->last_child);
}
template <class T, class tree_node_allocator>
template <typename iter>
iter tree<T, tree_node_allocator>::previous_sibling(iter position) const
{
tree_assert(position.node!=0);
iter ret(position);
ret.node=position.node->prev_sibling;
return ret;
}
template <class T, class tree_node_allocator>
template <typename iter>
iter tree<T, tree_node_allocator>::next_sibling(iter position) const
{
tree_assert(position.node!=0);
iter ret(position);
ret.node=position.node->next_sibling;
return ret;
}
template <class T, class tree_node_allocator>
template <typename iter>
iter tree<T, tree_node_allocator>::next_at_same_depth(iter position) const
{
tree_assert(position.node!=0);
iter ret(position);
if(position.node->next_sibling) {
ret.node=position.node->next_sibling;
}
else {
int relative_depth=0;
upper:
do {
ret.node=ret.node->parent;
if(ret.node==0) return ret;
--relative_depth;
} while(ret.node->next_sibling==0);
lower:
ret.node=ret.node->next_sibling;
while(ret.node->first_child==0) {
if(ret.node->next_sibling==0)
goto upper;
ret.node=ret.node->next_sibling;
if(ret.node==0) return ret;
}
while(relative_depth<0 && ret.node->first_child!=0) {
ret.node=ret.node->first_child;
++relative_depth;
}
if(relative_depth<0) {
if(ret.node->next_sibling==0) goto upper;
else goto lower;
}
}
return ret;
}
template <class T, class tree_node_allocator>
template <typename iter>
iter tree<T, tree_node_allocator>::append_child(iter position)
{
tree_assert(position.node!=head);
tree_assert(position.node);
tree_node *tmp=alloc_.allocate(1,0);
kp::constructor(&tmp->data);
tmp->first_child=0;
tmp->last_child=0;
tmp->parent=position.node;
if(position.node->last_child!=0) {
position.node->last_child->next_sibling=tmp;
}
else {
position.node->first_child=tmp;
}
tmp->prev_sibling=position.node->last_child;
position.node->last_child=tmp;
tmp->next_sibling=0;
return tmp;
}
template <class T, class tree_node_allocator>
template <typename iter>
iter tree<T, tree_node_allocator>::prepend_child(iter position)
{
tree_assert(position.node!=head);
tree_assert(position.node);
tree_node *tmp=alloc_.allocate(1,0);
kp::constructor(&tmp->data);
tmp->first_child=0;
tmp->last_child=0;
tmp->parent=position.node;
if(position.node->first_child!=0) {
position.node->first_child->prev_sibling=tmp;
}
else {
position.node->last_child=tmp;
}
tmp->next_sibling=position.node->first_child;
position.node->first_child=tmp;
tmp->prev_sibling=0;
return tmp;
}
template <class T, class tree_node_allocator>
template <class iter>
iter tree<T, tree_node_allocator>::append_children(iter position, int n) {
while ((n--)>0)
append_child(position);
return iter(position.node->last_child);
}
template <class T, class tree_node_allocator>
template <class iter>
iter tree<T, tree_node_allocator>::prepend_children(iter position, int n) {
iter res=position;
while ((n--)>0)
res=prepend_child(position);
return res;
}
template <class T, class tree_node_allocator>
template <class iter>
iter tree<T, tree_node_allocator>::append_child(iter position, const T& x)
{
tree_assert(position.node!=head);
tree_assert(position.node);
tree_node* tmp = alloc_.allocate(1,0);
kp::constructor(&tmp->data, x);
tmp->first_child=0;
tmp->last_child=0;
tmp->parent=position.node;
if(position.node->last_child!=0) {
position.node->last_child->next_sibling=tmp;
}
else {
position.node->first_child=tmp;
}
tmp->prev_sibling=position.node->last_child;
position.node->last_child=tmp;
tmp->next_sibling=0;
return tmp;
}
template <class T, class tree_node_allocator>
template <class iter>
iter tree<T, tree_node_allocator>::prepend_child(iter position, const T& x)
{
tree_assert(position.node!=head);
tree_assert(position.node);
tree_node* tmp = alloc_.allocate(1,0);
kp::constructor(&tmp->data, x);
tmp->first_child=0;
tmp->last_child=0;
tmp->parent=position.node;
if(position.node->first_child!=0) {
position.node->first_child->prev_sibling=tmp;
}
else {
position.node->last_child=tmp;
}
tmp->next_sibling=position.node->first_child;
position.node->first_child=tmp;
tmp->prev_sibling=0;
return tmp;
}
template <class T, class tree_node_allocator>
template <class iter>
iter tree<T, tree_node_allocator>::append_children(iter position, const T& x,int n) {
while ((n--)>0)
append_child(position,x);
return iter(position.node->last_child);
}
template <class T, class tree_node_allocator>
template <class iter>
iter tree<T, tree_node_allocator>::prepend_children(iter position, const T& x,int n) {
iter res=position;
while ((n--)>0)
res=prepend_child(position,x);
return res;
}
template <class T, class tree_node_allocator>
template <class iter>
iter tree<T, tree_node_allocator>::append_children(iter position, const std::initializer_list<T>& il) {
for (const T& x : il)
append_child(position,x);
return iter(position.node->last_child);
}
template <class T, class tree_node_allocator>
template <class iter>
iter tree<T, tree_node_allocator>::prepend_children(iter position, const std::initializer_list<T>& il) {
iter res=position;
for (const T& x : il)
res=prepend_child(position,x);
return res;
}
template <class T, class tree_node_allocator>
template <class iter>
iter tree<T, tree_node_allocator>::append_child(iter position, iter other)
{
tree_assert(position.node!=head);
tree_assert(position.node);
sibling_iterator aargh=append_child(position, value_type());
return replace(aargh, other);
}
template <class T, class tree_node_allocator>
template <class iter>
iter tree<T, tree_node_allocator>::prepend_child(iter position, iter other)
{
tree_assert(position.node!=head);
tree_assert(position.node);
sibling_iterator aargh=prepend_child(position, value_type());
return replace(aargh, other);
}
template <class T, class tree_node_allocator>
template <class iter>
iter tree<T, tree_node_allocator>::append_children(iter position, sibling_iterator from, sibling_iterator to)
{
tree_assert(position.node!=head);
tree_assert(position.node);
iter ret=from;
while(from!=to) {
insert_subtree(position.end(), from);
++from;
}
return ret;
}
template <class T, class tree_node_allocator>
template <class iter>
iter tree<T, tree_node_allocator>::prepend_children(iter position, sibling_iterator from, sibling_iterator to)
{
tree_assert(position.node!=head);
tree_assert(position.node);
iter ret=from;
while(from!=to) {
insert_subtree(position.begin(), from);
++from;
}
return ret;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::pre_order_iterator tree<T, tree_node_allocator>::set_head(const T& x)
{
tree_assert(head->next_sibling==feet);
return insert(iterator(feet), x);
}
template <class T, class tree_node_allocator>
template <class iter>
iter tree<T, tree_node_allocator>::insert(iter position, const T& x)
{
if(position.node==0) {
position.node=feet;
}
tree_node* tmp = alloc_.allocate(1,0);
kp::constructor(&tmp->data, x);
tmp->first_child=0;
tmp->last_child=0;
tmp->parent=position.node->parent;
tmp->next_sibling=position.node;
tmp->prev_sibling=position.node->prev_sibling;
position.node->prev_sibling=tmp;
if(tmp->prev_sibling==0) {
if(tmp->parent)
tmp->parent->first_child=tmp;
}
else
tmp->prev_sibling->next_sibling=tmp;
return tmp;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::sibling_iterator tree<T, tree_node_allocator>::insert(sibling_iterator position, const T& x)
{
tree_node* tmp = alloc_.allocate(1,0);
kp::constructor(&tmp->data, x);
tmp->first_child=0;
tmp->last_child=0;
tmp->next_sibling=position.node;
if(position.node==0) {
tmp->parent=position.parent_;
tmp->prev_sibling=position.range_last();
tmp->parent->last_child=tmp;
}
else {
tmp->parent=position.node->parent;
tmp->prev_sibling=position.node->prev_sibling;
position.node->prev_sibling=tmp;
}
if(tmp->prev_sibling==0) {
if(tmp->parent)
tmp->parent->first_child=tmp;
}
else
tmp->prev_sibling->next_sibling=tmp;
return tmp;
}
template <class T, class tree_node_allocator>
template <class iter>
iter tree<T, tree_node_allocator>::insert_after(iter position, const T& x)
{
tree_node* tmp = alloc_.allocate(1,0);
kp::constructor(&tmp->data, x);
tmp->first_child=0;
tmp->last_child=0;
tmp->parent=position.node->parent;
tmp->prev_sibling=position.node;
tmp->next_sibling=position.node->next_sibling;
position.node->next_sibling=tmp;
if(tmp->next_sibling==0) {
if(tmp->parent)
tmp->parent->last_child=tmp;
}
else {
tmp->next_sibling->prev_sibling=tmp;
}
return tmp;
}
template <class T, class tree_node_allocator>
template <class iter>
iter tree<T, tree_node_allocator>::insert_above(iter position, const T& x) {
tree_node *dst=position.node;
tree_node* tmp = alloc_.allocate(1,0);
kp::constructor(&tmp->data, x);
tmp->first_child=dst;
tmp->last_child=dst;
tmp->parent=dst->parent;
tmp->prev_sibling=dst->prev_sibling;
tmp->next_sibling=dst->next_sibling;
if (tmp->prev_sibling) {
tmp->prev_sibling->next_sibling=tmp;
} else if (tmp->parent) {
tmp->parent->first_child=tmp;
}
if (tmp->next_sibling) {
tmp->next_sibling->prev_sibling=tmp;
} else if (tmp->parent) {
tmp->parent->last_child=tmp;
}
dst->parent=tmp;
dst->prev_sibling=0;
dst->next_sibling=0;
return tmp;
}
template <class T, class tree_node_allocator>
template <class iter>
iter tree<T, tree_node_allocator>::insert_subtree(iter position, const iterator_base& subtree)
{
iter it=insert(position, value_type());
return replace(it, subtree);
}
template <class T, class tree_node_allocator>
template <class iter>
iter tree<T, tree_node_allocator>::insert_subtree_after(iter position, const iterator_base& subtree)
{
iter it=insert_after(position, value_type());
return replace(it, subtree);
}
template <class T, class tree_node_allocator>
template <class iter>
iter tree<T, tree_node_allocator>::replace(iter position, const T& x)
{
kp::destructor(&position.node->data);
kp::constructor(&position.node->data, x);
return position;
}
template <class T, class tree_node_allocator>
template <class iter>
iter tree<T, tree_node_allocator>::replace(iter position, const iterator_base& from)
{
tree_assert(position.node!=head);
if (position==from)
return position;
tree_node *current_from=from.node;
tree_node *start_from=from.node;
tree_node *current_to =position.node;
erase_children(position);
tree_node* tmp = alloc_.allocate(1,0);
kp::constructor(&tmp->data, (*from));
tmp->first_child=0;
tmp->last_child=0;
if(current_to->prev_sibling==0) {
current_to->parent->first_child=tmp;
}
else {
current_to->prev_sibling->next_sibling=tmp;
}
tmp->prev_sibling=current_to->prev_sibling;
if(current_to->next_sibling==0) {
current_to->parent->last_child=tmp;
}
else {
current_to->next_sibling->prev_sibling=tmp;
}
tmp->next_sibling=current_to->next_sibling;
tmp->parent=current_to->parent;
kp::destructor(&current_to->data);
alloc_.deallocate(current_to,1);
current_to=tmp;
tree_node *last=from.node->next_sibling;
pre_order_iterator toit=tmp;
do {
tree_assert(current_from!=0);
if(current_from->first_child != 0) {
current_from=current_from->first_child;
toit=append_child(toit, current_from->data);
}
else {
while(current_from->next_sibling==0 && current_from!=start_from) {
current_from=current_from->parent;
toit=parent(toit);
tree_assert(current_from!=0);
}
current_from=current_from->next_sibling;
if(current_from!=last) {
toit=append_child(parent(toit), current_from->data);
}
}
} while(current_from!=last);
return current_to;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::sibling_iterator tree<T, tree_node_allocator>::replace(
sibling_iterator orig_begin,
sibling_iterator orig_end,
sibling_iterator new_begin,
sibling_iterator new_end)
{
tree_node *orig_first=orig_begin.node;
tree_node *new_first=new_begin.node;
tree_node *orig_last=orig_first;
while((++orig_begin)!=orig_end)
orig_last=orig_last->next_sibling;
tree_node *new_last=new_first;
while((++new_begin)!=new_end)
new_last=new_last->next_sibling;
bool first=true;
pre_order_iterator ret;
while(true) {
pre_order_iterator tt=insert_subtree(pre_order_iterator(orig_first), pre_order_iterator(new_first));
if(first) {
ret=tt;
first=false;
}
if(new_first==new_last)
break;
new_first=new_first->next_sibling;
}
bool last=false;
tree_node *next=orig_first;
while(true) {
if(next==orig_last)
last=true;
next=next->next_sibling;
erase((pre_order_iterator)orig_first);
if(last)
break;
orig_first=next;
}
return ret;
}
template <class T, class tree_node_allocator>
template <typename iter>
iter tree<T, tree_node_allocator>::flatten(iter position)
{
if(position.node->first_child==0)
return position;
tree_node *tmp=position.node->first_child;
while(tmp) {
tmp->parent=position.node->parent;
tmp=tmp->next_sibling;
}
if(position.node->next_sibling) {
position.node->last_child->next_sibling=position.node->next_sibling;
position.node->next_sibling->prev_sibling=position.node->last_child;
}
else {
position.node->parent->last_child=position.node->last_child;
}
position.node->next_sibling=position.node->first_child;
position.node->next_sibling->prev_sibling=position.node;
position.node->first_child=0;
position.node->last_child=0;
return position;
}
template <class T, class tree_node_allocator>
template <typename iter>
iter tree<T, tree_node_allocator>::reparent(iter position, sibling_iterator begin, sibling_iterator end)
{
tree_node *first=begin.node;
tree_node *last=first;
tree_assert(first!=position.node);
if(begin==end) return begin;
while((++begin)!=end) {
last=last->next_sibling;
}
if(first->prev_sibling==0) {
first->parent->first_child=last->next_sibling;
}
else {
first->prev_sibling->next_sibling=last->next_sibling;
}
if(last->next_sibling==0) {
last->parent->last_child=first->prev_sibling;
}
else {
last->next_sibling->prev_sibling=first->prev_sibling;
}
if(position.node->first_child==0) {
position.node->first_child=first;
position.node->last_child=last;
first->prev_sibling=0;
}
else {
position.node->last_child->next_sibling=first;
first->prev_sibling=position.node->last_child;
position.node->last_child=last;
}
last->next_sibling=0;
tree_node *pos=first;
while(true) {
pos->parent=position.node;
if(pos==last) break;
pos=pos->next_sibling;
}
return first;
}
template <class T, class tree_node_allocator>
template <typename iter> iter tree<T, tree_node_allocator>::reparent(iter position, iter from)
{
if(from.node->first_child==0) return position;
return reparent(position, from.node->first_child, end(from));
}
template <class T, class tree_node_allocator>
template <typename iter> iter tree<T, tree_node_allocator>::wrap(iter position, const T& x)
{
tree_assert(position.node!=0);
sibling_iterator fr=position, to=position;
++to;
iter ret = insert(position, x);
reparent(ret, fr, to);
return ret;
}
template <class T, class tree_node_allocator>
template <typename iter> iter tree<T, tree_node_allocator>::move_after(iter target, iter source)
{
tree_node *dst=target.node;
tree_node *src=source.node;
tree_assert(dst);
tree_assert(src);
if(dst==src) return source;
if(dst->next_sibling)
if(dst->next_sibling==src)
return source;
if(src->prev_sibling!=0) src->prev_sibling->next_sibling=src->next_sibling;
else src->parent->first_child=src->next_sibling;
if(src->next_sibling!=0) src->next_sibling->prev_sibling=src->prev_sibling;
else src->parent->last_child=src->prev_sibling;
if(dst->next_sibling!=0) dst->next_sibling->prev_sibling=src;
else dst->parent->last_child=src;
src->next_sibling=dst->next_sibling;
dst->next_sibling=src;
src->prev_sibling=dst;
src->parent=dst->parent;
return src;
}
template <class T, class tree_node_allocator>
template <typename iter> iter tree<T, tree_node_allocator>::move_before(iter target, iter source)
{
tree_node *dst=target.node;
tree_node *src=source.node;
tree_assert(dst);
tree_assert(src);
if(dst==src) return source;
if(dst->prev_sibling)
if(dst->prev_sibling==src)
return source;
if(src->prev_sibling!=0) src->prev_sibling->next_sibling=src->next_sibling;
else src->parent->first_child=src->next_sibling;
if(src->next_sibling!=0) src->next_sibling->prev_sibling=src->prev_sibling;
else src->parent->last_child=src->prev_sibling;
if(dst->prev_sibling!=0) dst->prev_sibling->next_sibling=src;
else dst->parent->first_child=src;
src->prev_sibling=dst->prev_sibling;
dst->prev_sibling=src;
src->next_sibling=dst;
src->parent=dst->parent;
return src;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::sibling_iterator tree<T, tree_node_allocator>::move_before(sibling_iterator target,
sibling_iterator source)
{
tree_node *dst=target.node;
tree_node *src=source.node;
tree_node *dst_prev_sibling;
if(dst==0) {
dst_prev_sibling=target.parent_->last_child;
tree_assert(dst_prev_sibling);
}
else dst_prev_sibling=dst->prev_sibling;
tree_assert(src);
if(dst==src) return source;
if(dst_prev_sibling)
if(dst_prev_sibling==src)
return source;
if(src->prev_sibling!=0) src->prev_sibling->next_sibling=src->next_sibling;
else src->parent->first_child=src->next_sibling;
if(src->next_sibling!=0) src->next_sibling->prev_sibling=src->prev_sibling;
else src->parent->last_child=src->prev_sibling;
if(dst_prev_sibling!=0) dst_prev_sibling->next_sibling=src;
else if (dst) dst->parent->first_child=src;
else target.parent_->first_child=src;
src->prev_sibling=dst_prev_sibling;
if(dst) {
dst->prev_sibling=src;
src->parent=dst->parent;
}
src->next_sibling=dst;
return src;
}
template <class T, class tree_node_allocator>
template <typename iter> iter tree<T, tree_node_allocator>::move_ontop(iter target, iter source)
{
tree_node *dst=target.node;
tree_node *src=source.node;
tree_assert(dst);
tree_assert(src);
if(dst==src) return source;
tree_node *b_prev_sibling=dst->prev_sibling;
tree_node *b_next_sibling=dst->next_sibling;
tree_node *b_parent=dst->parent;
erase(target);
if(src->prev_sibling!=0) src->prev_sibling->next_sibling=src->next_sibling;
else src->parent->first_child=src->next_sibling;
if(src->next_sibling!=0) src->next_sibling->prev_sibling=src->prev_sibling;
else src->parent->last_child=src->prev_sibling;
if(b_prev_sibling!=0) b_prev_sibling->next_sibling=src;
else b_parent->first_child=src;
if(b_next_sibling!=0) b_next_sibling->prev_sibling=src;
else b_parent->last_child=src;
src->prev_sibling=b_prev_sibling;
src->next_sibling=b_next_sibling;
src->parent=b_parent;
return src;
}
template <class T, class tree_node_allocator>
void tree<T, tree_node_allocator>::merge(sibling_iterator to1, sibling_iterator to2,
sibling_iterator from1, sibling_iterator from2,
bool duplicate_leaves)
{
sibling_iterator fnd;
while(from1!=from2) {
if((fnd=std::find(to1, to2, (*from1))) != to2) {
if(from1.begin()==from1.end()) {
if(duplicate_leaves)
append_child(parent(to1), (*from1));
}
else {
merge(fnd.begin(), fnd.end(), from1.begin(), from1.end(), duplicate_leaves);
}
}
else {
insert_subtree(to2, from1);
}
++from1;
}
}
template <class T, class tree_node_allocator>
void tree<T, tree_node_allocator>::sort(sibling_iterator from, sibling_iterator to, bool deep)
{
std::less<T> comp;
sort(from, to, comp, deep);
}
template <class T, class tree_node_allocator>
template <class StrictWeakOrdering>
void tree<T, tree_node_allocator>::sort(sibling_iterator from, sibling_iterator to,
StrictWeakOrdering comp, bool deep) {
if(from==to) return;
std::multiset<tree_node *, compare_nodes<StrictWeakOrdering> > nodes(comp);
sibling_iterator it=from, it2=to;
while(it != to) {
nodes.insert(it.node);
++it;
}
--it2;
tree_node *prev=from.node->prev_sibling;
tree_node *next=it2.node->next_sibling;
typename std::multiset<tree_node *, compare_nodes<StrictWeakOrdering> >::iterator nit=nodes.begin(), eit=nodes.end();
if(prev==0) {
if((*nit)->parent!=0)
(*nit)->parent->first_child=(*nit);
}
else prev->next_sibling=(*nit);
--eit;
while(nit!=eit) {
(*nit)->prev_sibling=prev;
if(prev)
prev->next_sibling=(*nit);
prev=(*nit);
++nit;
}
if(prev)
prev->next_sibling=(*eit);
(*eit)->next_sibling=next;
(*eit)->prev_sibling=prev;
if(next==0) {
if((*eit)->parent!=0)
(*eit)->parent->last_child=(*eit);
}
else next->prev_sibling=(*eit);
if(deep) {
sibling_iterator bcs(*nodes.begin());
sibling_iterator ecs(*eit);
++ecs;
while(bcs!=ecs) {
sort(begin(bcs), end(bcs), comp, deep);
++bcs;
}
}
}
template <class T, class tree_node_allocator>
template <class Partition>
typename tree<T, tree_node_allocator>::sibling_iterator tree<T, tree_node_allocator>::partition
(sibling_iterator from, sibling_iterator to,Partition pred) {
while (true) {
while (from!=to && pred(*from))
++from;
while (from!=to && !pred(*--to))
;
if (from==to)
return from;
this->swap(to,from);
std::swap(to,from);
}
}
template <class T, class tree_node_allocator>
template <class StrictWeakOrdering>
void tree<T, tree_node_allocator>::sort_on_subtrees(sibling_iterator from,
sibling_iterator to,
StrictWeakOrdering comp,
bool deep) {
typedef std::multiset<tree_node*,compare_nodes_pre_it<StrictWeakOrdering> >
mset;
if(from==to) return;
if(deep) {
for (sibling_iterator sib=from;sib!=to;++sib)
sort_on_subtrees(sib.begin(),sib.end(),comp,true);
}
mset nodes(comp);
sibling_iterator it=from, it2=to;
while(it != to) {
nodes.insert(it.node);
++it;
}
--it2;
tree_node *prev=from.node->prev_sibling;
tree_node *next=it2.node->next_sibling;
typename mset::iterator nit=nodes.begin(), eit=nodes.end();
if(prev==0) {
if((*nit)->parent!=0)
(*nit)->parent->first_child=(*nit);
}
else prev->next_sibling=(*nit);
--eit;
while(nit!=eit) {
(*nit)->prev_sibling=prev;
if(prev)
prev->next_sibling=(*nit);
prev=(*nit);
++nit;
}
if(prev)
prev->next_sibling=(*eit);
(*eit)->next_sibling=next;
(*eit)->prev_sibling=prev;
if(next==0) {
if((*eit)->parent!=0)
(*eit)->parent->last_child=(*eit);
}
else next->prev_sibling=(*eit);
}
template <class T, class tree_node_allocator>
template <typename iter>
bool tree<T, tree_node_allocator>::equal(const iter& one_, const iter& two, const iter& three_) const
{
std::equal_to<T> comp;
return equal(one_, two, three_, comp);
}
template <class T, class tree_node_allocator>
template <typename iter>
bool tree<T, tree_node_allocator>::equal_subtree(const iter& one_, const iter& two_) const
{
std::equal_to<T> comp;
return equal_subtree(one_, two_, comp);
}
template <class T, class tree_node_allocator>
template <typename iter, class BinaryPredicate>
bool tree<T, tree_node_allocator>::equal(const iter& one_, const iter& two, const iter& three_, BinaryPredicate fun) const
{
pre_order_iterator one(one_), three(three_);
while(one!=two && is_valid(three)) {
if(!fun(*one,*three))
return false;
if(one.number_of_children()!=three.number_of_children())
return false;
++one;
++three;
}
return true;
}
template <class T, class tree_node_allocator>
template <typename iter, class BinaryPredicate>
bool tree<T, tree_node_allocator>::equal_subtree(const iter& one_, const iter& two_, BinaryPredicate fun) const
{
if (one_.node==two_.node)
return true;
pre_order_iterator one(one_), two(two_);
if(!fun(*one,*two)) return false;
if(number_of_children(one)!=number_of_children(two)) return false;
return equal(begin(one),end(one),begin(two),fun);
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator> tree<T, tree_node_allocator>::subtree(sibling_iterator from, sibling_iterator to) const
{
tree tmp;
tmp.set_head(value_type());
tmp.replace(tmp.begin(), tmp.end(), from, to);
return tmp;
}
template <class T, class tree_node_allocator>
void tree<T, tree_node_allocator>::subtree(tree& tmp, sibling_iterator from, sibling_iterator to) const
{
tmp.set_head(value_type());
tmp.replace(tmp.begin(), tmp.end(), from, to);
}
template <class T, class tree_node_allocator>
int tree<T, tree_node_allocator>::size() const
{
int i=0;
pre_order_iterator it=begin(), eit=end();
while(it!=eit) {
++i;
++it;
}
return i;
}
template <class T, class tree_node_allocator>
int tree<T, tree_node_allocator>::subtree_size(const iterator_base& it) const
{
int i=1;
for(sibling_iterator sib = it.begin(); sib != it.end(); ++sib)
i += subtree_size(sib);
return i;
}
template <class T, class tree_node_allocator>
bool tree<T, tree_node_allocator>::empty() const
{
pre_order_iterator it=begin(), eit=end();
return (it==eit);
}
template <class T, class tree_node_allocator>
int tree<T, tree_node_allocator>::depth(const iterator_base& it) const
{
tree_node* pos=it.node;
tree_assert(pos!=0);
int ret=0;
while(pos->parent!=0) {
pos=pos->parent;
++ret;
}
return ret;
}
template <class T, class tree_node_allocator>
unsigned int tree<T, tree_node_allocator>::number_of_children(const iterator_base& it) const
{
tree_node *pos=it.node->first_child;
if(pos==0) return 0;
unsigned int ret=1;
while((pos=pos->next_sibling))
++ret;
return ret;
}
template <class T, class tree_node_allocator>
unsigned int tree<T, tree_node_allocator>::number_of_siblings(const iterator_base& it) const
{
tree_node *pos=it.node;
unsigned int ret=0;
while(pos->next_sibling &&
pos->next_sibling!=head &&
pos->next_sibling!=feet) {
++ret;
pos=pos->next_sibling;
}
pos=it.node;
while(pos->prev_sibling &&
pos->prev_sibling!=head &&
pos->prev_sibling!=feet) {
++ret;
pos=pos->prev_sibling;
}
return ret;
}
template <class T, class tree_node_allocator>
void tree<T, tree_node_allocator>::swap(sibling_iterator it)
{
tree_node *nxt=it.node->next_sibling;
if(nxt) {
if(it.node->prev_sibling)
it.node->prev_sibling->next_sibling=nxt;
else
it.node->parent->first_child=nxt;
nxt->prev_sibling=it.node->prev_sibling;
tree_node *nxtnxt=nxt->next_sibling;
if(nxtnxt)
nxtnxt->prev_sibling=it.node;
else
it.node->parent->last_child=it.node;
nxt->next_sibling=it.node;
it.node->prev_sibling=nxt;
it.node->next_sibling=nxtnxt;
}
}
template <class T, class tree_node_allocator>
void tree<T, tree_node_allocator>::swap(iterator one, iterator two)
{
if(one.node->next_sibling==two.node) swap(one);
else if(two.node->next_sibling==one.node) swap(two);
else if (one.node!=two.node) {
tree_node *nxt1=one.node->next_sibling;
tree_node *nxt2=two.node->next_sibling;
tree_node *pre1=one.node->prev_sibling;
tree_node *pre2=two.node->prev_sibling;
tree_node *par1=one.node->parent;
tree_node *par2=two.node->parent;
one.node->parent=par2;
one.node->next_sibling=nxt2;
if(nxt2) nxt2->prev_sibling=one.node;
else par2->last_child=one.node;
one.node->prev_sibling=pre2;
if(pre2) pre2->next_sibling=one.node;
else par2->first_child=one.node;
two.node->parent=par1;
two.node->next_sibling=nxt1;
if(nxt1) nxt1->prev_sibling=two.node;
else par1->last_child=two.node;
two.node->prev_sibling=pre1;
if(pre1) pre1->next_sibling=two.node;
else par1->first_child=two.node;
}
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::pre_order_iterator
tree<T, tree_node_allocator>::find_subtree(const iterator_base& it, const iterator_base& begin,
const iterator_base& end) const {
for(pre_order_iterator i = pre_order_iterator(begin); i != pre_order_iterator(end); ++i)
{
if(equal_subtree(pre_order_iterator(it), i))
return i;
}
pre_order_iterator res;
return res;
}
template <class T, class tree_node_allocator>
bool tree<T, tree_node_allocator>::is_in_subtree(const iterator_base& it, const iterator_base& begin,
const iterator_base& end) const
{
pre_order_iterator tmp=begin;
while(tmp!=end) {
if(tmp==it) return true;
++tmp;
}
return false;
}
template <class T, class tree_node_allocator>
bool tree<T, tree_node_allocator>::is_valid(const iterator_base& it) const
{
if(it.node==0 || it.node==feet || it.node==head) return false;
else return true;
}
template <class T, class tree_node_allocator>
void tree<T, tree_node_allocator>::validate(const iterator_base& it) const {
tree_node *n=it.node;
if (n->parent) {
if (!n->prev_sibling)
tree_assert(n->parent->first_child==n);
if (!n->next_sibling)
tree_assert(n->parent->last_child==n);
}
if (n->prev_sibling)
tree_assert(n->prev_sibling->next_sibling==n);
if (n->next_sibling)
tree_assert(n->next_sibling->prev_sibling==n);
for (sibling_iterator sib=begin(it);sib!=end(it);++sib)
validate(sib);
}
template <class T, class tree_node_allocator>
unsigned int tree<T, tree_node_allocator>::sibling_index(sibling_iterator it) const
{
unsigned int ind=0;
if(it.node->parent==0) {
while(it.node->prev_sibling!=head) {
it.node=it.node->prev_sibling;
++ind;
}
}
else {
while(it.node->prev_sibling!=0) {
it.node=it.node->prev_sibling;
++ind;
}
}
return ind;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::sibling_iterator tree<T, tree_node_allocator>::child(const iterator_base& it, unsigned int num) const
{
tree_node *tmp=it.node->first_child;
while(num--) {
tree_assert(tmp!=0);
tmp=tmp->next_sibling;
}
return tmp;
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::iterator_base::iterator_base()
: node(0), skip_current_children_(false)
{
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::iterator_base::iterator_base(tree_node *tn)
: node(tn), skip_current_children_(false)
{
}
template <class T, class tree_node_allocator>
T& tree<T, tree_node_allocator>::iterator_base::operator*() const
{
return node->data;
}
template <class T, class tree_node_allocator>
T* tree<T, tree_node_allocator>::iterator_base::operator->() const
{
return &(node->data);
}
template <class T, class tree_node_allocator>
bool tree<T, tree_node_allocator>::post_order_iterator::operator!=(const post_order_iterator& other) const
{
if(other.node!=this->node) return true;
else return false;
}
template <class T, class tree_node_allocator>
bool tree<T, tree_node_allocator>::post_order_iterator::operator==(const post_order_iterator& other) const
{
if(other.node==this->node) return true;
else return false;
}
template <class T, class tree_node_allocator>
bool tree<T, tree_node_allocator>::pre_order_iterator::operator!=(const pre_order_iterator& other) const
{
if(other.node!=this->node) return true;
else return false;
}
template <class T, class tree_node_allocator>
bool tree<T, tree_node_allocator>::pre_order_iterator::operator==(const pre_order_iterator& other) const
{
if(other.node==this->node) return true;
else return false;
}
template <class T, class tree_node_allocator>
bool tree<T, tree_node_allocator>::sibling_iterator::operator!=(const sibling_iterator& other) const
{
if(other.node!=this->node) return true;
else return false;
}
template <class T, class tree_node_allocator>
bool tree<T, tree_node_allocator>::sibling_iterator::operator==(const sibling_iterator& other) const
{
if(other.node==this->node) return true;
else return false;
}
template <class T, class tree_node_allocator>
bool tree<T, tree_node_allocator>::leaf_iterator::operator!=(const leaf_iterator& other) const
{
if(other.node!=this->node) return true;
else return false;
}
template <class T, class tree_node_allocator>
bool tree<T, tree_node_allocator>::leaf_iterator::operator==(const leaf_iterator& other) const
{
if(other.node==this->node) return true;
else return false;
}
template <class T, class tree_node_allocator>
bool tree<T, tree_node_allocator>::upwards_iterator::operator!=(const upwards_iterator& other) const
{
if(other.node!=this->node) return true;
else return false;
}
template <class T, class tree_node_allocator>
bool tree<T, tree_node_allocator>::upwards_iterator::operator==(const upwards_iterator& other) const
{
if(other.node==this->node) return true;
else return false;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::sibling_iterator tree<T, tree_node_allocator>::iterator_base::begin() const
{
sibling_iterator ret(node->first_child);
ret.parent_=this->node;
return ret;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::sibling_iterator tree<T, tree_node_allocator>::iterator_base::end() const
{
sibling_iterator ret;
ret.parent_=node;
return ret;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::sibling_iterator tree<T, tree_node_allocator>::iterator_base::last_child() const
{
sibling_iterator ret(node->last_child);
ret.parent_=node;
return ret;
}
template <class T, class tree_node_allocator>
void tree<T, tree_node_allocator>::iterator_base::skip_children()
{
skip_current_children_=true;
}
template <class T, class tree_node_allocator>
unsigned int tree<T, tree_node_allocator>::iterator_base::number_of_children() const
{
tree_node *pos=node->first_child;
if(pos==0) return 0;
unsigned int ret=1;
while(pos!=node->last_child) {
++ret;
pos=pos->next_sibling;
}
return ret;
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::pre_order_iterator::pre_order_iterator()
: iterator_base(0)
{
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::pre_order_iterator::pre_order_iterator(tree_node *tn)
: iterator_base(tn)
{
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::pre_order_iterator::pre_order_iterator(const iterator_base &other)
: iterator_base(other.node)
{
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::pre_order_iterator::pre_order_iterator(const sibling_iterator& other)
: iterator_base(other.node)
{
if(this->node==0) {
if(other.range_last()!=0)
this->node=other.range_last();
else
this->node=other.parent_;
this->skip_children();
++(*this);
}
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::pre_order_iterator& tree<T, tree_node_allocator>::pre_order_iterator::operator++()
{
tree_assert(this->node!=0);
if(!this->skip_current_children_ && this->node->first_child != 0) {
this->node=this->node->first_child;
}
else {
this->skip_current_children_=false;
while(this->node->next_sibling==0) {
this->node=this->node->parent;
if(this->node==0)
return *this;
}
this->node=this->node->next_sibling;
}
return *this;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::pre_order_iterator& tree<T, tree_node_allocator>::pre_order_iterator::operator--()
{
tree_assert(this->node!=0);
if(this->node->prev_sibling) {
this->node=this->node->prev_sibling;
while(this->node->last_child)
this->node=this->node->last_child;
}
else {
this->node=this->node->parent;
if(this->node==0)
return *this;
}
return *this;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::pre_order_iterator tree<T, tree_node_allocator>::pre_order_iterator::operator++(int n)
{
pre_order_iterator copy = *this;
++(*this);
return copy;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::pre_order_iterator tree<T, tree_node_allocator>::pre_order_iterator::operator--(int n)
{
pre_order_iterator copy = *this;
--(*this);
return copy;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::pre_order_iterator& tree<T, tree_node_allocator>::pre_order_iterator::operator+=(unsigned int num)
{
while(num>0) {
++(*this);
--num;
}
return (*this);
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::pre_order_iterator& tree<T, tree_node_allocator>::pre_order_iterator::operator-=(unsigned int num)
{
while(num>0) {
--(*this);
--num;
}
return (*this);
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::post_order_iterator::post_order_iterator()
: iterator_base(0)
{
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::post_order_iterator::post_order_iterator(tree_node *tn)
: iterator_base(tn)
{
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::post_order_iterator::post_order_iterator(const iterator_base &other)
: iterator_base(other.node)
{
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::post_order_iterator::post_order_iterator(const sibling_iterator& other)
: iterator_base(other.node)
{
if(this->node==0) {
if(other.range_last()!=0)
this->node=other.range_last();
else
this->node=other.parent_;
this->skip_children();
++(*this);
}
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::post_order_iterator& tree<T, tree_node_allocator>::post_order_iterator::operator++()
{
tree_assert(this->node!=0);
if(this->node->next_sibling==0) {
this->node=this->node->parent;
this->skip_current_children_=false;
}
else {
this->node=this->node->next_sibling;
if(this->skip_current_children_) {
this->skip_current_children_=false;
}
else {
while(this->node->first_child)
this->node=this->node->first_child;
}
}
return *this;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::post_order_iterator& tree<T, tree_node_allocator>::post_order_iterator::operator--()
{
tree_assert(this->node!=0);
if(this->skip_current_children_ || this->node->last_child==0) {
this->skip_current_children_=false;
while(this->node->prev_sibling==0)
this->node=this->node->parent;
this->node=this->node->prev_sibling;
}
else {
this->node=this->node->last_child;
}
return *this;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::post_order_iterator tree<T, tree_node_allocator>::post_order_iterator::operator++(int)
{
post_order_iterator copy = *this;
++(*this);
return copy;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::post_order_iterator tree<T, tree_node_allocator>::post_order_iterator::operator--(int)
{
post_order_iterator copy = *this;
--(*this);
return copy;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::post_order_iterator& tree<T, tree_node_allocator>::post_order_iterator::operator+=(unsigned int num)
{
while(num>0) {
++(*this);
--num;
}
return (*this);
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::post_order_iterator& tree<T, tree_node_allocator>::post_order_iterator::operator-=(unsigned int num)
{
while(num>0) {
--(*this);
--num;
}
return (*this);
}
template <class T, class tree_node_allocator>
void tree<T, tree_node_allocator>::post_order_iterator::descend_all()
{
tree_assert(this->node!=0);
while(this->node->first_child)
this->node=this->node->first_child;
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::breadth_first_queued_iterator::breadth_first_queued_iterator()
: iterator_base()
{
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::breadth_first_queued_iterator::breadth_first_queued_iterator(tree_node *tn)
: iterator_base(tn)
{
traversal_queue.push(tn);
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::breadth_first_queued_iterator::breadth_first_queued_iterator(const iterator_base& other)
: iterator_base(other.node)
{
traversal_queue.push(other.node);
}
template <class T, class tree_node_allocator>
bool tree<T, tree_node_allocator>::breadth_first_queued_iterator::operator!=(const breadth_first_queued_iterator& other) const
{
if(other.node!=this->node) return true;
else return false;
}
template <class T, class tree_node_allocator>
bool tree<T, tree_node_allocator>::breadth_first_queued_iterator::operator==(const breadth_first_queued_iterator& other) const
{
if(other.node==this->node) return true;
else return false;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::breadth_first_queued_iterator& tree<T, tree_node_allocator>::breadth_first_queued_iterator::operator++()
{
tree_assert(this->node!=0);
sibling_iterator sib=this->begin();
while(sib!=this->end()) {
traversal_queue.push(sib.node);
++sib;
}
traversal_queue.pop();
if(!traversal_queue.empty())
this->node=traversal_queue.front();
else
this->node=0;
return (*this);
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::breadth_first_queued_iterator tree<T, tree_node_allocator>::breadth_first_queued_iterator::operator++(int n)
{
breadth_first_queued_iterator copy = *this;
++(*this);
return copy;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::breadth_first_queued_iterator& tree<T, tree_node_allocator>::breadth_first_queued_iterator::operator+=(unsigned int num)
{
while(num>0) {
++(*this);
--num;
}
return (*this);
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::fixed_depth_iterator::fixed_depth_iterator()
: iterator_base()
{
set_first_parent_();
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::fixed_depth_iterator::fixed_depth_iterator(tree_node *tn)
: iterator_base(tn)
{
set_first_parent_();
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::fixed_depth_iterator::fixed_depth_iterator(const iterator_base& other)
: iterator_base(other.node)
{
set_first_parent_();
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::fixed_depth_iterator::fixed_depth_iterator(const sibling_iterator& other)
: iterator_base(other.node), first_parent_(other.parent_)
{
find_leftmost_parent_();
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::fixed_depth_iterator::fixed_depth_iterator(const fixed_depth_iterator& other)
: iterator_base(other.node), first_parent_(other.first_parent_)
{
}
template <class T, class tree_node_allocator>
bool tree<T, tree_node_allocator>::fixed_depth_iterator::operator==(const fixed_depth_iterator& other) const
{
if(other.node==this->node && other.first_parent_==first_parent_) return true;
else return false;
}
template <class T, class tree_node_allocator>
bool tree<T, tree_node_allocator>::fixed_depth_iterator::operator!=(const fixed_depth_iterator& other) const
{
if(other.node!=this->node || other.first_parent_!=first_parent_) return true;
else return false;
}
template <class T, class tree_node_allocator>
void tree<T, tree_node_allocator>::fixed_depth_iterator::set_first_parent_()
{
return;
first_parent_=0;
if(this->node==0) return;
if(this->node->parent!=0)
first_parent_=this->node->parent;
if(first_parent_)
find_leftmost_parent_();
}
template <class T, class tree_node_allocator>
void tree<T, tree_node_allocator>::fixed_depth_iterator::find_leftmost_parent_()
{
return;
tree_node *tmppar=first_parent_;
while(tmppar->prev_sibling) {
tmppar=tmppar->prev_sibling;
if(tmppar->first_child)
first_parent_=tmppar;
}
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::fixed_depth_iterator& tree<T, tree_node_allocator>::fixed_depth_iterator::operator++()
{
tree_assert(this->node!=0);
if(this->node->next_sibling) {
this->node=this->node->next_sibling;
}
else {
int relative_depth=0;
upper:
do {
this->node=this->node->parent;
if(this->node==0) return *this;
--relative_depth;
} while(this->node->next_sibling==0);
lower:
this->node=this->node->next_sibling;
while(this->node->first_child==0) {
if(this->node->next_sibling==0)
goto upper;
this->node=this->node->next_sibling;
if(this->node==0) return *this;
}
while(relative_depth<0 && this->node->first_child!=0) {
this->node=this->node->first_child;
++relative_depth;
}
if(relative_depth<0) {
if(this->node->next_sibling==0) goto upper;
else goto lower;
}
}
return *this;
return *this;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::fixed_depth_iterator& tree<T, tree_node_allocator>::fixed_depth_iterator::operator--()
{
tree_assert(this->node!=0);
if(this->node->prev_sibling!=0) {
this->node=this->node->prev_sibling;
tree_assert(this->node!=0);
if(this->node->parent==0 && this->node->prev_sibling==0)
this->node=0;
}
else {
tree_node *par=this->node->parent;
do {
par=par->prev_sibling;
if(par==0) {
this->node=0;
return *this;
}
} while(par->last_child==0);
this->node=par->last_child;
}
return *this;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::fixed_depth_iterator tree<T, tree_node_allocator>::fixed_depth_iterator::operator++(int)
{
fixed_depth_iterator copy = *this;
++(*this);
return copy;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::fixed_depth_iterator tree<T, tree_node_allocator>::fixed_depth_iterator::operator--(int)
{
fixed_depth_iterator copy = *this;
--(*this);
return copy;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::fixed_depth_iterator& tree<T, tree_node_allocator>::fixed_depth_iterator::operator-=(unsigned int num)
{
while(num>0) {
--(*this);
--(num);
}
return (*this);
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::fixed_depth_iterator& tree<T, tree_node_allocator>::fixed_depth_iterator::operator+=(unsigned int num)
{
while(num>0) {
++(*this);
--(num);
}
return *this;
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::sibling_iterator::sibling_iterator()
: iterator_base(),parent_(0) { }
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::sibling_iterator::sibling_iterator(tree_node *tn)
: iterator_base(tn),parent_(tn==0 ? 0 : tn->parent) { }
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::sibling_iterator::sibling_iterator(const iterator_base& other)
: iterator_base(other.node),parent_(other.node==0 ? 0 : other.node->parent) { }
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::sibling_iterator::sibling_iterator(const sibling_iterator& other)
: iterator_base(other), parent_(other.parent_)
{
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::sibling_iterator& tree<T, tree_node_allocator>::sibling_iterator::operator++()
{
if(this->node) {
if (!this->node->next_sibling) {
this->parent_=this->node->parent;
this->node=NULL;
} else {
this->node=this->node->next_sibling;
}
}
return *this;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::sibling_iterator& tree<T, tree_node_allocator>::sibling_iterator::operator--()
{
if(this->node) this->node=this->node->prev_sibling;
else {
tree_assert(parent_);
this->node=parent_->last_child;
}
return *this;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::sibling_iterator tree<T, tree_node_allocator>::sibling_iterator::operator++(int)
{
sibling_iterator copy = *this;
++(*this);
return copy;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::sibling_iterator tree<T, tree_node_allocator>::sibling_iterator::operator--(int)
{
sibling_iterator copy = *this;
--(*this);
return copy;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::sibling_iterator& tree<T, tree_node_allocator>::sibling_iterator::operator+=(unsigned int num)
{
while(num>0) {
++(*this);
--num;
}
return (*this);
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::sibling_iterator& tree<T, tree_node_allocator>::sibling_iterator::operator-=(unsigned int num)
{
while(num>0) {
--(*this);
--num;
}
return (*this);
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::tree_node *tree<T, tree_node_allocator>::sibling_iterator::range_first() const
{
tree_node *tmp=parent_->first_child;
return tmp;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::tree_node *tree<T, tree_node_allocator>::sibling_iterator::range_last() const
{
return parent_->last_child;
}
template<typename T>
bool operator==(const tree<T>& t1,const tree<T>& t2) {
return (t1.empty() ? t2.empty() : (t2.empty() ? false :
t1.equal_subtree(t1.begin(),t2.begin())));
}
template<typename T>
bool operator!=(const tree<T>& t1,const tree<T>& t2) {
return (!(t1==t2));
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::leaf_iterator::leaf_iterator()
: iterator_base(0)
{
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::leaf_iterator::leaf_iterator(tree_node *tn)
: iterator_base(tn)
{
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::leaf_iterator::leaf_iterator(const iterator_base &other)
: iterator_base(other.node)
{
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::leaf_iterator::leaf_iterator(const sibling_iterator& other)
: iterator_base(other.node)
{
if(this->node==0) {
if(other.range_last()!=0)
this->node=other.range_last();
else
this->node=other.parent_;
++(*this);
}
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::leaf_iterator& tree<T, tree_node_allocator>::leaf_iterator::operator++()
{
tree_assert(this->node!=0);
while(this->node->next_sibling==0) {
if (this->node->parent==0) return *this;
this->node=this->node->parent;
}
this->node=this->node->next_sibling;
while(this->node->first_child)
this->node=this->node->first_child;
return *this;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::leaf_iterator& tree<T, tree_node_allocator>::leaf_iterator::operator--()
{
tree_assert(this->node!=0);
while (this->node->prev_sibling==0) {
if (this->node->parent==0) return *this;
this->node=this->node->parent;
}
this->node=this->node->prev_sibling;
while(this->node->last_child)
this->node=this->node->last_child;
return *this;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::leaf_iterator tree<T, tree_node_allocator>::leaf_iterator::operator++(int)
{
leaf_iterator copy = *this;
++(*this);
return copy;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::leaf_iterator tree<T, tree_node_allocator>::leaf_iterator::operator--(int)
{
leaf_iterator copy = *this;
--(*this);
return copy;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::leaf_iterator& tree<T, tree_node_allocator>::leaf_iterator::operator+=(unsigned int num)
{
while(num>0) {
++(*this);
--num;
}
return (*this);
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::leaf_iterator& tree<T, tree_node_allocator>::leaf_iterator::operator-=(unsigned int num)
{
while(num>0) {
--(*this);
--num;
}
return (*this);
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::upwards_iterator::upwards_iterator()
: iterator_base(0)
{
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::upwards_iterator::upwards_iterator(tree_node *tn)
: iterator_base(tn)
{
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::upwards_iterator::upwards_iterator(const iterator_base &other)
: iterator_base(other.node)
{
}
template <class T, class tree_node_allocator>
tree<T, tree_node_allocator>::upwards_iterator::upwards_iterator(const sibling_iterator& other)
: iterator_base(other.node)
{
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::upwards_iterator& tree<T, tree_node_allocator>::upwards_iterator::operator++()
{
tree_assert(this->node!=0);
this->node=this->node->parent;
return *this;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::upwards_iterator tree<T, tree_node_allocator>::upwards_iterator::operator++(int)
{
upwards_iterator copy = *this;
++(*this);
return copy;
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::upwards_iterator& tree<T, tree_node_allocator>::upwards_iterator::operator+=(unsigned int num)
{
while(num>0) {
++(*this);
--num;
}
return (*this);
}
template <class T, class tree_node_allocator>
typename tree<T, tree_node_allocator>::upwards_iterator& tree<T, tree_node_allocator>::upwards_iterator::operator-=(unsigned int num)
{
while(num>0) {
--(*this);
--num;
}
return (*this);
}
}
namespace std {
template<typename T>
void swap(opencog::tree<T>& x, opencog::tree<T>& y) {
std::swap(x.head,y.head);
std::swap(x.feet,y.feet);
}
}
namespace opencog {
template<typename treeT1,typename src_iterator,
typename treeT2,typename dst_iterator>
void subtree_copy(const treeT1& src,src_iterator src_it,
treeT2& dst,dst_iterator dst_it) {
dst_it=dst.replace(dst_it,(*src_it));
dst.erase_children(dst_it);
for (typename treeT1::sibling_iterator sib=src.begin(src_it);
sib!=src.end(src_it);++sib)
subtree_copy(src,sib,dst,dst.append_child(dst_it));
}
template<typename treeT1,typename treeT2>
void tree_copy(const treeT1& src,treeT2& dst) {
typedef typename treeT2::value_type T2;
dst=treeT2(T2());
typename treeT1::iterator src_it=src.begin();
typename treeT2::iterator dst_it=dst.begin();
while (src_it!=src.end()) {
dst_it=dst.insert_after(dst_it,T2());
subtree_copy(src,src_it,dst,dst_it);
src_it.skip_children();
++src_it;
}
dst.erase(dst.begin());
}
template<typename treeT1,typename src_iterator,
typename treeT2,typename dst_iterator>
void subtree_convert(const treeT1& src, src_iterator src_it,
treeT2& dst,dst_iterator dst_it)
{
typedef typename treeT2::value_type T2;
dst_it=dst.replace(dst_it,boost::lexical_cast<T2>(*src_it));
dst.erase_children(dst_it);
for (typename treeT1::sibling_iterator sib=src.begin(src_it);
sib!=src.end(src_it);++sib)
subtree_convert(src,sib,dst,dst.append_child(dst_it));
}
template<typename treeT1,typename treeT2>
void tree_convert(const treeT1& src, treeT2& dst)
{
typedef typename treeT2::value_type T2;
dst = treeT2(T2());
typename treeT1::iterator src_it = src.begin();
typename treeT2::iterator dst_it = dst.begin();
while (src_it != src.end())
{
dst_it = dst.insert_after(dst_it, T2());
subtree_convert(src,src_it, dst, dst_it);
src_it.skip_children();
++src_it;
}
dst.erase(dst.begin());
}
template<typename T, typename compare=std::less<T>>
struct lexicographic_subtree_order {
lexicographic_subtree_order() {}
template<typename iter>
bool operator()(const tree<T>& tr1, const iter& it2) const {
return (cmp(iter(tr1.begin()),it2)>0);
}
template<typename iter>
bool operator()(const iter& it1, const tree<T>& tr2) const {
return (cmp(it1,iter(tr2.begin()))>0);
}
bool operator()(const tree<T>& tr1,
const tree<T>& tr2) const {
return (cmp(tr1.begin(),tr2.begin())>0);
}
template<typename iter>
bool operator()(const iter& it1, const iter& it2) const {
return (cmp(it1,it2)>0);
}
template<typename iter>
int cmp(const iter& it1, const iter& it2) const {
typedef typename iter::sibling_iterator sib_it;
if (*it1<*it2)
return 1;
else if (*it2<*it1)
return -1;
sib_it sib1=it1.begin(), sib2=it2.begin();
while (true) {
if (sib1==it1.end()) {
if (sib2==it2.end())
return 0;
return 1;
} else if (sib2==it2.end()) {
return -1;
}
int res=cmp(sib1++, sib2++);
if (res)
return res;
}
}
};
template<typename T,typename compare=std::less<T> >
struct size_tree_order : public lexicographic_subtree_order<T, compare> {
bool operator()(const tree<T>& tr1,
const tree<T>& tr2) const {
int s1 = tr1.size();
int s2 = tr2.size();
if(s1 == s2) {
return (lexicographic_subtree_order<T, compare>::cmp(tr1.begin(),tr2.begin())>0);
}
else return (s1 < s2);
}
};
template<typename T>
inline boost::counting_iterator<typename tree<T>::sibling_iterator>
boost_range_begin(typename tree<T>::iterator_base it) {
return make_counting_iterator(it.begin());
}
template<typename T>
inline boost::counting_iterator<typename tree<T>::sibling_iterator>
boost_range_end(typename tree<T>::iterator_base it) {
return make_counting_iterator(it.end());
}
template<typename T>
inline boost::counting_iterator<typename tree<T>::iterator>
boost_range_begin(const tree<T>& tr) {
return make_counting_iterator(tr.begin());
}
template<typename T>
inline boost::counting_iterator<typename tree<T>::iterator>
boost_range_end(const tree<T>& tr) {
return make_counting_iterator(tr.end());
}
template<typename T>
unsigned int pre_order_index(const tree<T>& tr,
typename tree<T>::iterator it) {
unsigned int i = 0;
while(it != tr.begin()) {
--it;
++i;
}
return i;
}
template<typename iter>
std::string subtree_to_string(iter it)
{
std::stringstream ss;
int nChildren = it.number_of_children();
if (1 == nChildren) {
ss << (*it) << "(";
ss << subtree_to_string(it.begin());
ss << ")";
} else if (0 == nChildren) {
ss << (*it);
} else {
ss << (*it) << "(";
ss << subtree_to_string(it.begin());
for (typename iter::sibling_iterator sib = ++it.begin();
sib != it.end(); ++sib)
{
ss << " ";
ss << subtree_to_string(sib);
}
ss << ")";
}
return ss.str();
}
}
namespace std {
std::istream& operator>>(std::istream&, opencog::tree<std::string>&);
}
namespace opencog {
template<typename T>
std::ostream& operator<<(std::ostream& out, const opencog::tree<T>& tr)
{
typename opencog::tree<T>::sibling_iterator it = tr.begin();
if (it != tr.end()) out << subtree_to_string(it);
return out;
}
template<typename T>
std::istream& operator>>(std::istream& in, opencog::tree<T>& tr)
{
opencog::tree<std::string> tmp;
in >> tmp;
try {
opencog::tree_convert(tmp, tr);
} catch (boost::bad_lexical_cast&) {
std::stringstream stream (std::stringstream::out);
stream << "Bad node data in tree '" << tr << "'" << std::endl;
throw opencog::InconsistenceException(TRACE_INFO,
"tree - %s.",
stream.str().c_str());
}
return in;
}
}
#endif