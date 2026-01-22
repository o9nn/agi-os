#ifndef TEMPLATE_OCTREE_NODE_H
#define TEMPLATE_OCTREE_NODE_H
#include <iostream>
#include <memory>
#include <octomap/OcTreeNode.h>
namespace octomap
{
template <typename T>
class AtomOcTreeNode : public OcTreeNode
{
public:
AtomOcTreeNode() : OcTreeNode()
{}
AtomOcTreeNode(const AtomOcTreeNode<T>& rhs) : OcTreeNode(rhs), dat(rhs.dat)
{}
~AtomOcTreeNode()
{
delete[] children;
children = nullptr;
}
bool operator==(const AtomOcTreeNode<T>& rhs) const
{
return (rhs.value == value && rhs.dat == dat);
}
inline AtomOcTreeNode<T>* getChild(unsigned int i)
{
#ifdef NEED_OBSOLETE_OCTREE_API
return static_cast<AtomOcTreeNode<T>*> (OcTreeNode<T>::getChild(i));
#else
return static_cast<AtomOcTreeNode<T>*> (children[i]);
#endif
}
inline const AtomOcTreeNode<T>* getChild(unsigned int i) const
{
#ifdef NEED_OBSOLETE_OCTREE_API
return static_cast<const AtomOcTreeNode<T>*> (OcTreeNode<T>::getChild(i));
#else
return static_cast<const AtomOcTreeNode<T>*> (children[i]);
#endif
}
bool createChild(unsigned int i)
{
if (children == nullptr) allocChildren();
children[i] = new AtomOcTreeNode<T>();
return true;
}
bool pruneNode(){
#ifdef NEED_OBSOLETE_OCTREE_API
if (!this->collapsible()) return false;
#endif
setLogOdds(getChild(0)->getLogOdds());
dat = T();
for (unsigned int i = 0; i < 8; i++) {
delete children[i];
}
delete[] children;
children = nullptr;
return true;
}
void expandNode(){
for (unsigned int k = 0; k < 8; k++) {
this->createChild(k);
#ifdef NEED_OBSOLETE_OCTREE_API
this->children[k]->setValue(value);
#endif
this->getChild(k)->setData(dat);
}
}
inline T getData() const
{
return dat;
}
inline void setData(T c)
{
this->dat = c;
}
T& getData()
{
return dat;
}
std::istream& readValue (std::istream &s) {
char children_char;
s.read((char*) &value, sizeof(value));
s.read((char*) &dat, sizeof(T));
s.read((char*)&children_char, sizeof(char));
std::bitset<8> children ((unsigned long long) children_char);
for (unsigned int i = 0; i < 8; i++) {
if (children[i] == 1) {
createChild(i);
getChild(i)->readValue(s);
}
}
return s;
}
std::ostream& writeValue(std::ostream &s) const{
std::bitset<8> children;
for (unsigned int i = 0; i < 8; i++) {
if (this->childExists(i)) children[i] = 1;
else children[i] = 0;
}
char children_char = (char) children.to_ulong();
s.write((const char*) &value, sizeof(value));
s.write((const char*) &dat, sizeof(T));
s.write((char*)&children_char, sizeof(char));
for (unsigned int i = 0; i < 8; ++i)
if (children[i] == 1) this->getChild(i)->writeValue(s);
return s;
}
protected:
T dat;
};
}
#endif