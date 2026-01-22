#ifndef TEMPLATE_OCTREE_H
#define TEMPLATE_OCTREE_H
#include <bitset>
#include <iostream>
#include "AtomOcTreeNode.h"
#include <octomap/OccupancyOcTreeBase.h>
namespace octomap
{
template <typename T>
class AtomOcTree : public OccupancyOcTreeBase < AtomOcTreeNode<T> >
{
public:
AtomOcTree(double resolution = 0.1)
: OccupancyOcTreeBase< AtomOcTreeNode<T> >(resolution)
{}
AtomOcTree<T> *create() const
{
return new AtomOcTree<T>(this->resolution);
}
std::string getTreeType() const
{
return "AtomOcTree";
}
AtomOcTreeNode<T>* setNodeData(const OcTreeKey& key, const T& r){
AtomOcTreeNode<T>* n = this->search(key);
if (n != 0) {
n->setData(r);
}
return n;
}
AtomOcTreeNode<T>* setNodeData(const point3d& xyz, const T& r)
{
OcTreeKey key;
if (!this->coordToKeyChecked(xyz, key)) return nullptr;
return setNodeData(key, r);
}
inline std::string getMapName() const {return mMapName;}
std::string mMapName;
protected:
struct StaticMemberInitializer
{
StaticMemberInitializer()
{
AtomOcTree<T>* tree = new AtomOcTree<T>(0.1);
AbstractOcTree::registerTreeType(tree);
}
};
static StaticMemberInitializer atomOcTreeMemberInit;
};
template <typename T>
typename AtomOcTree<T>::StaticMemberInitializer AtomOcTree<T>::atomOcTreeMemberInit;
}
#endif