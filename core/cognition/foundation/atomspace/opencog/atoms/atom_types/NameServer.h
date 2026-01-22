#ifndef _OPENCOG_CLASS_NAMESERVER_H
#define _OPENCOG_CLASS_NAMESERVER_H
#include <mutex>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>
#include <opencog/util/sigslot.h>
#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/atom_types/atom_types.h>
class ClassServerUTest;
namespace opencog
{
typedef SigSlot<Type> TypeSignal;
class NameServer
{
friend class ::ClassServerUTest;
private:
NameServer();
std::set< std::string > _loaded_modules;
mutable std::mutex type_mutex;
mutable std::mutex _module_mutex;
mutable int _tmod;
Type nTypes;
Type nValues;
Type _maxDepth;
std::vector< std::vector<bool> > inheritanceMap;
std::vector< std::vector<bool> > recursiveMap;
std::unordered_map<std::string, Type> name2CodeMap;
std::vector<const std::string*> _code2NameMap;
std::vector<const std::string*> _code2ShortMap;
std::vector<int> _mod;
std::vector<size_t> _hash;
TypeSignal _addTypeSignal;
void setParentRecursively(Type parent, Type type, Type& maxd);
public:
friend NameServer& nameserver();
bool beginTypeDecls(const char *);
void endTypeDecls(void);
Type declType(const Type parent,
const std::string& name,
const std::string& shrt = "");
TypeSignal& typeAddedSignal();
template<typename OutputIterator>
unsigned long getChildren(Type type, OutputIterator result) const
{
unsigned long n_children = 0;
for (Type i = type+1; i < nTypes; ++i) {
if (inheritanceMap[type][i]) {
*(result++) = i;
n_children++;
}
}
return n_children;
}
template<typename OutputIterator>
unsigned long getParents(Type type, OutputIterator result) const
{
unsigned long n_parents = 0;
for (Type i = 0; i < type; ++i) {
if (inheritanceMap[i][type]) {
*(result++) = i;
n_parents++;
}
}
return n_parents;
}
template <typename OutputIterator>
unsigned long getChildrenRecursive(Type type, OutputIterator result) const
{
unsigned long n_children = 0;
for (Type i = type+1; i < nTypes; ++i) {
if (recursiveMap[type][i]) {
*(result++) = i;
n_children++;
}
}
return n_children;
}
TypeSet getChildrenRecursive(Type type) const
{
TypeSet ts;
for (Type i = type+1; i < nTypes; ++i) {
if (recursiveMap[type][i]) {
ts.insert(i);
}
}
return ts;
}
template <typename OutputIterator>
unsigned long getParentsRecursive(Type type, OutputIterator result) const
{
unsigned long n_parents = 0;
for (Type i = 0; i < type; ++i) {
if (recursiveMap[i][type]) {
*(result++) = i;
n_parents++;
}
}
return n_parents;
}
TypeSet getParentsRecursive(Type type) const
{
TypeSet ts;
for (Type i = 0; i < type; ++i) {
if (recursiveMap[i][type]) {
ts.insert(i);
}
}
return ts;
}
template <typename Function>
void foreachRecursive(Function func, Type type) const
{
for (Type i = 0; i < nTypes; ++i) {
if (recursiveMap[type][i]) (func)(i);
}
}
Type getNumberOfClasses() const { return nTypes; }
bool isA(Type sub, Type super) const
{
if ((sub >= nTypes) || (super >= nTypes)) return false;
return recursiveMap[super][sub];
}
bool isAncestor(Type super, Type sub) const;
bool isValue(Type t) const { return isA(t, VALUE); }
bool isAtom(Type t) const { return isA(t, ATOM); }
bool isNode(Type t) const { return isA(t, NODE); }
bool isLink(Type t) const { return isA(t, LINK); }
bool isDefined(const std::string& typeName) const;
bool isDefined(Type) const;
Type getType(const std::string& typeName) const;
const std::string& getTypeName(Type type) const;
const std::string& getTypeShortName(Type type) const;
size_t getTypeHash(Type type) const { return _hash[type]; }
};
NameServer& nameserver();
}
#endif