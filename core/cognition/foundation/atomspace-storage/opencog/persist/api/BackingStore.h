#ifndef _OPENCOG_BACKING_STORE_H
#define _OPENCOG_BACKING_STORE_H
#include <opencog/util/exceptions.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Node.h>
namespace opencog
{
class BackingStore
{
friend class BackingImplicator;
friend class BackingSatisfyingSet;
friend class BackingJoinCallback;
public:
virtual ~BackingStore() {}
virtual void getAtom(const Handle&);
virtual void fetchIncomingSet(AtomSpace*, const Handle&) = 0;
virtual void fetchIncomingByType(AtomSpace*, const Handle&, Type) = 0;
virtual void storeAtom(const Handle&, bool synchronous = false) = 0;
virtual void removeAtom(AtomSpace*, const Handle&, bool recursive)
{
throw IOException(TRACE_INFO, "Not implemented!");
}
virtual void preRemoveAtom(AtomSpace* as, const Handle& h,
bool recursive)
{
removeAtom(as, h, recursive);
}
virtual void postRemoveAtom(AtomSpace* as, const Handle& h,
bool recursive, bool extracted)
{}
virtual void storeValue(const Handle& atom, const Handle& key)
{
throw IOException(TRACE_INFO, "Not implemented!");
}
virtual void updateValue(const Handle& atom, const Handle& key,
const ValuePtr& delta)
{
throw IOException(TRACE_INFO, "Not implemented!");
}
virtual void loadValue(const Handle& atom, const Handle& key)
{
throw IOException(TRACE_INFO, "Not implemented!");
}
virtual void runQuery(const Handle& query, const Handle& key,
const Handle& metadata_key = Handle::UNDEFINED,
bool fresh=false);
virtual void loadType(AtomSpace*, Type) = 0;
virtual void loadAtomSpace(AtomSpace*) = 0;
virtual void storeAtomSpace(const AtomSpace*) = 0;
virtual HandleSeq loadFrameDAG(void)
{
throw IOException(TRACE_INFO, "Not implemented!");
}
virtual void storeFrameDAG(AtomSpace*)
{
throw IOException(TRACE_INFO, "Not implemented!");
}
virtual void deleteFrame(AtomSpace*)
{
throw IOException(TRACE_INFO, "Not implemented!");
}
virtual void barrier(AtomSpace* = nullptr) = 0;
protected:
virtual Handle getLink(Type, const HandleSeq&) {
throw IOException(TRACE_INFO, "Implementation is buggy!");
}
virtual Handle getNode(Type, const char *) {
throw IOException(TRACE_INFO, "Implementation is buggy!");
}
};
}
#endif