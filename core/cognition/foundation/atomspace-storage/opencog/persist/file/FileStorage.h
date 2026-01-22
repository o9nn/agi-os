#ifndef _OPENCOG_FILE_STORAGE_H
#define _OPENCOG_FILE_STORAGE_H
#include <opencog/persist/api/StorageNode.h>
namespace opencog
{
class FileStorageNode : public StorageNode
{
private:
std::string _filename;
FILE* _fh;
bool _already_loaded;
public:
FileStorageNode(Type t, const std::string& uri);
virtual ~FileStorageNode();
void open(void);
void close(void);
bool connected(void);
void kill_data(void);
void create(void) { erase(); }
void destroy(void) { erase(); }
void erase(void);
Handle getNode(Type, const char *);
Handle getLink(Type, const HandleSeq&);
void fetchIncomingSet(AtomSpace*, const Handle&);
void fetchIncomingByType(AtomSpace*, const Handle&, Type t);
void storeAtom(const Handle&, bool synchronous = false);
void removeAtom(AtomSpace*, const Handle&, bool recursive);
void storeValue(const Handle&, const Handle&);
void loadValue(const Handle&, const Handle&);
void loadType(AtomSpace*, Type);
void barrier();
void loadAtomSpace(AtomSpace*);
void storeAtomSpace(const AtomSpace*);
static Handle factory(const Handle&);
};
NODE_PTR_DECL(FileStorageNode)
#define createFileStorageNode CREATE_DECL(FileStorageNode)
}
#endif