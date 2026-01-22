#include <stdlib.h>
#include <unistd.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/persist/tlb/TLB.h>
#include "llapi.h"
#include "SQLAtomStorage.h"
using namespace opencog;
class SQLAtomStorage::Response
{
public:
LLRecordSet *rs;
UUID uuid;
Type itype;
const char* name;
const char* outlist;
int height;
double *floatval;
const char *stringval;
UUID *linkval;
private:
concurrent_stack<LLConnection*>& _pool;
LLConnection* _conn;
public:
Response(concurrent_stack<LLConnection*>& pool) :
rs(nullptr),
itype(0),
name(nullptr),
outlist(nullptr),
height(0),
floatval(0),
stringval(nullptr),
linkval(nullptr),
_pool(pool),
_conn(nullptr),
table(nullptr),
store(nullptr),
pvec(nullptr),
uvec(nullptr),
tname(""),
fltval(0),
strval(nullptr),
lnkval(nullptr),
intval(0)
{}
~Response()
{
if (rs) rs->release();
rs = nullptr;
if (_conn) _pool.push(_conn);
_conn = nullptr;
}
void exec(const char * buff)
{
if (rs) rs->release();
if (nullptr == _conn) _conn = _pool.value_pop();
rs = _conn->exec(buff, false);
}
void try_exec(const char * buff)
{
if (rs) rs->release();
if (nullptr == _conn) _conn = _pool.value_pop();
rs = _conn->exec(buff, true);
}
void exec(const std::string& str)
{
exec(str.c_str());
}
void try_exec(const std::string& str)
{
try_exec(str.c_str());
}
bool create_atom_column_cb(const char *colname, const char * colvalue)
{
if ('t' == colname[0])
{
itype = atoi(colvalue);
}
else if ('n' == colname[0])
{
name = colvalue;
}
else if ('o' == colname[0])
{
outlist = colvalue;
}
else if ('u' == colname[0])
{
uuid = strtoul(colvalue, NULL, 10);
}
return false;
}
bool create_atom_cb(void)
{
rs->foreach_column(&Response::create_atom_column_cb, this);
return true;
}
AtomSpace *table;
SQLAtomStorage *store;
bool load_all_atoms_cb(void)
{
rs->foreach_column(&Response::create_atom_column_cb, this);
try
{
PseudoPtr p(store->makeAtom(*this, uuid));
Handle atom(store->get_recursive_if_not_exists(p));
Handle h(table->storage_add_nocheck(atom));
store->_tlbuf.addAtom(h, uuid);
store->get_atom_values(h);
}
catch (const IOException& ex) {}
return false;
}
bool load_if_not_exists_cb(void)
{
rs->foreach_column(&Response::create_atom_column_cb, this);
Handle h(store->_tlbuf.getAtom(uuid));
if (nullptr == h)
{
PseudoPtr p(store->makeAtom(*this, uuid));
h = store->get_recursive_if_not_exists(p);
h = table->storage_add_nocheck(h);
store->_tlbuf.addAtom(h, uuid);
}
else
{
h = table->storage_add_nocheck(h);
}
store->get_atom_values(h);
return false;
}
std::vector<PseudoPtr> *pvec;
bool fetch_incoming_set_cb(void)
{
rs->foreach_column(&Response::create_atom_column_cb, this);
pvec->emplace_back(store->makeAtom(*this, uuid));
return false;
}
bool get_uuid_column_cb(const char *colname, const char * colvalue)
{
uuid = strtoul(colvalue, NULL, 10);
return false;
}
std::vector<UUID> *uvec;
bool get_uuid_cb(void)
{
rs->foreach_column(&Response::get_uuid_column_cb, this);
uvec->emplace_back(uuid);
return false;
}
bool type_cb(void)
{
rs->foreach_column(&Response::type_column_cb, this);
store->set_typemap(itype, tname);
return false;
}
const char * tname;
bool type_column_cb(const char *colname, const char * colvalue)
{
if (!strcmp(colname, "type"))
{
itype = atoi(colvalue);
}
else if (!strcmp(colname, "typename"))
{
tname = colvalue;
}
return false;
}
VUID vuid;
Type vtype;
const char * fltval;
const char * strval;
const char * lnkval;
UUID key;
bool get_value_cb(void)
{
rs->foreach_column(&Response::get_value_column_cb, this);
return true;
}
bool get_value_column_cb(const char *colname, const char * colvalue)
{
if ('f' == colname[0])
{
fltval = colvalue;
}
else if ('s' == colname[0])
{
strval = colvalue;
}
else if ('l' == colname[0])
{
lnkval = colvalue;
}
else if ('t' == colname[0])
{
vtype = atoi(colvalue);
}
else if ('k' == colname[0])
{
key = atol(colvalue);
}
else if ('a' == colname[0])
{
uuid = atol(colvalue);
}
return false;
}
Handle atom;
bool get_all_values_cb(void)
{
rs->foreach_column(&Response::get_value_column_cb, this);
Handle hkey(store->_tlbuf.getAtom(key));
if (nullptr == hkey)
{
PseudoPtr pu(store->petAtom(key));
hkey = store->get_recursive_if_not_exists(pu);
if (table) hkey = table->storage_add_nocheck(hkey);
else if (atom->getAtomSpace())
hkey = atom->getAtomSpace()->storage_add_nocheck(hkey);
store->_tlbuf.addAtom(hkey, key);
}
if (nullptr == hkey->getAtomSpace() and
nullptr != atom->getAtomSpace())
{
hkey = atom->getAtomSpace()->storage_add_nocheck(hkey);
store->_tlbuf.addAtom(hkey, key);
}
ValuePtr pap = store->doUnpackValue(*this);
atom->setValue(hkey, pap);
return false;
}
unsigned long intval;
bool intval_cb(void)
{
rs->foreach_column(&Response::intval_column_cb, this);
return false;
}
bool intval_column_cb(const char *colname, const char * colvalue)
{
intval = strtoul(colvalue, NULL, 10);
return false;
}
};