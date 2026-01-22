#include <stdlib.h>
#include <unistd.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/core/TypeNode.h>
#include "llapi.h"
#include "BridgeStorage.h"
using namespace opencog;
class BridgeStorage::Response
{
public:
LLRecordSet *rs;
private:
concurrent_stack<LLConnection*>& _pool;
LLConnection* _conn;
public:
Response(concurrent_stack<LLConnection*>& pool) :
rs(nullptr),
_pool(pool),
_conn(nullptr),
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
std::vector<std::string>* strvec;
bool strvec_cb(void)
{
rs->foreach_column(&Response::strval_column_cb, this);
return false;
}
bool strval_column_cb(const char *colname, const char * colvalue)
{
strvec->emplace_back(colvalue);
return false;
}
AtomSpace* as;
HandleSeq* tentries;
Handle vcol;
Handle tcol;
bool tabledesc_cb(void)
{
tcol = nullptr;
rs->foreach_column(&Response::table_column_cb, this);
if (tcol)
{
Handle tyv = as->add_link(TYPED_VARIABLE_LINK, vcol, tcol);
tentries->emplace_back(tyv);
}
return false;
}
bool table_column_cb(const char *colname, const char * colvalue)
{
if ('c' == colname[0])
{
vcol = as->add_node(VARIABLE_NODE, std::string(colvalue));
}
else if ('t' == colname[0])
{
if (!strcmp(colvalue, "text") or
!strcmp(colvalue, "varchar"))
{
tcol = as->add_node(TYPE_NODE, "ConceptNode");
}
else
if (!strcmp(colvalue, "int4") or
!strcmp(colvalue, "int2") or
!strcmp(colvalue, "int8") or
!strcmp(colvalue, "float4") or
!strcmp(colvalue, "float8") or
!strcmp(colvalue, "bool"))
{
tcol = as->add_node(TYPE_NODE, "NumberNode");
}
else
if (!strcmp(colvalue, "timestamp") or
!strcmp(colvalue, "date"))
{
tcol = nullptr;
}
else
if (!strcmp(colvalue, "bpchar"))
{
tcol = nullptr;
}
else
if (!strcmp(colvalue, "jsonb"))
{
tcol = nullptr;
}
else
printf("duuuude unknown coltype >>%s<<\n", colvalue);
}
return false;
}
Handle pred;
HandleSeq cols;
HandleSeq elts;
size_t it;
size_t nrows;
bool tabledata_cb(void)
{
it = 0;
elts.clear();
rs->foreach_column(&Response::table_row_cb, this);
if (0 < elts.size())
{
Handle row = as->add_link(LIST_LINK, HandleSeq(elts));
as->add_link(EDGE_LINK, pred, row);
nrows++;
}
return false;
}
bool table_row_cb(const char *colname, const char * colvalue)
{
const Handle& typed_var = cols.at(it);
if (typed_var->getOutgoingAtom(0)->get_name().compare(colname))
throw RuntimeException(TRACE_INFO,
"Intrnal Error: column names don't match");
TypeNodePtr tnp = TypeNodeCast(typed_var->getOutgoingAtom(1));
Handle h = as->add_node(tnp->get_kind(), colvalue);
elts.emplace_back(h);
it++;
return false;
}
};