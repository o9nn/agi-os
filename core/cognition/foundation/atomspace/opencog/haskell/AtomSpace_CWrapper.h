#include <opencog/atomspace/AtomSpace.h>
extern "C"
{
using namespace opencog;
AtomSpace* AtomSpace_new( AtomSpace * parent_ptr );
void AtomSpace_delete( AtomSpace* this_ptr );
int AtomSpace_addNode( AtomSpace* this_ptr
, const char* type
, const char* name
, Handle* uuid_out );
int AtomSpace_addLink( AtomSpace* this_ptr
, const char* type
, const Handle** outgoing
, int size
, Handle* uuid_out );
int AtomSpace_getNode( AtomSpace* this_ptr
, const char* type
, const char* name
, Handle* uuid_out );
int AtomSpace_getLink( AtomSpace* this_ptr
, const char* type
, const Handle** outgoing
, int size
, Handle* uuid_out );
int AtomSpace_removeAtom( AtomSpace* this_ptr
, Handle* uuid );
int AtomSpace_getAtomByHandle( AtomSpace* this_ptr
, Handle* handle
, int* node_or_link
, char** type
, char** name
, Handle** out
, int* out_len);
int AtomSpace_getAtom( AtomSpace * this_ptr
, Handle* id
, const char * name
, const char * type
, size_t * size
, Handle* outsetp);
void AtomSpace_debug( AtomSpace* this_ptr );
}