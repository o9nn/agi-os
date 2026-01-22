#ifndef _OPENCOG_PERSISTENT_POSTGRES_DRIVER_H
#define _OPENCOG_PERSISTENT_POSTGRES_DRIVER_H
#ifdef HAVE_PGSQL_STORAGE
#include <libpq-fe.h>
#include "llapi.h"
class LLPGRecordSet;
class LLPGConnection : public LLConnection
{
friend class LLPGRecordSet;
private:
PGconn* _pgconn;
LLPGRecordSet* get_record_set(void);
public:
LLPGConnection(const char * uri);
~LLPGConnection();
LLRecordSet *exec(const char *, bool);
};
class LLPGRecordSet : public LLRecordSet
{
friend class LLPGConnection;
private:
PGresult* _result;
int _nrows;
int _curr_row;
void setup_cols(int ncols);
LLPGRecordSet(LLPGConnection *);
~LLPGRecordSet();
void get_column_labels(void);
public:
bool fetch_row(void);
void release(void);
};
#endif
#endif