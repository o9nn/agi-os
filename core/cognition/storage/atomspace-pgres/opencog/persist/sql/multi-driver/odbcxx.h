#ifndef _OPENCOG_PERSISTENT_ODBC_DRIVER_H
#define _OPENCOG_PERSISTENT_ODBC_DRIVER_H
#ifdef HAVE_ODBC_STORAGE
#include <sql.h>
#include <sqlext.h>
#include "llapi.h"
class ODBCRecordSet;
class ODBCConnection : public LLConnection
{
friend class ODBCRecordSet;
private:
bool need_qmark_escape;
SQLHENV sql_henv;
SQLHDBC sql_hdbc;
ODBCRecordSet *get_record_set(void);
public:
ODBCConnection(const char * uri);
~ODBCConnection();
LLRecordSet *exec(const char *, bool);
void extract_error(const char *);
};
class ODBCRecordSet : public LLRecordSet
{
friend class ODBCConnection;
private:
SQLHSTMT sql_hstmt;
void alloc_and_bind_cols(int ncols);
ODBCRecordSet(ODBCConnection *);
~ODBCRecordSet();
void get_column_labels(void);
public:
bool fetch_row(void);
void release(void);
};
#endif
#endif