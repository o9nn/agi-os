#ifndef _OPENCOG_PERSISTENT_LL_DRIVER_H
#define _OPENCOG_PERSISTENT_LL_DRIVER_H
#include <stack>
#include <string>
class LLRecordSet;
class LLConnection
{
friend class LLRecordSet;
protected:
bool is_connected;
std::stack<LLRecordSet *> free_pool;
public:
LLConnection(void);
virtual ~LLConnection();
bool connected(void) const { return is_connected; }
virtual LLRecordSet *exec(const char *, bool=false) = 0;
};
class LLRecordSet
{
friend class LLConnection;
protected:
LLConnection *conn;
int ncols;
int arrsize;
char **column_labels;
int *column_datatype;
char **values;
int *vsizes;
LLRecordSet(LLConnection *);
virtual ~LLRecordSet();
virtual void get_column_labels(void) = 0;
int get_col_by_name (const char *);
public:
virtual bool fetch_row(void) = 0;
const char * get_value(const char * fieldname);
int get_column_count();
const char * get_column_value(int column);
virtual void release(void);
template<class T> bool
foreach_row(bool (T::*cb)(void), T *data)
{
while (fetch_row())
{
bool rc = (data->*cb) ();
if (rc) return rc;
}
return false;
}
template<class T> bool
foreach_column(bool (T::*cb)(const char *, const char *), T *data)
{
int i;
if (0 > ncols)
{
get_column_labels();
}
for (i=0; i<ncols; i++)
{
bool rc = (data->*cb) (column_labels[i], values[i]);
if (rc) return rc;
}
return false;
}
};
inline void escape_single_quotes(std::string &str)
{
std::string::size_type pos = 0;
pos = str.find ('\'', pos);
while (pos != std::string::npos)
{
str.insert(pos, 1, '\'');
pos += 2;
pos = str.find('\'', pos);
}
}
#endif