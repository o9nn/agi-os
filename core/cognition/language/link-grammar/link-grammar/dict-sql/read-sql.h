#ifndef READ_SQL_H
#define READ_SQL_H
#include "link-includes.h"
#ifdef HAVE_SQLITE3
Dictionary dictionary_create_from_db(const char *lang);
#endif
#endif