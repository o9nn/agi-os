#ifndef	_STATEMENT_H
#define	_STATEMENT_H
#include "routine.h"
typedef enum statement_kind
{
skRoutine,
skImport,
skUImport,
skSImport,
skRCSDecl,
} statement_kind_t;
typedef struct statement
{
statement_kind_t stKind;
struct statement *stNext;
union
{
routine_t *_stRoutine;
const_string_t _stFileName;
} data;
} statement_t;
#define	stRoutine	data._stRoutine
#define	stFileName	data._stFileName
#define stNULL		((statement_t *) 0)
extern statement_t *stAlloc(void);
extern statement_t *StatementList;
#endif