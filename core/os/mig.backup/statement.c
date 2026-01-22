#include <stdlib.h>
#include "global.h"
#include "error.h"
#include "statement.h"
statement_t *StatementList = stNULL;
static statement_t **last = &StatementList;
statement_t *
stAlloc(void)
{
statement_t *new;
new = malloc(sizeof *new);
if (new == stNULL)
fatal("stAlloc(): %s", unix_error_string(errno));
*last = new;
last = &new->stNext;
new->stNext = stNULL;
return new;
}