#ifndef _LG_DICT_AFFIX_H_
#define _LG_DICT_AFFIX_H_
#include "dict-common.h"
typedef enum {
AFDICT_RPUNC,
AFDICT_LPUNC,
AFDICT_MPUNC,
AFDICT_UNITS,
AFDICT_SUF,
AFDICT_PRE,
AFDICT_MPRE,
AFDICT_QUOTES,
AFDICT_BULLETS,
AFDICT_INFIXMARK,
AFDICT_STEMSUBSCR,
AFDICT_SANEMORPHISM,
AFDICT_REGPRE,
AFDICT_REGMID,
AFDICT_REGSUF,
AFDICT_REGALTS,
AFDICT_REGPARTS,
AFDICT_NUM_ENTRIES
} afdict_classnum;
#define AFDICT_CLASSNAMES1 \
"RPUNC", \
"LPUNC", \
"MPUNC", \
"UNITS", \
"SUF", \
"PRE", \
"MPRE", \
"QUOTES", \
"BULLETS", \
"INFIXMARK", \
"STEMSUBSCR", \
"SANEMORPHISM",
#define AFDICT_CLASSNAMES2 \
"REGPRE", \
"REGMID", \
"REGSUF", \
"REGALTS", \
"REGPARTS",
static const char * const afdict_classname[] =
{AFDICT_CLASSNAMES1 AFDICT_CLASSNAMES2};
#define AFCLASS(afdict, class) (&afdict->afdict_class[class])
Afdict_class * afdict_find(Dictionary, const char *, bool);
#define INFIX_MARK(afdict) \
((NULL == afdict) ? '\0' : (AFCLASS(afdict, AFDICT_INFIXMARK)->string[0][0]))
static const afdict_classnum affix_strippable[] =
{AFDICT_UNITS, AFDICT_LPUNC, AFDICT_RPUNC, AFDICT_MPUNC};
static inline int get_affix_regex_cg(const char *s)
{
if (s[0] != '/') return -1;
const char *endslash = strrchr(s, '/');
if ((endslash == NULL) || (endslash < s + 3)) return -1;
if (((endslash[1] == '.') || (endslash[1] == SUBSCRIPT_MARK)) &&
(endslash[2] == '\\'))
{
if ((endslash[3] >= '0') && (endslash[3] <= '9'))
return (endslash[3] - '0');
}
return -1;
}
#endif