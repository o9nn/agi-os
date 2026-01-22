#ifndef __REGEXP_LIBRARY_H__
#define __REGEXP_LIBRARY_H__
#if !defined (_POSIX_C_SOURCE) && !defined (_POSIX_SOURCE) && defined (VMS)
#include <stddef.h>
#endif
typedef unsigned reg_syntax_t;
#define RE_BACKSLASH_ESCAPE_IN_LISTS (1)
#define RE_BK_PLUS_QM (RE_BACKSLASH_ESCAPE_IN_LISTS << 1)
#define RE_CHAR_CLASSES (RE_BK_PLUS_QM << 1)
#define RE_CONTEXT_INDEP_ANCHORS (RE_CHAR_CLASSES << 1)
#define RE_CONTEXT_INDEP_OPS (RE_CONTEXT_INDEP_ANCHORS << 1)
#define RE_CONTEXT_INVALID_OPS (RE_CONTEXT_INDEP_OPS << 1)
#define RE_DOT_NEWLINE (RE_CONTEXT_INVALID_OPS << 1)
#define RE_DOT_NOT_NULL (RE_DOT_NEWLINE << 1)
#define RE_HAT_LISTS_NOT_NEWLINE (RE_DOT_NOT_NULL << 1)
#define RE_INTERVALS (RE_HAT_LISTS_NOT_NEWLINE << 1)
#define RE_LIMITED_OPS (RE_INTERVALS << 1)
#define RE_NEWLINE_ALT (RE_LIMITED_OPS << 1)
#define RE_NO_BK_BRACES (RE_NEWLINE_ALT << 1)
#define RE_NO_BK_PARENS (RE_NO_BK_BRACES << 1)
#define RE_NO_BK_REFS (RE_NO_BK_PARENS << 1)
#define RE_NO_BK_VBAR (RE_NO_BK_REFS << 1)
#define RE_NO_EMPTY_RANGES (RE_NO_BK_VBAR << 1)
#define RE_UNMATCHED_RIGHT_PAREN_ORD (RE_NO_EMPTY_RANGES << 1)
#define RE_NO_POSIX_BACKTRACKING (RE_UNMATCHED_RIGHT_PAREN_ORD << 1)
extern reg_syntax_t re_syntax_options;
#ifdef emacs
extern Lisp_Object re_match_object;
#endif
#define RE_SYNTAX_EMACS 0
#define RE_SYNTAX_AWK \
(RE_BACKSLASH_ESCAPE_IN_LISTS | RE_DOT_NOT_NULL \
| RE_NO_BK_PARENS | RE_NO_BK_REFS \
| RE_NO_BK_VBAR | RE_NO_EMPTY_RANGES \
| RE_UNMATCHED_RIGHT_PAREN_ORD)
#define RE_SYNTAX_POSIX_AWK \
(RE_SYNTAX_POSIX_EXTENDED | RE_BACKSLASH_ESCAPE_IN_LISTS)
#define RE_SYNTAX_GREP \
(RE_BK_PLUS_QM | RE_CHAR_CLASSES \
| RE_HAT_LISTS_NOT_NEWLINE | RE_INTERVALS \
| RE_NEWLINE_ALT)
#define RE_SYNTAX_EGREP \
(RE_CHAR_CLASSES | RE_CONTEXT_INDEP_ANCHORS \
| RE_CONTEXT_INDEP_OPS | RE_HAT_LISTS_NOT_NEWLINE \
| RE_NEWLINE_ALT | RE_NO_BK_PARENS \
| RE_NO_BK_VBAR)
#define RE_SYNTAX_POSIX_EGREP \
(RE_SYNTAX_EGREP | RE_INTERVALS | RE_NO_BK_BRACES)
#define RE_SYNTAX_ED RE_SYNTAX_POSIX_BASIC
#define RE_SYNTAX_SED RE_SYNTAX_POSIX_BASIC
#define _RE_SYNTAX_POSIX_COMMON \
(RE_CHAR_CLASSES | RE_DOT_NEWLINE | RE_DOT_NOT_NULL \
| RE_INTERVALS | RE_NO_EMPTY_RANGES)
#define RE_SYNTAX_POSIX_BASIC \
(_RE_SYNTAX_POSIX_COMMON | RE_BK_PLUS_QM)
#define RE_SYNTAX_POSIX_MINIMAL_BASIC \
(_RE_SYNTAX_POSIX_COMMON | RE_LIMITED_OPS)
#define RE_SYNTAX_POSIX_EXTENDED \
(_RE_SYNTAX_POSIX_COMMON | RE_CONTEXT_INDEP_ANCHORS \
| RE_CONTEXT_INDEP_OPS | RE_NO_BK_BRACES \
| RE_NO_BK_PARENS | RE_NO_BK_VBAR \
| RE_UNMATCHED_RIGHT_PAREN_ORD)
#define RE_SYNTAX_POSIX_MINIMAL_EXTENDED \
(_RE_SYNTAX_POSIX_COMMON | RE_CONTEXT_INDEP_ANCHORS \
| RE_CONTEXT_INVALID_OPS | RE_NO_BK_BRACES \
| RE_NO_BK_PARENS | RE_NO_BK_REFS \
| RE_NO_BK_VBAR | RE_UNMATCHED_RIGHT_PAREN_ORD)
#ifdef RE_DUP_MAX
#undef RE_DUP_MAX
#endif
#define RE_DUP_MAX ((1 << 15) - 1)
#define REG_EXTENDED 1
#define REG_ICASE (REG_EXTENDED << 1)
#define REG_NEWLINE (REG_ICASE << 1)
#define REG_NOSUB (REG_NEWLINE << 1)
#define REG_NOTBOL 1
#define REG_NOTEOL (1 << 1)
typedef enum
{
REG_NOERROR = 0,
REG_NOMATCH,
REG_BADPAT,
REG_ECOLLATE,
REG_ECTYPE,
REG_EESCAPE,
REG_ESUBREG,
REG_EBRACK,
REG_EPAREN,
REG_EBRACE,
REG_BADBR,
REG_ERANGE,
REG_ESPACE,
REG_BADRPT,
REG_EEND,
REG_ESIZE,
REG_ERPAREN
} reg_errcode_t;
#ifndef RE_TRANSLATE_TYPE
#define RE_TRANSLATE_TYPE char *
#define RE_TRANSLATE(TBL, C) ((TBL)[C])
#define RE_TRANSLATE_P(TBL) (TBL)
#endif
struct re_pattern_buffer
{
unsigned char *buffer;
unsigned long allocated;
unsigned long used;
reg_syntax_t syntax;
char *fastmap;
RE_TRANSLATE_TYPE translate;
size_t re_nsub;
unsigned can_be_null : 1;
#define REGS_UNALLOCATED 0
#define REGS_REALLOCATE 1
#define REGS_FIXED 2
unsigned regs_allocated : 2;
unsigned fastmap_accurate : 1;
unsigned no_sub : 1;
unsigned not_bol : 1;
unsigned not_eol : 1;
unsigned newline_anchor : 1;
unsigned multibyte : 1;
};
typedef struct re_pattern_buffer regex_t;
typedef int regoff_t;
struct re_registers
{
unsigned num_regs;
regoff_t *start;
regoff_t *end;
};
#ifndef RE_NREGS
#define RE_NREGS 30
#endif
typedef struct
{
regoff_t rm_so;
regoff_t rm_eo;
} regmatch_t;
#if __STDC__
#define _RE_ARGS(args) args
#else
#define _RE_ARGS(args) ()
#endif
extern reg_syntax_t re_set_syntax _RE_ARGS ((reg_syntax_t syntax));
extern const char *re_compile_pattern
_RE_ARGS ((const char *pattern, int length,
struct re_pattern_buffer *buffer));
extern int re_compile_fastmap _RE_ARGS ((struct re_pattern_buffer *buffer));
extern int re_search
_RE_ARGS ((struct re_pattern_buffer *buffer, const char *string,
int length, int start, int range, struct re_registers *regs));
extern int re_search_2
_RE_ARGS ((struct re_pattern_buffer *buffer, const char *string1,
int length1, const char *string2, int length2,
int start, int range, struct re_registers *regs, int stop));
extern int re_match
_RE_ARGS ((struct re_pattern_buffer *buffer, const char *string,
int length, int start, struct re_registers *regs));
extern int re_match_2
_RE_ARGS ((struct re_pattern_buffer *buffer, const char *string1,
int length1, const char *string2, int length2,
int start, struct re_registers *regs, int stop));
extern void re_set_registers
_RE_ARGS ((struct re_pattern_buffer *buffer, struct re_registers *regs,
unsigned num_regs, regoff_t *starts, regoff_t *ends));
#ifdef _REGEX_RE_COMP
extern char *re_comp _RE_ARGS (());
extern int re_exec _RE_ARGS (());
#endif
extern int regcomp _RE_ARGS ((regex_t *preg, const char *pattern, int cflags));
extern int regexec
_RE_ARGS ((const regex_t *preg, const char *string, size_t nmatch,
regmatch_t pmatch[], int eflags));
extern size_t regerror
_RE_ARGS ((int errcode, const regex_t *preg, char *errbuf,
size_t errbuf_size));
extern void regfree _RE_ARGS ((regex_t *preg));
#endif