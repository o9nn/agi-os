#if defined (_AIX) && !defined (REGEX_MALLOC)
#pragma alloca
#endif
#undef _GNU_SOURCE
#define _GNU_SOURCE
#ifdef emacs
#define PTR_TO_OFFSET(d) \
POS_AS_IN_BUFFER (MATCHING_IN_FIRST_STRING \
? (d) - string1 : (d) - (string2 - size1))
#define POS_AS_IN_BUFFER(p) ((p) + (NILP (re_match_object) || BUFFERP (re_match_object)))
#else
#define PTR_TO_OFFSET(d) 0
#endif
#include "config.h"
#include <sys/types.h>
#if HAVE_LIBINTL_H || defined (_LIBC)
# include <libintl.h>
#else
# define gettext(msgid) (msgid)
#endif
#ifndef gettext_noop
#define gettext_noop(String) String
#endif
#ifdef emacs
#include "lisp.h"
#include "buffer.h"
#define SYNTAX_ENTRY_VIA_PROPERTY
#include "syntax.h"
#include "charset.h"
#include "category.h"
#define malloc xmalloc
#define realloc xrealloc
#define free xfree
#else
#undef REL_ALLOC
#if defined (STDC_HEADERS) || defined (_LIBC)
#include <stdlib.h>
#else
char *malloc ();
char *realloc ();
#endif
#ifdef INHIBIT_STRING_HEADER
#if !(defined (HAVE_BZERO) && defined (HAVE_BCOPY))
#if !defined (bzero) && !defined (bcopy)
#undef INHIBIT_STRING_HEADER
#endif
#endif
#endif
#ifndef INHIBIT_STRING_HEADER
#if defined (HAVE_STRING_H) || defined (STDC_HEADERS) || defined (_LIBC)
#include <string.h>
#ifndef bcmp
#define bcmp(s1, s2, n) memcmp ((s1), (s2), (n))
#endif
#ifndef bcopy
#define bcopy(s, d, n) memcpy ((d), (s), (n))
#endif
#ifndef bzero
#define bzero(s, n) memset ((s), 0, (n))
#endif
#else
#include <strings.h>
#endif
#endif
#ifndef Sword
#define Sword 1
#endif
#ifdef SWITCH_ENUM_BUG
#define SWITCH_ENUM_CAST(x) ((int)(x))
#else
#define SWITCH_ENUM_CAST(x) (x)
#endif
#ifdef SYNTAX_TABLE
extern char *re_syntax_table;
#else
#define CHAR_SET_SIZE 256
static char re_syntax_table[CHAR_SET_SIZE];
static void
init_syntax_once ()
{
register int c;
static int done = 0;
if (done)
return;
bzero (re_syntax_table, sizeof re_syntax_table);
for (c = 'a'; c <= 'z'; c++)
re_syntax_table[c] = Sword;
for (c = 'A'; c <= 'Z'; c++)
re_syntax_table[c] = Sword;
for (c = '0'; c <= '9'; c++)
re_syntax_table[c] = Sword;
re_syntax_table['_'] = Sword;
done = 1;
}
#endif
#define SYNTAX(c) re_syntax_table[c]
#define BASE_LEADING_CODE_P(c) (0)
#define WORD_BOUNDARY_P(c1, c2) (0)
#define CHAR_HEAD_P(p) (1)
#define SINGLE_BYTE_CHAR_P(c) (1)
#define SAME_CHARSET_P(c1, c2) (1)
#define MULTIBYTE_FORM_LENGTH(p, s) (1)
#define STRING_CHAR(p, s) (*(p))
#define STRING_CHAR_AND_LENGTH(p, s, actual_len) ((actual_len) = 1, *(p))
#define GET_CHAR_AFTER_2(c, p, str1, end1, str2, end2) \
(c = ((p) == (end1) ? *(str2) : *(p)))
#define GET_CHAR_BEFORE_2(c, p, str1, end1, str2, end2) \
(c = ((p) == (str2) ? *((end1) - 1) : *((p) - 1)))
#endif
#include "regex.h"
#include <ctype.h>
#if defined (STDC_HEADERS) || (!defined (isascii) && !defined (HAVE_ISASCII))
#define ISASCII(c) 1
#else
#define ISASCII(c) isascii(c)
#endif
#ifdef isblank
#define ISBLANK(c) (ISASCII (c) && isblank (c))
#else
#define ISBLANK(c) ((c) == ' ' || (c) == '\t')
#endif
#ifdef isgraph
#define ISGRAPH(c) (ISASCII (c) && isgraph (c))
#else
#define ISGRAPH(c) (ISASCII (c) && isprint (c) && !isspace (c))
#endif
#define ISPRINT(c) (ISASCII (c) && isprint (c))
#define ISDIGIT(c) (ISASCII (c) && isdigit (c))
#define ISALNUM(c) (ISASCII (c) && isalnum (c))
#define ISALPHA(c) (ISASCII (c) && isalpha (c))
#define ISCNTRL(c) (ISASCII (c) && iscntrl (c))
#define ISLOWER(c) (ISASCII (c) && islower (c))
#define ISPUNCT(c) (ISASCII (c) && ispunct (c))
#define ISSPACE(c) (ISASCII (c) && isspace (c))
#define ISUPPER(c) (ISASCII (c) && isupper (c))
#define ISXDIGIT(c) (ISASCII (c) && isxdigit (c))
#ifndef NULL
#define NULL (void *)0
#endif
#undef SIGN_EXTEND_CHAR
#if __STDC__
#define SIGN_EXTEND_CHAR(c) ((signed char) (c))
#else
#define SIGN_EXTEND_CHAR(c) ((((unsigned char) (c)) ^ 128) - 128)
#endif
#ifdef REGEX_MALLOC
#define REGEX_ALLOCATE malloc
#define REGEX_REALLOCATE(source, osize, nsize) realloc (source, nsize)
#define REGEX_FREE free
#else
#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else
#if HAVE_ALLOCA_H
#include <alloca.h>
#else
#if 0
#ifndef _AIX
char *alloca ();
#endif
#endif
#endif
#endif
#endif
#define REGEX_ALLOCATE alloca
#define REGEX_REALLOCATE(source, osize, nsize) \
(destination = (char *) alloca (nsize), \
bcopy (source, destination, osize), \
destination)
#define REGEX_FREE(arg) ((void)0)
#endif
#if defined (REL_ALLOC) && defined (REGEX_MALLOC)
#define REGEX_ALLOCATE_STACK(size) \
r_alloc (&failure_stack_ptr, (size))
#define REGEX_REALLOCATE_STACK(source, osize, nsize) \
r_re_alloc (&failure_stack_ptr, (nsize))
#define REGEX_FREE_STACK(ptr) \
r_alloc_free (&failure_stack_ptr)
#else
#ifdef REGEX_MALLOC
#define REGEX_ALLOCATE_STACK malloc
#define REGEX_REALLOCATE_STACK(source, osize, nsize) realloc (source, nsize)
#define REGEX_FREE_STACK free
#else
#define REGEX_ALLOCATE_STACK alloca
#define REGEX_REALLOCATE_STACK(source, osize, nsize) \
REGEX_REALLOCATE (source, osize, nsize)
#define REGEX_FREE_STACK(arg)
#endif
#endif
#define FIRST_STRING_P(ptr) \
(size1 && string1 <= (ptr) && (ptr) <= string1 + size1)
#define TALLOC(n, t) ((t *) malloc ((n) * sizeof (t)))
#define RETALLOC(addr, n, t) ((addr) = (t *) realloc (addr, (n) * sizeof (t)))
#define RETALLOC_IF(addr, n, t) \
if (addr) RETALLOC((addr), (n), t); else (addr) = TALLOC ((n), t)
#define REGEX_TALLOC(n, t) ((t *) REGEX_ALLOCATE ((n) * sizeof (t)))
#define BYTEWIDTH 8
#define STREQ(s1, s2) ((strcmp (s1, s2) == 0))
#undef MAX
#undef MIN
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))
typedef char boolean;
#define false 0
#define true 1
static int re_match_2_internal ();
typedef enum
{
no_op = 0,
succeed,
exactn,
anychar,
charset,
charset_not,
start_memory,
stop_memory,
duplicate,
begline,
endline,
begbuf,
endbuf,
jump,
jump_past_alt,
on_failure_jump,
on_failure_keep_string_jump,
pop_failure_jump,
maybe_pop_jump,
dummy_failure_jump,
push_dummy_failure,
succeed_n,
jump_n,
set_number_at,
wordchar,
notwordchar,
wordbeg,
wordend,
wordbound,
notwordbound
#ifdef emacs
,before_dot,
at_dot,
after_dot,
syntaxspec,
notsyntaxspec,
categoryspec,
notcategoryspec
#endif
} re_opcode_t;
#define STORE_NUMBER(destination, number) \
do { \
(destination)[0] = (number) & 0377; \
(destination)[1] = (number) >> 8; \
} while (0)
#define STORE_NUMBER_AND_INCR(destination, number) \
do { \
STORE_NUMBER (destination, number); \
(destination) += 2; \
} while (0)
#define EXTRACT_NUMBER(destination, source) \
do { \
(destination) = *(source) & 0377; \
(destination) += SIGN_EXTEND_CHAR (*((source) + 1)) << 8; \
} while (0)
#ifdef DEBUG
static void
extract_number (dest, source)
int *dest;
unsigned char *source;
{
int temp = SIGN_EXTEND_CHAR (*(source + 1));
*dest = *source & 0377;
*dest += temp << 8;
}
#ifndef EXTRACT_MACROS
#undef EXTRACT_NUMBER
#define EXTRACT_NUMBER(dest, src) extract_number (&dest, src)
#endif
#endif
#define EXTRACT_NUMBER_AND_INCR(destination, source) \
do { \
EXTRACT_NUMBER (destination, source); \
(source) += 2; \
} while (0)
#ifdef DEBUG
static void
extract_number_and_incr (destination, source)
int *destination;
unsigned char **source;
{
extract_number (destination, *source);
*source += 2;
}
#ifndef EXTRACT_MACROS
#undef EXTRACT_NUMBER_AND_INCR
#define EXTRACT_NUMBER_AND_INCR(dest, src) \
extract_number_and_incr (&dest, &src)
#endif
#endif
#define STORE_CHARACTER_AND_INCR(destination, character) \
do { \
(destination)[0] = (character) & 0377; \
(destination)[1] = ((character) >> 8) & 0377; \
(destination)[2] = (character) >> 16; \
(destination) += 3; \
} while (0)
#define EXTRACT_CHARACTER(destination, source) \
do { \
(destination) = ((source)[0] \
| ((source)[1] << 8) \
| ((source)[2] << 16)); \
} while (0)
#define CHARSET_BITMAP_SIZE(p) ((p)[1] & 0x7F)
#define CHARSET_RANGE_TABLE_EXISTS_P(p) ((p)[1] & 0x80)
#define CHARSET_RANGE_TABLE(p) (&(p)[2 + CHARSET_BITMAP_SIZE (p)])
#define CHARSET_LOOKUP_BITMAP(p, c) \
((c) < CHARSET_BITMAP_SIZE (p) * BYTEWIDTH \
&& (p)[2 + (c) / BYTEWIDTH] & (1 << ((c) % BYTEWIDTH)))
#define CHARSET_RANGE_TABLE_END(range_table, count) \
((range_table) + (count) * 2 * 3)
#define CHARSET_LOOKUP_RANGE_TABLE_RAW(not, c, range_table, count) \
do \
{ \
int range_start, range_end; \
unsigned char *p; \
unsigned char *range_table_end \
= CHARSET_RANGE_TABLE_END ((range_table), (count)); \
\
for (p = (range_table); p < range_table_end; p += 2 * 3) \
{ \
EXTRACT_CHARACTER (range_start, p); \
EXTRACT_CHARACTER (range_end, p + 3); \
\
if (range_start <= (c) && (c) <= range_end) \
{ \
(not) = !(not); \
break; \
} \
} \
} \
while (0)
#define CHARSET_LOOKUP_RANGE_TABLE(not, c, charset) \
do \
{ \
\
int count; \
unsigned char *range_table = CHARSET_RANGE_TABLE (charset); \
\
EXTRACT_NUMBER_AND_INCR (count, range_table); \
CHARSET_LOOKUP_RANGE_TABLE_RAW ((not), (c), range_table, count); \
} \
while (0)
#ifdef DEBUG
#include <stdio.h>
#include <assert.h>
static int debug = 0;
#define DEBUG_STATEMENT(e) e
#define DEBUG_PRINT1(x) if (debug) printf (x)
#define DEBUG_PRINT2(x1, x2) if (debug) printf (x1, x2)
#define DEBUG_PRINT3(x1, x2, x3) if (debug) printf (x1, x2, x3)
#define DEBUG_PRINT4(x1, x2, x3, x4) if (debug) printf (x1, x2, x3, x4)
#define DEBUG_PRINT_COMPILED_PATTERN(p, s, e) \
if (debug) print_partial_compiled_pattern (s, e)
#define DEBUG_PRINT_DOUBLE_STRING(w, s1, sz1, s2, sz2) \
if (debug) print_double_string (w, s1, sz1, s2, sz2)
void
print_fastmap (fastmap)
char *fastmap;
{
unsigned was_a_range = 0;
unsigned i = 0;
while (i < (1 << BYTEWIDTH))
{
if (fastmap[i++])
{
was_a_range = 0;
putchar (i - 1);
while (i < (1 << BYTEWIDTH) && fastmap[i])
{
was_a_range = 1;
i++;
}
if (was_a_range)
{
printf ("-");
putchar (i - 1);
}
}
}
putchar ('\n');
}
void
print_partial_compiled_pattern (start, end)
unsigned char *start;
unsigned char *end;
{
int mcnt, mcnt2;
unsigned char *p = start;
unsigned char *pend = end;
if (start == NULL)
{
printf ("(null)\n");
return;
}
while (p < pend)
{
printf ("%d:\t", p - start);
switch ((re_opcode_t) *p++)
{
case no_op:
printf ("/no_op");
break;
case exactn:
mcnt = *p++;
printf ("/exactn/%d", mcnt);
do
{
putchar ('/');
putchar (*p++);
}
while (--mcnt);
break;
case start_memory:
mcnt = *p++;
printf ("/start_memory/%d/%d", mcnt, *p++);
break;
case stop_memory:
mcnt = *p++;
printf ("/stop_memory/%d/%d", mcnt, *p++);
break;
case duplicate:
printf ("/duplicate/%d", *p++);
break;
case anychar:
printf ("/anychar");
break;
case charset:
case charset_not:
{
register int c, last = -100;
register int in_range = 0;
printf ("/charset [%s",
(re_opcode_t) *(p - 1) == charset_not ? "^" : "");
assert (p + *p < pend);
for (c = 0; c < 256; c++)
if (c / 8 < *p
&& (p[1 + (c/8)] & (1 << (c % 8))))
{
if (last + 1 == c && ! in_range)
{
putchar ('-');
in_range = 1;
}
else if (last + 1 != c && in_range)
{
putchar (last);
in_range = 0;
}
if (! in_range)
putchar (c);
last = c;
}
if (in_range)
putchar (last);
putchar (']');
p += 1 + *p;
}
break;
case begline:
printf ("/begline");
break;
case endline:
printf ("/endline");
break;
case on_failure_jump:
extract_number_and_incr (&mcnt, &p);
printf ("/on_failure_jump to %d", p + mcnt - start);
break;
case on_failure_keep_string_jump:
extract_number_and_incr (&mcnt, &p);
printf ("/on_failure_keep_string_jump to %d", p + mcnt - start);
break;
case dummy_failure_jump:
extract_number_and_incr (&mcnt, &p);
printf ("/dummy_failure_jump to %d", p + mcnt - start);
break;
case push_dummy_failure:
printf ("/push_dummy_failure");
break;
case maybe_pop_jump:
extract_number_and_incr (&mcnt, &p);
printf ("/maybe_pop_jump to %d", p + mcnt - start);
break;
case pop_failure_jump:
extract_number_and_incr (&mcnt, &p);
printf ("/pop_failure_jump to %d", p + mcnt - start);
break;
case jump_past_alt:
extract_number_and_incr (&mcnt, &p);
printf ("/jump_past_alt to %d", p + mcnt - start);
break;
case jump:
extract_number_and_incr (&mcnt, &p);
printf ("/jump to %d", p + mcnt - start);
break;
case succeed_n:
extract_number_and_incr (&mcnt, &p);
extract_number_and_incr (&mcnt2, &p);
printf ("/succeed_n to %d, %d times", p + mcnt - start, mcnt2);
break;
case jump_n:
extract_number_and_incr (&mcnt, &p);
extract_number_and_incr (&mcnt2, &p);
printf ("/jump_n to %d, %d times", p + mcnt - start, mcnt2);
break;
case set_number_at:
extract_number_and_incr (&mcnt, &p);
extract_number_and_incr (&mcnt2, &p);
printf ("/set_number_at location %d to %d", p + mcnt - start, mcnt2);
break;
case wordbound:
printf ("/wordbound");
break;
case notwordbound:
printf ("/notwordbound");
break;
case wordbeg:
printf ("/wordbeg");
break;
case wordend:
printf ("/wordend");
#ifdef emacs
case before_dot:
printf ("/before_dot");
break;
case at_dot:
printf ("/at_dot");
break;
case after_dot:
printf ("/after_dot");
break;
case syntaxspec:
printf ("/syntaxspec");
mcnt = *p++;
printf ("/%d", mcnt);
break;
case notsyntaxspec:
printf ("/notsyntaxspec");
mcnt = *p++;
printf ("/%d", mcnt);
break;
#endif
case wordchar:
printf ("/wordchar");
break;
case notwordchar:
printf ("/notwordchar");
break;
case begbuf:
printf ("/begbuf");
break;
case endbuf:
printf ("/endbuf");
break;
default:
printf ("?%d", *(p-1));
}
putchar ('\n');
}
printf ("%d:\tend of pattern.\n", p - start);
}
void
print_compiled_pattern (bufp)
struct re_pattern_buffer *bufp;
{
unsigned char *buffer = bufp->buffer;
print_partial_compiled_pattern (buffer, buffer + bufp->used);
printf ("%d bytes used/%d bytes allocated.\n", bufp->used, bufp->allocated);
if (bufp->fastmap_accurate && bufp->fastmap)
{
printf ("fastmap: ");
print_fastmap (bufp->fastmap);
}
printf ("re_nsub: %d\t", bufp->re_nsub);
printf ("regs_alloc: %d\t", bufp->regs_allocated);
printf ("can_be_null: %d\t", bufp->can_be_null);
printf ("newline_anchor: %d\n", bufp->newline_anchor);
printf ("no_sub: %d\t", bufp->no_sub);
printf ("not_bol: %d\t", bufp->not_bol);
printf ("not_eol: %d\t", bufp->not_eol);
printf ("syntax: %d\n", bufp->syntax);
}
void
print_double_string (where, string1, size1, string2, size2)
const char *where;
const char *string1;
const char *string2;
int size1;
int size2;
{
unsigned this_char;
if (where == NULL)
printf ("(null)");
else
{
if (FIRST_STRING_P (where))
{
for (this_char = where - string1; this_char < size1; this_char++)
putchar (string1[this_char]);
where = string2;
}
for (this_char = where - string2; this_char < size2; this_char++)
putchar (string2[this_char]);
}
}
#else
#undef assert
#define assert(e)
#define DEBUG_STATEMENT(e)
#define DEBUG_PRINT1(x)
#define DEBUG_PRINT2(x1, x2)
#define DEBUG_PRINT3(x1, x2, x3)
#define DEBUG_PRINT4(x1, x2, x3, x4)
#define DEBUG_PRINT_COMPILED_PATTERN(p, s, e)
#define DEBUG_PRINT_DOUBLE_STRING(w, s1, sz1, s2, sz2)
#endif
reg_syntax_t re_syntax_options;
reg_syntax_t
re_set_syntax (syntax)
reg_syntax_t syntax;
{
reg_syntax_t ret = re_syntax_options;
re_syntax_options = syntax;
return ret;
}
static const char *re_error_msgid[] =
{
gettext_noop ("Success"),
gettext_noop ("No match"),
gettext_noop ("Invalid regular expression"),
gettext_noop ("Invalid collation character"),
gettext_noop ("Invalid character class name"),
gettext_noop ("Trailing backslash"),
gettext_noop ("Invalid back reference"),
gettext_noop ("Unmatched [ or [^"),
gettext_noop ("Unmatched ( or \\("),
gettext_noop ("Unmatched \\{"),
gettext_noop ("Invalid content of \\{\\}"),
gettext_noop ("Invalid range end"),
gettext_noop ("Memory exhausted"),
gettext_noop ("Invalid preceding regular expression"),
gettext_noop ("Premature end of regular expression"),
gettext_noop ("Regular expression too big"),
gettext_noop ("Unmatched ) or \\)"),
};
#define MATCH_MAY_ALLOCATE
#ifdef __GNUC__
#undef C_ALLOCA
#endif
#if (defined (C_ALLOCA) || defined (REGEX_MALLOC)) && defined (emacs)
#undef MATCH_MAY_ALLOCATE
#endif
#ifndef INIT_FAILURE_ALLOC
#define INIT_FAILURE_ALLOC 20
#endif
#if defined (MATCH_MAY_ALLOCATE)
int re_max_failures = 40000;
#else
int re_max_failures = 4000;
#endif
union fail_stack_elt
{
unsigned char *pointer;
int integer;
};
typedef union fail_stack_elt fail_stack_elt_t;
typedef struct
{
fail_stack_elt_t *stack;
unsigned size;
unsigned avail;
} fail_stack_type;
#define FAIL_STACK_EMPTY() (fail_stack.avail == 0)
#define FAIL_STACK_PTR_EMPTY() (fail_stack_ptr->avail == 0)
#define FAIL_STACK_FULL() (fail_stack.avail == fail_stack.size)
#ifdef MATCH_MAY_ALLOCATE
#define INIT_FAIL_STACK() \
do { \
fail_stack.stack = (fail_stack_elt_t *) \
REGEX_ALLOCATE_STACK (INIT_FAILURE_ALLOC * TYPICAL_FAILURE_SIZE \
* sizeof (fail_stack_elt_t)); \
\
if (fail_stack.stack == NULL) \
return -2; \
\
fail_stack.size = INIT_FAILURE_ALLOC; \
fail_stack.avail = 0; \
} while (0)
#define RESET_FAIL_STACK() REGEX_FREE_STACK (fail_stack.stack)
#else
#define INIT_FAIL_STACK() \
do { \
fail_stack.avail = 0; \
} while (0)
#define RESET_FAIL_STACK()
#endif
#define FAIL_STACK_GROWTH_FACTOR 4
#define GROW_FAIL_STACK(fail_stack) \
(((fail_stack).size * sizeof (fail_stack_elt_t) \
>= re_max_failures * TYPICAL_FAILURE_SIZE) \
? 0 \
: ((fail_stack).stack \
= (fail_stack_elt_t *) \
REGEX_REALLOCATE_STACK ((fail_stack).stack, \
(fail_stack).size * sizeof (fail_stack_elt_t), \
MIN (re_max_failures * TYPICAL_FAILURE_SIZE, \
((fail_stack).size * sizeof (fail_stack_elt_t) \
* FAIL_STACK_GROWTH_FACTOR))), \
\
(fail_stack).stack == NULL \
? 0 \
: ((fail_stack).size \
= (MIN (re_max_failures * TYPICAL_FAILURE_SIZE, \
((fail_stack).size * sizeof (fail_stack_elt_t) \
* FAIL_STACK_GROWTH_FACTOR)) \
/ sizeof (fail_stack_elt_t)), \
1)))
#define PUSH_PATTERN_OP(POINTER, FAIL_STACK) \
((FAIL_STACK_FULL () \
&& !GROW_FAIL_STACK (FAIL_STACK)) \
? 0 \
: ((FAIL_STACK).stack[(FAIL_STACK).avail++].pointer = POINTER, \
1))
#define PUSH_FAILURE_POINTER(item) \
fail_stack.stack[fail_stack.avail++].pointer = (unsigned char *) (item)
#define PUSH_FAILURE_INT(item) \
fail_stack.stack[fail_stack.avail++].integer = (item)
#define PUSH_FAILURE_ELT(item) \
fail_stack.stack[fail_stack.avail++] = (item)
#define POP_FAILURE_POINTER() fail_stack.stack[--fail_stack.avail].pointer
#define POP_FAILURE_INT() fail_stack.stack[--fail_stack.avail].integer
#define POP_FAILURE_ELT() fail_stack.stack[--fail_stack.avail]
#ifdef DEBUG
#define DEBUG_PUSH PUSH_FAILURE_INT
#define DEBUG_POP(item_addr) *(item_addr) = POP_FAILURE_INT ()
#else
#define DEBUG_PUSH(item)
#define DEBUG_POP(item_addr)
#endif
#define PUSH_FAILURE_POINT(pattern_place, string_place, failure_code) \
do { \
char *destination; \
\
int this_reg; \
\
DEBUG_STATEMENT (failure_id++); \
DEBUG_STATEMENT (nfailure_points_pushed++); \
DEBUG_PRINT2 ("\nPUSH_FAILURE_POINT #%u:\n", failure_id); \
DEBUG_PRINT2 ("  Before push, next avail: %d\n", (fail_stack).avail);\
DEBUG_PRINT2 ("			size: %d\n", (fail_stack).size);\
\
DEBUG_PRINT2 ("  slots needed: %d\n", NUM_FAILURE_ITEMS); \
DEBUG_PRINT2 ("	available: %d\n", REMAINING_AVAIL_SLOTS); \
\
\
while (REMAINING_AVAIL_SLOTS < NUM_FAILURE_ITEMS) \
{ \
if (!GROW_FAIL_STACK (fail_stack)) \
return failure_code; \
\
DEBUG_PRINT2 ("\n  Doubled stack; size now: %d\n", \
(fail_stack).size); \
DEBUG_PRINT2 ("	 slots available: %d\n", REMAINING_AVAIL_SLOTS);\
} \
\
\
DEBUG_PRINT1 ("\n"); \
\
if (1) \
for (this_reg = lowest_active_reg; this_reg <= highest_active_reg; \
this_reg++) \
{ \
DEBUG_PRINT2 ("  Pushing reg: %d\n", this_reg); \
DEBUG_STATEMENT (num_regs_pushed++); \
\
DEBUG_PRINT2 ("    start: 0x%x\n", regstart[this_reg]); \
PUSH_FAILURE_POINTER (regstart[this_reg]); \
\
DEBUG_PRINT2 ("    end: 0x%x\n", regend[this_reg]); \
PUSH_FAILURE_POINTER (regend[this_reg]); \
\
DEBUG_PRINT2 ("    info: 0x%x\n      ", reg_info[this_reg]); \
DEBUG_PRINT2 (" match_null=%d", \
REG_MATCH_NULL_STRING_P (reg_info[this_reg])); \
DEBUG_PRINT2 (" active=%d", IS_ACTIVE (reg_info[this_reg])); \
DEBUG_PRINT2 (" matched_something=%d", \
MATCHED_SOMETHING (reg_info[this_reg])); \
DEBUG_PRINT2 (" ever_matched=%d", \
EVER_MATCHED_SOMETHING (reg_info[this_reg])); \
DEBUG_PRINT1 ("\n"); \
PUSH_FAILURE_ELT (reg_info[this_reg].word); \
} \
\
DEBUG_PRINT2 ("  Pushing  low active reg: %d\n", lowest_active_reg);\
PUSH_FAILURE_INT (lowest_active_reg); \
\
DEBUG_PRINT2 ("  Pushing high active reg: %d\n", highest_active_reg);\
PUSH_FAILURE_INT (highest_active_reg); \
\
DEBUG_PRINT2 ("  Pushing pattern 0x%x: ", pattern_place); \
DEBUG_PRINT_COMPILED_PATTERN (bufp, pattern_place, pend); \
PUSH_FAILURE_POINTER (pattern_place); \
\
DEBUG_PRINT2 ("  Pushing string 0x%x: `", string_place); \
DEBUG_PRINT_DOUBLE_STRING (string_place, string1, size1, string2, \
size2); \
DEBUG_PRINT1 ("'\n"); \
PUSH_FAILURE_POINTER (string_place); \
\
DEBUG_PRINT2 ("  Pushing failure id: %u\n", failure_id); \
DEBUG_PUSH (failure_id); \
} while (0)
#define NUM_REG_ITEMS 3
#ifdef DEBUG
#define NUM_NONREG_ITEMS 5
#else
#define NUM_NONREG_ITEMS 4
#endif
#define TYPICAL_FAILURE_SIZE 20
#define NUM_FAILURE_ITEMS \
(((0 \
? 0 : highest_active_reg - lowest_active_reg + 1) \
* NUM_REG_ITEMS) \
+ NUM_NONREG_ITEMS)
#define REMAINING_AVAIL_SLOTS ((fail_stack).size - (fail_stack).avail)
#define POP_FAILURE_POINT(str, pat, low_reg, high_reg, regstart, regend, reg_info)\
{ \
DEBUG_STATEMENT (fail_stack_elt_t failure_id;) \
int this_reg; \
const unsigned char *string_temp; \
\
assert (!FAIL_STACK_EMPTY ()); \
\
\
DEBUG_PRINT1 ("POP_FAILURE_POINT:\n"); \
DEBUG_PRINT2 ("  Before pop, next avail: %d\n", fail_stack.avail); \
DEBUG_PRINT2 ("		     size: %d\n", fail_stack.size); \
\
assert (fail_stack.avail >= NUM_NONREG_ITEMS); \
\
DEBUG_POP (&failure_id); \
DEBUG_PRINT2 ("  Popping failure id: %u\n", failure_id); \
\
\
string_temp = POP_FAILURE_POINTER (); \
if (string_temp != NULL) \
str = (const char *) string_temp; \
\
DEBUG_PRINT2 ("  Popping string 0x%x: `", str); \
DEBUG_PRINT_DOUBLE_STRING (str, string1, size1, string2, size2); \
DEBUG_PRINT1 ("'\n"); \
\
pat = (unsigned char *) POP_FAILURE_POINTER (); \
DEBUG_PRINT2 ("  Popping pattern 0x%x: ", pat); \
DEBUG_PRINT_COMPILED_PATTERN (bufp, pat, pend); \
\
\
high_reg = (unsigned) POP_FAILURE_INT (); \
DEBUG_PRINT2 ("  Popping high active reg: %d\n", high_reg); \
\
low_reg = (unsigned) POP_FAILURE_INT (); \
DEBUG_PRINT2 ("  Popping  low active reg: %d\n", low_reg); \
\
if (1) \
for (this_reg = high_reg; this_reg >= low_reg; this_reg--) \
{ \
DEBUG_PRINT2 ("	   Popping reg: %d\n", this_reg); \
\
reg_info[this_reg].word = POP_FAILURE_ELT (); \
DEBUG_PRINT2 ("	     info: 0x%x\n", reg_info[this_reg]); \
\
regend[this_reg] = (const char *) POP_FAILURE_POINTER (); \
DEBUG_PRINT2 ("	     end: 0x%x\n", regend[this_reg]); \
\
regstart[this_reg] = (const char *) POP_FAILURE_POINTER (); \
DEBUG_PRINT2 ("	     start: 0x%x\n", regstart[this_reg]); \
} \
else \
{ \
for (this_reg = highest_active_reg; this_reg > high_reg; this_reg--) \
{ \
reg_info[this_reg].word.integer = 0; \
regend[this_reg] = 0; \
regstart[this_reg] = 0; \
} \
highest_active_reg = high_reg; \
} \
\
set_regs_matched_done = 0; \
DEBUG_STATEMENT (nfailure_points_popped++); \
}
typedef union
{
fail_stack_elt_t word;
struct
{
#define MATCH_NULL_UNSET_VALUE 3
unsigned match_null_string_p : 2;
unsigned is_active : 1;
unsigned matched_something : 1;
unsigned ever_matched_something : 1;
} bits;
} register_info_type;
#define REG_MATCH_NULL_STRING_P(R) ((R).bits.match_null_string_p)
#define IS_ACTIVE(R) ((R).bits.is_active)
#define MATCHED_SOMETHING(R) ((R).bits.matched_something)
#define EVER_MATCHED_SOMETHING(R) ((R).bits.ever_matched_something)
#define SET_REGS_MATCHED() \
do \
{ \
if (!set_regs_matched_done) \
{ \
unsigned r; \
set_regs_matched_done = 1; \
for (r = lowest_active_reg; r <= highest_active_reg; r++) \
{ \
MATCHED_SOMETHING (reg_info[r]) \
= EVER_MATCHED_SOMETHING (reg_info[r]) \
= 1; \
} \
} \
} \
while (0)
static char reg_unset_dummy;
#define REG_UNSET_VALUE (&reg_unset_dummy)
#define REG_UNSET(e) ((e) == REG_UNSET_VALUE)
static void store_op1 (), store_op2 ();
static void insert_op1 (), insert_op2 ();
static boolean at_begline_loc_p (), at_endline_loc_p ();
static boolean group_in_compile_stack ();
static reg_errcode_t compile_range ();
#ifndef PATFETCH
#define PATFETCH(c) \
do {if (p == pend) return REG_EEND; \
c = (unsigned char) *p++; \
if (RE_TRANSLATE_P (translate)) c = RE_TRANSLATE (translate, c); \
} while (0)
#endif
#define PATFETCH_RAW(c) \
do {if (p == pend) return REG_EEND; \
c = (unsigned char) *p++; \
} while (0)
#define PATUNFETCH p--
#ifndef TRANSLATE
#define TRANSLATE(d) \
(RE_TRANSLATE_P (translate) \
? (unsigned) RE_TRANSLATE (translate, (unsigned) (d)) : (d))
#endif
#define INIT_BUF_SIZE 32
#define GET_BUFFER_SPACE(n) \
while (b - bufp->buffer + (n) > bufp->allocated) \
EXTEND_BUFFER ()
#define BUF_PUSH(c) \
do { \
GET_BUFFER_SPACE (1); \
*b++ = (unsigned char) (c); \
} while (0)
#define BUF_PUSH_2(c1, c2) \
do { \
GET_BUFFER_SPACE (2); \
*b++ = (unsigned char) (c1); \
*b++ = (unsigned char) (c2); \
} while (0)
#define BUF_PUSH_3(c1, c2, c3) \
do { \
GET_BUFFER_SPACE (3); \
*b++ = (unsigned char) (c1); \
*b++ = (unsigned char) (c2); \
*b++ = (unsigned char) (c3); \
} while (0)
#define STORE_JUMP(op, loc, to) \
store_op1 (op, loc, (to) - (loc) - 3)
#define STORE_JUMP2(op, loc, to, arg) \
store_op2 (op, loc, (to) - (loc) - 3, arg)
#define INSERT_JUMP(op, loc, to) \
insert_op1 (op, loc, (to) - (loc) - 3, b)
#define INSERT_JUMP2(op, loc, to, arg) \
insert_op2 (op, loc, (to) - (loc) - 3, arg, b)
#define MAX_BUF_SIZE (1L << 16)
#define EXTEND_BUFFER() \
do { \
unsigned char *old_buffer = bufp->buffer; \
if (bufp->allocated == MAX_BUF_SIZE) \
return REG_ESIZE; \
bufp->allocated <<= 1; \
if (bufp->allocated > MAX_BUF_SIZE) \
bufp->allocated = MAX_BUF_SIZE; \
bufp->buffer = (unsigned char *) realloc (bufp->buffer, bufp->allocated);\
if (bufp->buffer == NULL) \
return REG_ESPACE; \
\
if (old_buffer != bufp->buffer) \
{ \
b = (b - old_buffer) + bufp->buffer; \
begalt = (begalt - old_buffer) + bufp->buffer; \
if (fixup_alt_jump) \
fixup_alt_jump = (fixup_alt_jump - old_buffer) + bufp->buffer;\
if (laststart) \
laststart = (laststart - old_buffer) + bufp->buffer; \
if (pending_exact) \
pending_exact = (pending_exact - old_buffer) + bufp->buffer; \
} \
} while (0)
#define MAX_REGNUM 255
typedef unsigned regnum_t;
typedef int pattern_offset_t;
typedef struct
{
pattern_offset_t begalt_offset;
pattern_offset_t fixup_alt_jump;
pattern_offset_t inner_group_offset;
pattern_offset_t laststart_offset;
regnum_t regnum;
} compile_stack_elt_t;
typedef struct
{
compile_stack_elt_t *stack;
unsigned size;
unsigned avail;
} compile_stack_type;
#define INIT_COMPILE_STACK_SIZE 32
#define COMPILE_STACK_EMPTY (compile_stack.avail == 0)
#define COMPILE_STACK_FULL (compile_stack.avail == compile_stack.size)
#define COMPILE_STACK_TOP (compile_stack.stack[compile_stack.avail])
struct range_table_work_area
{
int *table;
int allocated;
int used;
};
#define EXTEND_RANGE_TABLE_WORK_AREA(work_area, n) \
do { \
if (((work_area).used + (n)) * sizeof (int) > (work_area).allocated) \
{ \
(work_area).allocated += 16 * sizeof (int); \
if ((work_area).table) \
(work_area).table \
= (int *) realloc ((work_area).table, (work_area).allocated); \
else \
(work_area).table \
= (int *) malloc ((work_area).allocated); \
if ((work_area).table == 0) \
FREE_STACK_RETURN (REG_ESPACE); \
} \
} while (0)
#define SET_RANGE_TABLE_WORK_AREA(work_area, range_start, range_end) \
do { \
EXTEND_RANGE_TABLE_WORK_AREA ((work_area), 2); \
(work_area).table[(work_area).used++] = (range_start); \
(work_area).table[(work_area).used++] = (range_end); \
} while (0)
#define FREE_RANGE_TABLE_WORK_AREA(work_area) \
do { \
if ((work_area).table) \
free ((work_area).table); \
} while (0)
#define CLEAR_RANGE_TABLE_WORK_USED(work_area) ((work_area).used = 0)
#define RANGE_TABLE_WORK_USED(work_area) ((work_area).used)
#define RANGE_TABLE_WORK_ELT(work_area, i) ((work_area).table[i])
#define SET_LIST_BIT(c) \
(b[((unsigned char) (c)) / BYTEWIDTH] \
|= 1 << (((unsigned char) c) % BYTEWIDTH))
#define GET_UNSIGNED_NUMBER(num) \
{ if (p != pend) \
{ \
PATFETCH (c); \
while (ISDIGIT (c)) \
{ \
if (num < 0) \
num = 0; \
num = num * 10 + c - '0'; \
if (p == pend) \
break; \
PATFETCH (c); \
} \
} \
}
#define CHAR_CLASS_MAX_LENGTH 6
#define IS_CHAR_CLASS(string) \
(STREQ (string, "alpha") || STREQ (string, "upper") \
|| STREQ (string, "lower") || STREQ (string, "digit") \
|| STREQ (string, "alnum") || STREQ (string, "xdigit") \
|| STREQ (string, "space") || STREQ (string, "print") \
|| STREQ (string, "punct") || STREQ (string, "graph") \
|| STREQ (string, "cntrl") || STREQ (string, "blank"))
#ifndef MATCH_MAY_ALLOCATE
static fail_stack_type fail_stack;
static int regs_allocated_size;
static const char ** regstart, ** regend;
static const char ** old_regstart, ** old_regend;
static const char **best_regstart, **best_regend;
static register_info_type *reg_info;
static const char **reg_dummy;
static register_info_type *reg_info_dummy;
static
regex_grow_registers (num_regs)
int num_regs;
{
if (num_regs > regs_allocated_size)
{
RETALLOC_IF (regstart, num_regs, const char *);
RETALLOC_IF (regend, num_regs, const char *);
RETALLOC_IF (old_regstart, num_regs, const char *);
RETALLOC_IF (old_regend, num_regs, const char *);
RETALLOC_IF (best_regstart, num_regs, const char *);
RETALLOC_IF (best_regend, num_regs, const char *);
RETALLOC_IF (reg_info, num_regs, register_info_type);
RETALLOC_IF (reg_dummy, num_regs, const char *);
RETALLOC_IF (reg_info_dummy, num_regs, register_info_type);
regs_allocated_size = num_regs;
}
}
#endif
#define FREE_STACK_RETURN(value) \
do { \
FREE_RANGE_TABLE_WORK_AREA (range_table_work); \
free (compile_stack.stack); \
return value; \
} while (0)
static reg_errcode_t
regex_compile (pattern, size, syntax, bufp)
const char *pattern;
int size;
reg_syntax_t syntax;
struct re_pattern_buffer *bufp;
{
register unsigned int c, c1;
const char *p1;
register unsigned char *b;
compile_stack_type compile_stack;
#ifdef AIX
char *p = pattern;
#else
const char *p = pattern;
#endif
const char *pend = pattern + size;
RE_TRANSLATE_TYPE translate = bufp->translate;
unsigned char *pending_exact = 0;
unsigned char *laststart = 0;
unsigned char *begalt;
const char *beg_interval;
unsigned char *fixup_alt_jump = 0;
regnum_t regnum = 0;
struct range_table_work_area range_table_work;
#ifdef DEBUG
DEBUG_PRINT1 ("\nCompiling pattern: ");
if (debug)
{
unsigned debug_count;
for (debug_count = 0; debug_count < size; debug_count++)
putchar (pattern[debug_count]);
putchar ('\n');
}
#endif
compile_stack.stack = TALLOC (INIT_COMPILE_STACK_SIZE, compile_stack_elt_t);
if (compile_stack.stack == NULL)
return REG_ESPACE;
compile_stack.size = INIT_COMPILE_STACK_SIZE;
compile_stack.avail = 0;
range_table_work.table = 0;
range_table_work.allocated = 0;
bufp->syntax = syntax;
bufp->fastmap_accurate = 0;
bufp->not_bol = bufp->not_eol = 0;
bufp->used = 0;
bufp->re_nsub = 0;
#ifdef emacs
#else
bufp->multibyte = 0;
#endif
#if !defined (emacs) && !defined (SYNTAX_TABLE)
init_syntax_once ();
#endif
if (bufp->allocated == 0)
{
if (bufp->buffer)
{
RETALLOC (bufp->buffer, INIT_BUF_SIZE, unsigned char);
}
else
{
bufp->buffer = TALLOC (INIT_BUF_SIZE, unsigned char);
}
if (!bufp->buffer) FREE_STACK_RETURN (REG_ESPACE);
bufp->allocated = INIT_BUF_SIZE;
}
begalt = b = bufp->buffer;
while (p != pend)
{
PATFETCH (c);
switch (c)
{
case '^':
{
if (
p == pattern + 1
|| syntax & RE_CONTEXT_INDEP_ANCHORS
|| at_begline_loc_p (pattern, p, syntax))
BUF_PUSH (begline);
else
goto normal_char;
}
break;
case '$':
{
if (
p == pend
|| syntax & RE_CONTEXT_INDEP_ANCHORS
|| at_endline_loc_p (p, pend, syntax))
BUF_PUSH (endline);
else
goto normal_char;
}
break;
case '+':
case '?':
if ((syntax & RE_BK_PLUS_QM)
|| (syntax & RE_LIMITED_OPS))
goto normal_char;
handle_plus:
case '*':
if (!laststart)
{
if (syntax & RE_CONTEXT_INVALID_OPS)
FREE_STACK_RETURN (REG_BADRPT);
else if (!(syntax & RE_CONTEXT_INDEP_OPS))
goto normal_char;
}
{
boolean keep_string_p = false;
char zero_times_ok = 0, many_times_ok = 0;
for (;;)
{
zero_times_ok |= c != '+';
many_times_ok |= c != '?';
if (p == pend)
break;
PATFETCH (c);
if (c == '*'
|| (!(syntax & RE_BK_PLUS_QM) && (c == '+' || c == '?')))
;
else if (syntax & RE_BK_PLUS_QM && c == '\\')
{
if (p == pend) FREE_STACK_RETURN (REG_EESCAPE);
PATFETCH (c1);
if (!(c1 == '+' || c1 == '?'))
{
PATUNFETCH;
PATUNFETCH;
break;
}
c = c1;
}
else
{
PATUNFETCH;
break;
}
}
if (!laststart)
break;
if (many_times_ok)
{
assert (p - 1 > pattern);
GET_BUFFER_SPACE (3);
if (TRANSLATE ((unsigned char)*(p - 2)) == TRANSLATE ('.')
&& zero_times_ok
&& p < pend
&& TRANSLATE ((unsigned char)*p) == TRANSLATE ('\n')
&& !(syntax & RE_DOT_NEWLINE))
{
STORE_JUMP (jump, b, laststart);
keep_string_p = true;
}
else
STORE_JUMP (maybe_pop_jump, b, laststart - 3);
b += 3;
}
GET_BUFFER_SPACE (3);
INSERT_JUMP (keep_string_p ? on_failure_keep_string_jump
: on_failure_jump,
laststart, b + 3);
pending_exact = 0;
b += 3;
if (!zero_times_ok)
{
GET_BUFFER_SPACE (3);
INSERT_JUMP (dummy_failure_jump, laststart, laststart + 6);
b += 3;
}
}
break;
case '.':
laststart = b;
BUF_PUSH (anychar);
break;
case '[':
{
CLEAR_RANGE_TABLE_WORK_USED (range_table_work);
if (p == pend) FREE_STACK_RETURN (REG_EBRACK);
GET_BUFFER_SPACE (34);
laststart = b;
BUF_PUSH (*p == '^' ? charset_not : charset);
if (*p == '^')
p++;
p1 = p;
BUF_PUSH ((1 << BYTEWIDTH) / BYTEWIDTH);
bzero (b, (1 << BYTEWIDTH) / BYTEWIDTH);
if ((re_opcode_t) b[-2] == charset_not
&& (syntax & RE_HAT_LISTS_NOT_NEWLINE))
SET_LIST_BIT ('\n');
for (;;)
{
int len;
boolean escaped_char = false;
if (p == pend) FREE_STACK_RETURN (REG_EBRACK);
PATFETCH (c);
if ((syntax & RE_BACKSLASH_ESCAPE_IN_LISTS) && c == '\\')
{
if (p == pend) FREE_STACK_RETURN (REG_EESCAPE);
PATFETCH (c);
escaped_char = true;
}
else
{
if (c == ']' && p != p1 + 1)
break;
}
if (bufp->multibyte && BASE_LEADING_CODE_P (c))
{
PATUNFETCH;
c = STRING_CHAR_AND_LENGTH (p, pend - p, len);
p += len;
}
else if (!escaped_char &&
syntax & RE_CHAR_CLASSES && c == '[' && *p == ':')
{
char str[CHAR_CLASS_MAX_LENGTH + 1];
PATFETCH (c);
c1 = 0;
if (p == pend) FREE_STACK_RETURN (REG_EBRACK);
for (;;)
{
PATFETCH (c);
if (c == ':' || c == ']' || p == pend
|| c1 == CHAR_CLASS_MAX_LENGTH)
break;
str[c1++] = c;
}
str[c1] = '\0';
if (c == ':' && *p == ']')
{
int ch;
boolean is_alnum = STREQ (str, "alnum");
boolean is_alpha = STREQ (str, "alpha");
boolean is_blank = STREQ (str, "blank");
boolean is_cntrl = STREQ (str, "cntrl");
boolean is_digit = STREQ (str, "digit");
boolean is_graph = STREQ (str, "graph");
boolean is_lower = STREQ (str, "lower");
boolean is_print = STREQ (str, "print");
boolean is_punct = STREQ (str, "punct");
boolean is_space = STREQ (str, "space");
boolean is_upper = STREQ (str, "upper");
boolean is_xdigit = STREQ (str, "xdigit");
if (!IS_CHAR_CLASS (str))
FREE_STACK_RETURN (REG_ECTYPE);
PATFETCH (c);
if (p == pend) FREE_STACK_RETURN (REG_EBRACK);
for (ch = 0; ch < 1 << BYTEWIDTH; ch++)
{
int translated = TRANSLATE (ch);
if ( (is_alnum && ISALNUM (ch))
|| (is_alpha && ISALPHA (ch))
|| (is_blank && ISBLANK (ch))
|| (is_cntrl && ISCNTRL (ch)))
SET_LIST_BIT (translated);
if ( (is_digit && ISDIGIT (ch))
|| (is_graph && ISGRAPH (ch))
|| (is_lower && ISLOWER (ch))
|| (is_print && ISPRINT (ch)))
SET_LIST_BIT (translated);
if ( (is_punct && ISPUNCT (ch))
|| (is_space && ISSPACE (ch))
|| (is_upper && ISUPPER (ch))
|| (is_xdigit && ISXDIGIT (ch)))
SET_LIST_BIT (translated);
}
continue;
}
else
{
c1++;
while (c1--)
PATUNFETCH;
SET_LIST_BIT ('[');
c = ':';
}
}
if (p < pend && p[0] == '-' && p[1] != ']')
{
PATFETCH (c1);
PATFETCH (c1);
if (bufp->multibyte && BASE_LEADING_CODE_P (c1))
{
PATUNFETCH;
c1 = STRING_CHAR_AND_LENGTH (p, pend - p, len);
p += len;
}
if (SINGLE_BYTE_CHAR_P (c)
&& ! SINGLE_BYTE_CHAR_P (c1))
{
int c1_base = (c1 & ~0177) | 040;
SET_RANGE_TABLE_WORK_AREA (range_table_work, c, c1);
c1 = 0237;
}
else if (!SAME_CHARSET_P (c, c1))
FREE_STACK_RETURN (REG_ERANGE);
}
else
c1 = c;
if (SINGLE_BYTE_CHAR_P (c))
{
unsigned this_char;
int range_start = c, range_end = c1;
if (range_start > range_end)
{
if (syntax & RE_NO_EMPTY_RANGES)
FREE_STACK_RETURN (REG_ERANGE);
}
else
{
for (this_char = range_start; this_char <= range_end;
this_char++)
SET_LIST_BIT (TRANSLATE (this_char));
}
}
else
SET_RANGE_TABLE_WORK_AREA (range_table_work, c, c1);
}
while ((int) b[-1] > 0 && b[b[-1] - 1] == 0)
b[-1]--;
b += b[-1];
if (RANGE_TABLE_WORK_USED (range_table_work))
{
int i;
int used = RANGE_TABLE_WORK_USED (range_table_work);
GET_BUFFER_SPACE (2 + used * 3);
laststart[1] |= 0x80;
STORE_NUMBER_AND_INCR (b, used / 2);
for (i = 0; i < used; i++)
STORE_CHARACTER_AND_INCR
(b, RANGE_TABLE_WORK_ELT (range_table_work, i));
}
}
break;
case '(':
if (syntax & RE_NO_BK_PARENS)
goto handle_open;
else
goto normal_char;
case ')':
if (syntax & RE_NO_BK_PARENS)
goto handle_close;
else
goto normal_char;
case '\n':
if (syntax & RE_NEWLINE_ALT)
goto handle_alt;
else
goto normal_char;
case '|':
if (syntax & RE_NO_BK_VBAR)
goto handle_alt;
else
goto normal_char;
case '{':
if (syntax & RE_INTERVALS && syntax & RE_NO_BK_BRACES)
goto handle_interval;
else
goto normal_char;
case '\\':
if (p == pend) FREE_STACK_RETURN (REG_EESCAPE);
PATFETCH_RAW (c);
switch (c)
{
case '(':
if (syntax & RE_NO_BK_PARENS)
goto normal_backslash;
handle_open:
bufp->re_nsub++;
regnum++;
if (COMPILE_STACK_FULL)
{
RETALLOC (compile_stack.stack, compile_stack.size << 1,
compile_stack_elt_t);
if (compile_stack.stack == NULL) return REG_ESPACE;
compile_stack.size <<= 1;
}
COMPILE_STACK_TOP.begalt_offset = begalt - bufp->buffer;
COMPILE_STACK_TOP.fixup_alt_jump
= fixup_alt_jump ? fixup_alt_jump - bufp->buffer + 1 : 0;
COMPILE_STACK_TOP.laststart_offset = b - bufp->buffer;
COMPILE_STACK_TOP.regnum = regnum;
if (regnum <= MAX_REGNUM)
{
COMPILE_STACK_TOP.inner_group_offset = b - bufp->buffer + 2;
BUF_PUSH_3 (start_memory, regnum, 0);
}
compile_stack.avail++;
fixup_alt_jump = 0;
laststart = 0;
begalt = b;
pending_exact = 0;
break;
case ')':
if (syntax & RE_NO_BK_PARENS) goto normal_backslash;
if (COMPILE_STACK_EMPTY)
if (syntax & RE_UNMATCHED_RIGHT_PAREN_ORD)
goto normal_backslash;
else
FREE_STACK_RETURN (REG_ERPAREN);
handle_close:
if (fixup_alt_jump)
{
BUF_PUSH (push_dummy_failure);
STORE_JUMP (jump_past_alt, fixup_alt_jump, b - 1);
}
if (COMPILE_STACK_EMPTY)
if (syntax & RE_UNMATCHED_RIGHT_PAREN_ORD)
goto normal_char;
else
FREE_STACK_RETURN (REG_ERPAREN);
assert (compile_stack.avail != 0);
{
regnum_t this_group_regnum;
compile_stack.avail--;
begalt = bufp->buffer + COMPILE_STACK_TOP.begalt_offset;
fixup_alt_jump
= COMPILE_STACK_TOP.fixup_alt_jump
? bufp->buffer + COMPILE_STACK_TOP.fixup_alt_jump - 1
: 0;
laststart = bufp->buffer + COMPILE_STACK_TOP.laststart_offset;
this_group_regnum = COMPILE_STACK_TOP.regnum;
pending_exact = 0;
if (this_group_regnum <= MAX_REGNUM)
{
unsigned char *inner_group_loc
= bufp->buffer + COMPILE_STACK_TOP.inner_group_offset;
*inner_group_loc = regnum - this_group_regnum;
BUF_PUSH_3 (stop_memory, this_group_regnum,
regnum - this_group_regnum);
}
}
break;
case '|':
if (syntax & RE_LIMITED_OPS || syntax & RE_NO_BK_VBAR)
goto normal_backslash;
handle_alt:
if (syntax & RE_LIMITED_OPS)
goto normal_char;
GET_BUFFER_SPACE (3);
INSERT_JUMP (on_failure_jump, begalt, b + 6);
pending_exact = 0;
b += 3;
if (fixup_alt_jump)
STORE_JUMP (jump_past_alt, fixup_alt_jump, b);
fixup_alt_jump = b;
GET_BUFFER_SPACE (3);
b += 3;
laststart = 0;
begalt = b;
break;
case '{':
if (!(syntax & RE_INTERVALS)
|| ((syntax & RE_INTERVALS) && (syntax & RE_NO_BK_BRACES))
|| (p - 2 == pattern && p == pend))
goto normal_backslash;
handle_interval:
{
int lower_bound = -1, upper_bound = -1;
beg_interval = p - 1;
if (p == pend)
{
if (syntax & RE_NO_BK_BRACES)
goto unfetch_interval;
else
FREE_STACK_RETURN (REG_EBRACE);
}
GET_UNSIGNED_NUMBER (lower_bound);
if (c == ',')
{
GET_UNSIGNED_NUMBER (upper_bound);
if (upper_bound < 0) upper_bound = RE_DUP_MAX;
}
else
upper_bound = lower_bound;
if (lower_bound < 0 || upper_bound > RE_DUP_MAX
|| lower_bound > upper_bound)
{
if (syntax & RE_NO_BK_BRACES)
goto unfetch_interval;
else
FREE_STACK_RETURN (REG_BADBR);
}
if (!(syntax & RE_NO_BK_BRACES))
{
if (c != '\\') FREE_STACK_RETURN (REG_EBRACE);
PATFETCH (c);
}
if (c != '}')
{
if (syntax & RE_NO_BK_BRACES)
goto unfetch_interval;
else
FREE_STACK_RETURN (REG_BADBR);
}
if (!laststart)
{
if (syntax & RE_CONTEXT_INVALID_OPS)
FREE_STACK_RETURN (REG_BADRPT);
else if (syntax & RE_CONTEXT_INDEP_OPS)
laststart = b;
else
goto unfetch_interval;
}
if (upper_bound == 0)
{
GET_BUFFER_SPACE (3);
INSERT_JUMP (jump, laststart, b + 3);
b += 3;
}
else
{
unsigned nbytes = 10 + (upper_bound > 1) * 10;
GET_BUFFER_SPACE (nbytes);
INSERT_JUMP2 (succeed_n, laststart,
b + 5 + (upper_bound > 1) * 5,
lower_bound);
b += 5;
insert_op2 (set_number_at, laststart, 5, lower_bound, b);
b += 5;
if (upper_bound > 1)
{
STORE_JUMP2 (jump_n, b, laststart + 5,
upper_bound - 1);
b += 5;
insert_op2 (set_number_at, laststart, b - laststart,
upper_bound - 1, b);
b += 5;
}
}
pending_exact = 0;
beg_interval = NULL;
}
break;
unfetch_interval:
assert (beg_interval);
p = beg_interval;
beg_interval = NULL;
PATFETCH (c);
if (!(syntax & RE_NO_BK_BRACES))
{
if (p > pattern && p[-1] == '\\')
goto normal_backslash;
}
goto normal_char;
#ifdef emacs
case '=':
BUF_PUSH (at_dot);
break;
case 's':
laststart = b;
PATFETCH (c);
BUF_PUSH_2 (syntaxspec, syntax_spec_code[c]);
break;
case 'S':
laststart = b;
PATFETCH (c);
BUF_PUSH_2 (notsyntaxspec, syntax_spec_code[c]);
break;
case 'c':
laststart = b;
PATFETCH_RAW (c);
BUF_PUSH_2 (categoryspec, c);
break;
case 'C':
laststart = b;
PATFETCH_RAW (c);
BUF_PUSH_2 (notcategoryspec, c);
break;
#endif
case 'w':
laststart = b;
BUF_PUSH (wordchar);
break;
case 'W':
laststart = b;
BUF_PUSH (notwordchar);
break;
case '<':
BUF_PUSH (wordbeg);
break;
case '>':
BUF_PUSH (wordend);
break;
case 'b':
BUF_PUSH (wordbound);
break;
case 'B':
BUF_PUSH (notwordbound);
break;
case '`':
BUF_PUSH (begbuf);
break;
case '\'':
BUF_PUSH (endbuf);
break;
case '1': case '2': case '3': case '4': case '5':
case '6': case '7': case '8': case '9':
if (syntax & RE_NO_BK_REFS)
goto normal_char;
c1 = c - '0';
if (c1 > regnum)
FREE_STACK_RETURN (REG_ESUBREG);
if (group_in_compile_stack (compile_stack, c1))
goto normal_char;
laststart = b;
BUF_PUSH_2 (duplicate, c1);
break;
case '+':
case '?':
if (syntax & RE_BK_PLUS_QM)
goto handle_plus;
else
goto normal_backslash;
default:
normal_backslash:
c = TRANSLATE (c);
goto normal_char;
}
break;
default:
normal_char:
p1 = p - 1;
#ifdef emacs
if (bufp->multibyte)
{
c = STRING_CHAR (p1, pend - p1);
c = TRANSLATE (c);
p += MULTIBYTE_FORM_LENGTH (p1, pend - p1) - 1;
}
#endif
if (!pending_exact
|| pending_exact + *pending_exact + 1 != b
|| *pending_exact >= (1 << BYTEWIDTH) - (p - p1)
|| (p != pend && (*p == '*' || *p == '^'))
|| ((syntax & RE_BK_PLUS_QM)
? p + 1 < pend && *p == '\\' && (p[1] == '+' || p[1] == '?')
: p != pend && (*p == '+' || *p == '?'))
|| ((syntax & RE_INTERVALS)
&& ((syntax & RE_NO_BK_BRACES)
? p != pend && *p == '{'
: p + 1 < pend && p[0] == '\\' && p[1] == '{')))
{
laststart = b;
BUF_PUSH_2 (exactn, 0);
pending_exact = b - 1;
}
#ifdef emacs
if (! SINGLE_BYTE_CHAR_P (c))
{
unsigned char work[4], *str;
int i = CHAR_STRING (c, work, str);
int j;
for (j = 0; j < i; j++)
{
BUF_PUSH (str[j]);
(*pending_exact)++;
}
}
else
#endif
{
BUF_PUSH (c);
(*pending_exact)++;
}
break;
}
}
if (fixup_alt_jump)
STORE_JUMP (jump_past_alt, fixup_alt_jump, b);
if (!COMPILE_STACK_EMPTY)
FREE_STACK_RETURN (REG_EPAREN);
if (syntax & RE_NO_POSIX_BACKTRACKING)
BUF_PUSH (succeed);
free (compile_stack.stack);
bufp->used = b - bufp->buffer;
#ifdef DEBUG
if (debug)
{
DEBUG_PRINT1 ("\nCompiled pattern: \n");
print_compiled_pattern (bufp);
}
#endif
#ifndef MATCH_MAY_ALLOCATE
{
int num_regs = bufp->re_nsub + 1;
if (fail_stack.size < re_max_failures * TYPICAL_FAILURE_SIZE)
{
fail_stack.size = re_max_failures * TYPICAL_FAILURE_SIZE;
#ifdef emacs
if (! fail_stack.stack)
fail_stack.stack
= (fail_stack_elt_t *) xmalloc (fail_stack.size
* sizeof (fail_stack_elt_t));
else
fail_stack.stack
= (fail_stack_elt_t *) xrealloc (fail_stack.stack,
(fail_stack.size
* sizeof (fail_stack_elt_t)));
#else
if (! fail_stack.stack)
fail_stack.stack
= (fail_stack_elt_t *) malloc (fail_stack.size
* sizeof (fail_stack_elt_t));
else
fail_stack.stack
= (fail_stack_elt_t *) realloc (fail_stack.stack,
(fail_stack.size
* sizeof (fail_stack_elt_t)));
#endif
}
regex_grow_registers (num_regs);
}
#endif
return REG_NOERROR;
}
static void
store_op1 (op, loc, arg)
re_opcode_t op;
unsigned char *loc;
int arg;
{
*loc = (unsigned char) op;
STORE_NUMBER (loc + 1, arg);
}
static void
store_op2 (op, loc, arg1, arg2)
re_opcode_t op;
unsigned char *loc;
int arg1, arg2;
{
*loc = (unsigned char) op;
STORE_NUMBER (loc + 1, arg1);
STORE_NUMBER (loc + 3, arg2);
}
static void
insert_op1 (op, loc, arg, end)
re_opcode_t op;
unsigned char *loc;
int arg;
unsigned char *end;
{
register unsigned char *pfrom = end;
register unsigned char *pto = end + 3;
while (pfrom != loc)
*--pto = *--pfrom;
store_op1 (op, loc, arg);
}
static void
insert_op2 (op, loc, arg1, arg2, end)
re_opcode_t op;
unsigned char *loc;
int arg1, arg2;
unsigned char *end;
{
register unsigned char *pfrom = end;
register unsigned char *pto = end + 5;
while (pfrom != loc)
*--pto = *--pfrom;
store_op2 (op, loc, arg1, arg2);
}
static boolean
at_begline_loc_p (pattern, p, syntax)
const char *pattern, *p;
reg_syntax_t syntax;
{
const char *prev = p - 2;
boolean prev_prev_backslash = prev > pattern && prev[-1] == '\\';
return
(*prev == '(' && (syntax & RE_NO_BK_PARENS || prev_prev_backslash))
|| (*prev == '|' && (syntax & RE_NO_BK_VBAR || prev_prev_backslash));
}
static boolean
at_endline_loc_p (p, pend, syntax)
const char *p, *pend;
int syntax;
{
const char *next = p;
boolean next_backslash = *next == '\\';
const char *next_next = p + 1 < pend ? p + 1 : 0;
return
(syntax & RE_NO_BK_PARENS ? *next == ')'
: next_backslash && next_next && *next_next == ')')
|| (syntax & RE_NO_BK_VBAR ? *next == '|'
: next_backslash && next_next && *next_next == '|');
}
static boolean
group_in_compile_stack (compile_stack, regnum)
compile_stack_type compile_stack;
regnum_t regnum;
{
int this_element;
for (this_element = compile_stack.avail - 1;
this_element >= 0;
this_element--)
if (compile_stack.stack[this_element].regnum == regnum)
return true;
return false;
}
int
re_compile_fastmap (bufp)
struct re_pattern_buffer *bufp;
{
int i, j, k;
#ifdef MATCH_MAY_ALLOCATE
fail_stack_type fail_stack;
#endif
#ifndef REGEX_MALLOC
char *destination;
#endif
unsigned num_regs = 0;
register char *fastmap = bufp->fastmap;
unsigned char *pattern = bufp->buffer;
unsigned long size = bufp->used;
unsigned char *p = pattern;
register unsigned char *pend = pattern + size;
fail_stack_elt_t *failure_stack_ptr;
boolean path_can_be_null = true;
boolean succeed_n_p = false;
boolean match_any_multibyte_characters = false;
int simple_char_max;
assert (fastmap != NULL && p != NULL);
INIT_FAIL_STACK ();
bzero (fastmap, 1 << BYTEWIDTH);
bufp->fastmap_accurate = 1;
bufp->can_be_null = 0;
while (1)
{
if (p == pend || *p == succeed)
{
if (!FAIL_STACK_EMPTY ())
{
bufp->can_be_null |= path_can_be_null;
path_can_be_null = true;
p = fail_stack.stack[--fail_stack.avail].pointer;
continue;
}
else
break;
}
assert (p < pend);
switch (SWITCH_ENUM_CAST ((re_opcode_t) *p++))
{
case duplicate:
bufp->can_be_null = 1;
goto done;
case exactn:
fastmap[p[1]] = 1;
break;
#ifndef emacs
case charset:
for (j = *p++ * BYTEWIDTH - 1; j >= 0; j--)
if (p[j / BYTEWIDTH] & (1 << (j % BYTEWIDTH)))
fastmap[j] = 1;
break;
case charset_not:
for (j = *p * BYTEWIDTH; j < (1 << BYTEWIDTH); j++)
fastmap[j] = 1;
for (j = *p++ * BYTEWIDTH - 1; j >= 0; j--)
if (!(p[j / BYTEWIDTH] & (1 << (j % BYTEWIDTH))))
fastmap[j] = 1;
break;
case wordchar:
for (j = 0; j < (1 << BYTEWIDTH); j++)
if (SYNTAX (j) == Sword)
fastmap[j] = 1;
break;
case notwordchar:
for (j = 0; j < (1 << BYTEWIDTH); j++)
if (SYNTAX (j) != Sword)
fastmap[j] = 1;
break;
#else
case charset:
for (j = CHARSET_BITMAP_SIZE (&p[-1]) * BYTEWIDTH - 1, p++;
j >= 0; j--)
if (p[j / BYTEWIDTH] & (1 << (j % BYTEWIDTH)))
fastmap[j] = 1;
if (CHARSET_RANGE_TABLE_EXISTS_P (&p[-2])
&& match_any_multibyte_characters == false)
{
int c, count;
p += CHARSET_BITMAP_SIZE (&p[-2]);
EXTRACT_NUMBER_AND_INCR (count, p);
for (; count > 0; count--, p += 2 * 3)
{
EXTRACT_CHARACTER (c, p);
j = CHAR_CHARSET (c);
fastmap[CHARSET_LEADING_CODE_BASE (j)] = 1;
}
}
break;
case charset_not:
simple_char_max = (1 << BYTEWIDTH);
for (j = CHARSET_BITMAP_SIZE (&p[-1]) * BYTEWIDTH;
j < simple_char_max; j++)
fastmap[j] = 1;
for (j = CHARSET_BITMAP_SIZE (&p[-1]) * BYTEWIDTH - 1, p++;
j >= 0; j--)
if (!(p[j / BYTEWIDTH] & (1 << (j % BYTEWIDTH))))
fastmap[j] = 1;
if (bufp->multibyte)
{
set_fastmap_for_multibyte_characters:
if (match_any_multibyte_characters == false)
{
for (j = 0x80; j < 0xA0; j++)
if (BASE_LEADING_CODE_P (j))
fastmap[j] = 1;
match_any_multibyte_characters = true;
}
}
break;
case wordchar:
simple_char_max = (1 << BYTEWIDTH);
for (j = 0; j < simple_char_max; j++)
if (SYNTAX (j) == Sword)
fastmap[j] = 1;
if (bufp->multibyte)
goto set_fastmap_for_multibyte_characters;
break;
case notwordchar:
simple_char_max = (1 << BYTEWIDTH);
for (j = 0; j < simple_char_max; j++)
if (SYNTAX (j) != Sword)
fastmap[j] = 1;
if (bufp->multibyte)
goto set_fastmap_for_multibyte_characters;
break;
#endif
case anychar:
{
int fastmap_newline = fastmap['\n'];
if (bufp->multibyte)
match_any_multibyte_characters = true;
simple_char_max = (1 << BYTEWIDTH);
for (j = 0; j < simple_char_max; j++)
fastmap[j] = 1;
if (!(bufp->syntax & RE_DOT_NEWLINE))
fastmap['\n'] = fastmap_newline;
else if (bufp->can_be_null)
goto done;
break;
}
#ifdef emacs
case wordbound:
case notwordbound:
case wordbeg:
case wordend:
case notsyntaxspec:
case syntaxspec:
bufp->can_be_null = 1;
goto done;
#if 0
k = *p++;
simple_char_max = bufp->multibyte ? 0x80 : (1 << BYTEWIDTH);
for (j = 0; j < simple_char_max; j++)
if (SYNTAX (j) == (enum syntaxcode) k)
fastmap[j] = 1;
if (bufp->multibyte)
goto set_fastmap_for_multibyte_characters;
break;
case notsyntaxspec:
k = *p++;
simple_char_max = bufp->multibyte ? 0x80 : (1 << BYTEWIDTH);
for (j = 0; j < simple_char_max; j++)
if (SYNTAX (j) != (enum syntaxcode) k)
fastmap[j] = 1;
if (bufp->multibyte)
goto set_fastmap_for_multibyte_characters;
break;
#endif
case categoryspec:
k = *p++;
simple_char_max = (1 << BYTEWIDTH);
for (j = 0; j < simple_char_max; j++)
if (CHAR_HAS_CATEGORY (j, k))
fastmap[j] = 1;
if (bufp->multibyte)
goto set_fastmap_for_multibyte_characters;
break;
case notcategoryspec:
k = *p++;
simple_char_max = (1 << BYTEWIDTH);
for (j = 0; j < simple_char_max; j++)
if (!CHAR_HAS_CATEGORY (j, k))
fastmap[j] = 1;
if (bufp->multibyte)
goto set_fastmap_for_multibyte_characters;
break;
case before_dot:
case at_dot:
case after_dot:
continue;
#endif
case no_op:
case begline:
case endline:
case begbuf:
case endbuf:
#ifndef emacs
case wordbound:
case notwordbound:
case wordbeg:
case wordend:
#endif
case push_dummy_failure:
continue;
case jump_n:
case pop_failure_jump:
case maybe_pop_jump:
case jump:
case jump_past_alt:
case dummy_failure_jump:
EXTRACT_NUMBER_AND_INCR (j, p);
p += j;
if (j > 0)
continue;
if ((re_opcode_t) *p != on_failure_jump
&& (re_opcode_t) *p != succeed_n)
continue;
p++;
EXTRACT_NUMBER_AND_INCR (j, p);
p += j;
if (!FAIL_STACK_EMPTY ()
&& fail_stack.stack[fail_stack.avail - 1].pointer == p)
fail_stack.avail--;
continue;
case on_failure_jump:
case on_failure_keep_string_jump:
handle_on_failure_jump:
EXTRACT_NUMBER_AND_INCR (j, p);
if (p + j < pend)
{
if (!PUSH_PATTERN_OP (p + j, fail_stack))
{
RESET_FAIL_STACK ();
return -2;
}
}
else
bufp->can_be_null = 1;
if (succeed_n_p)
{
EXTRACT_NUMBER_AND_INCR (k, p);
succeed_n_p = false;
}
continue;
case succeed_n:
p += 2;
EXTRACT_NUMBER_AND_INCR (k, p);
if (k == 0)
{
p -= 4;
succeed_n_p = true;
goto handle_on_failure_jump;
}
continue;
case set_number_at:
p += 4;
continue;
case start_memory:
case stop_memory:
p += 2;
continue;
default:
abort ();
}
path_can_be_null = false;
p = pend;
}
bufp->can_be_null |= path_can_be_null;
done:
RESET_FAIL_STACK ();
return 0;
}
void
re_set_registers (bufp, regs, num_regs, starts, ends)
struct re_pattern_buffer *bufp;
struct re_registers *regs;
unsigned num_regs;
regoff_t *starts, *ends;
{
if (num_regs)
{
bufp->regs_allocated = REGS_REALLOCATE;
regs->num_regs = num_regs;
regs->start = starts;
regs->end = ends;
}
else
{
bufp->regs_allocated = REGS_UNALLOCATED;
regs->num_regs = 0;
regs->start = regs->end = (regoff_t *) 0;
}
}
int
re_search (bufp, string, size, startpos, range, regs)
struct re_pattern_buffer *bufp;
const char *string;
int size, startpos, range;
struct re_registers *regs;
{
return re_search_2 (bufp, NULL, 0, string, size, startpos, range,
regs, size);
}
#define STOP_ADDR_VSTRING(P) \
(((P) >= size1 ? string2 + size2 : string1 + size1))
#define POS_ADDR_VSTRING(POS) \
(((POS) >= size1 ? string2 - size1 : string1) + (POS))
int
re_search_2 (bufp, string1, size1, string2, size2, startpos, range, regs, stop)
struct re_pattern_buffer *bufp;
const char *string1, *string2;
int size1, size2;
int startpos;
int range;
struct re_registers *regs;
int stop;
{
int val;
register char *fastmap = bufp->fastmap;
register RE_TRANSLATE_TYPE translate = bufp->translate;
int total_size = size1 + size2;
int endpos = startpos + range;
int anchored_start = 0;
int multibyte = bufp->multibyte;
if (startpos < 0 || startpos > total_size)
return -1;
if (endpos < 0)
range = 0 - startpos;
else if (endpos > total_size)
range = total_size - startpos;
if (bufp->used > 0 && (re_opcode_t) bufp->buffer[0] == begbuf && range > 0)
{
if (startpos > 0)
return -1;
else
range = 0;
}
#ifdef emacs
if (bufp->used > 0 && (re_opcode_t) bufp->buffer[0] == at_dot && range > 0)
{
range = PT_BYTE - BEGV_BYTE - startpos;
if (range < 0)
return -1;
}
#endif
if (fastmap && !bufp->fastmap_accurate)
if (re_compile_fastmap (bufp) == -2)
return -2;
if (bufp->buffer[0] == begline)
anchored_start = 1;
#ifdef emacs
gl_state.object = re_match_object;
{
int adjpos = NILP (re_match_object) || BUFFERP (re_match_object);
int charpos = SYNTAX_TABLE_BYTE_TO_CHAR (startpos + adjpos);
SETUP_SYNTAX_TABLE_FOR_OBJECT (re_match_object, charpos, 1);
}
#endif
for (;;)
{
if (anchored_start && startpos > 0)
{
if (! (bufp->newline_anchor
&& ((startpos <= size1 ? string1[startpos - 1]
: string2[startpos - size1 - 1])
== '\n')))
goto advance;
}
if (fastmap && startpos < total_size && !bufp->can_be_null)
{
register const char *d;
register unsigned int buf_ch;
d = POS_ADDR_VSTRING (startpos);
if (range > 0)
{
register int lim = 0;
int irange = range;
if (startpos < size1 && startpos + range >= size1)
lim = range - (size1 - startpos);
if (RE_TRANSLATE_P (translate))
{
if (multibyte)
while (range > lim)
{
int buf_charlen;
buf_ch = STRING_CHAR_AND_LENGTH (d, range - lim,
buf_charlen);
buf_ch = RE_TRANSLATE (translate, buf_ch);
if (buf_ch >= 0400
|| fastmap[buf_ch])
break;
range -= buf_charlen;
d += buf_charlen;
}
else
while (range > lim
&& !fastmap[(unsigned char)
RE_TRANSLATE (translate, (unsigned char) *d)])
{
d++;
range--;
}
}
else
while (range > lim && !fastmap[(unsigned char) *d])
{
d++;
range--;
}
startpos += irange - range;
}
else
{
int room = (size1 == 0 || startpos >= size1
? size2 + size1 - startpos
: size1 - startpos);
buf_ch = STRING_CHAR (d, room);
if (RE_TRANSLATE_P (translate))
buf_ch = RE_TRANSLATE (translate, buf_ch);
if (! (buf_ch >= 0400
|| fastmap[buf_ch]))
goto advance;
}
}
if (range >= 0 && startpos == total_size && fastmap
&& !bufp->can_be_null)
return -1;
val = re_match_2_internal (bufp, string1, size1, string2, size2,
startpos, regs, stop);
#ifndef REGEX_MALLOC
#ifdef C_ALLOCA
alloca (0);
#endif
#endif
if (val >= 0)
return startpos;
if (val == -2)
return -2;
advance:
if (!range)
break;
else if (range > 0)
{
if (multibyte)
{
const unsigned char *p
= (const unsigned char *) POS_ADDR_VSTRING (startpos);
const unsigned char *pend
= (const unsigned char *) STOP_ADDR_VSTRING (startpos);
int len = MULTIBYTE_FORM_LENGTH (p, pend - p);
range -= len;
if (range < 0)
break;
startpos += len;
}
else
{
range--;
startpos++;
}
}
else
{
range++;
startpos--;
if (multibyte)
{
const unsigned char *p
= (const unsigned char *) POS_ADDR_VSTRING (startpos);
int len = 0;
while (!CHAR_HEAD_P (*p))
p--, len++;
#if 0
if (MULTIBYTE_FORM_LENGTH (p, len + 1) != (len + 1))
;
else
#endif
{
range += len;
if (range > 0)
break;
startpos -= len;
}
}
}
}
return -1;
}
static int bcmp_translate ();
static boolean alt_match_null_string_p (),
common_op_match_null_string_p (),
group_match_null_string_p ();
#define POINTER_TO_OFFSET(ptr) \
(FIRST_STRING_P (ptr) \
? ((regoff_t) ((ptr) - string1)) \
: ((regoff_t) ((ptr) - string2 + size1)))
#define MATCHING_IN_FIRST_STRING (dend == end_match_1)
#define PREFETCH() \
while (d == dend) \
{ \
\
if (dend == end_match_2) \
goto fail; \
\
d = string2; \
dend = end_match_2; \
}
#define AT_STRINGS_BEG(d) ((d) == (size1 ? string1 : string2) || !size2)
#define AT_STRINGS_END(d) ((d) == end2)
#define WORDCHAR_P(d) \
(SYNTAX ((d) == end1 ? *string2 \
: (d) == string2 - 1 ? *(end1 - 1) : *(d)) \
== Sword)
#if 0
#define AT_WORD_BOUNDARY(d) \
(AT_STRINGS_BEG (d) || AT_STRINGS_END (d) \
|| WORDCHAR_P (d - 1) != WORDCHAR_P (d))
#endif
#ifdef MATCH_MAY_ALLOCATE
#define FREE_VAR(var) if (var) { REGEX_FREE (var); var = NULL; } else
#define FREE_VARIABLES() \
do { \
REGEX_FREE_STACK (fail_stack.stack); \
FREE_VAR (regstart); \
FREE_VAR (regend); \
FREE_VAR (old_regstart); \
FREE_VAR (old_regend); \
FREE_VAR (best_regstart); \
FREE_VAR (best_regend); \
FREE_VAR (reg_info); \
FREE_VAR (reg_dummy); \
FREE_VAR (reg_info_dummy); \
} while (0)
#else
#define FREE_VARIABLES() ((void)0)
#endif
#define NO_HIGHEST_ACTIVE_REG (1 << BYTEWIDTH)
#define NO_LOWEST_ACTIVE_REG (NO_HIGHEST_ACTIVE_REG + 1)
#ifndef emacs
int
re_match (bufp, string, size, pos, regs)
struct re_pattern_buffer *bufp;
const char *string;
int size, pos;
struct re_registers *regs;
{
int result = re_match_2_internal (bufp, NULL, 0, string, size,
pos, regs, size);
#ifndef REGEX_MALLOC
#ifdef C_ALLOCA
alloca (0);
#endif
#endif
return result;
}
#endif
#ifdef emacs
Lisp_Object re_match_object;
#endif
int
re_match_2 (bufp, string1, size1, string2, size2, pos, regs, stop)
struct re_pattern_buffer *bufp;
const char *string1, *string2;
int size1, size2;
int pos;
struct re_registers *regs;
int stop;
{
int result;
#ifdef emacs
int charpos;
int adjpos = NILP (re_match_object) || BUFFERP (re_match_object);
gl_state.object = re_match_object;
charpos = SYNTAX_TABLE_BYTE_TO_CHAR (pos + adjpos);
SETUP_SYNTAX_TABLE_FOR_OBJECT (re_match_object, charpos, 1);
#endif
result = re_match_2_internal (bufp, string1, size1, string2, size2,
pos, regs, stop);
#ifndef REGEX_MALLOC
#ifdef C_ALLOCA
alloca (0);
#endif
#endif
return result;
}
static int
re_match_2_internal (bufp, string1, size1, string2, size2, pos, regs, stop)
struct re_pattern_buffer *bufp;
const char *string1, *string2;
int size1, size2;
int pos;
struct re_registers *regs;
int stop;
{
int mcnt;
unsigned char *p1;
const char *end1, *end2;
const char *end_match_1, *end_match_2;
const char *d, *dend;
unsigned char *p = bufp->buffer;
register unsigned char *pend = p + bufp->used;
unsigned char *just_past_start_mem = 0;
RE_TRANSLATE_TYPE translate = bufp->translate;
int multibyte = bufp->multibyte;
#ifdef MATCH_MAY_ALLOCATE
fail_stack_type fail_stack;
#endif
#ifdef DEBUG
static unsigned failure_id = 0;
unsigned nfailure_points_pushed = 0, nfailure_points_popped = 0;
#endif
fail_stack_elt_t *failure_stack_ptr;
unsigned num_regs = bufp->re_nsub + 1;
unsigned lowest_active_reg = NO_LOWEST_ACTIVE_REG;
unsigned highest_active_reg = NO_HIGHEST_ACTIVE_REG;
#ifdef MATCH_MAY_ALLOCATE
const char **regstart, **regend;
#endif
#ifdef MATCH_MAY_ALLOCATE
const char **old_regstart, **old_regend;
#endif
#ifdef MATCH_MAY_ALLOCATE
register_info_type *reg_info;
#endif
unsigned best_regs_set = false;
#ifdef MATCH_MAY_ALLOCATE
const char **best_regstart, **best_regend;
#endif
const char *match_end = NULL;
int set_regs_matched_done = 0;
#ifdef MATCH_MAY_ALLOCATE
const char **reg_dummy;
register_info_type *reg_info_dummy;
#endif
#ifdef DEBUG
unsigned num_regs_pushed = 0;
#endif
DEBUG_PRINT1 ("\n\nEntering re_match_2.\n");
INIT_FAIL_STACK ();
#ifdef MATCH_MAY_ALLOCATE
if (bufp->re_nsub)
{
regstart = REGEX_TALLOC (num_regs, const char *);
regend = REGEX_TALLOC (num_regs, const char *);
old_regstart = REGEX_TALLOC (num_regs, const char *);
old_regend = REGEX_TALLOC (num_regs, const char *);
best_regstart = REGEX_TALLOC (num_regs, const char *);
best_regend = REGEX_TALLOC (num_regs, const char *);
reg_info = REGEX_TALLOC (num_regs, register_info_type);
reg_dummy = REGEX_TALLOC (num_regs, const char *);
reg_info_dummy = REGEX_TALLOC (num_regs, register_info_type);
if (!(regstart && regend && old_regstart && old_regend && reg_info
&& best_regstart && best_regend && reg_dummy && reg_info_dummy))
{
FREE_VARIABLES ();
return -2;
}
}
else
{
regstart = regend = old_regstart = old_regend = best_regstart
= best_regend = reg_dummy = NULL;
reg_info = reg_info_dummy = (register_info_type *) NULL;
}
#endif
if (pos < 0 || pos > size1 + size2)
{
FREE_VARIABLES ();
return -1;
}
for (mcnt = 1; mcnt < num_regs; mcnt++)
{
regstart[mcnt] = regend[mcnt]
= old_regstart[mcnt] = old_regend[mcnt] = REG_UNSET_VALUE;
REG_MATCH_NULL_STRING_P (reg_info[mcnt]) = MATCH_NULL_UNSET_VALUE;
IS_ACTIVE (reg_info[mcnt]) = 0;
MATCHED_SOMETHING (reg_info[mcnt]) = 0;
EVER_MATCHED_SOMETHING (reg_info[mcnt]) = 0;
}
if (size2 == 0 && string1 != NULL)
{
string2 = string1;
size2 = size1;
string1 = 0;
size1 = 0;
}
end1 = string1 + size1;
end2 = string2 + size2;
if (stop <= size1)
{
end_match_1 = string1 + stop;
end_match_2 = string2;
}
else
{
end_match_1 = end1;
end_match_2 = string2 + stop - size1;
}
if (size1 > 0 && pos <= size1)
{
d = string1 + pos;
dend = end_match_1;
}
else
{
d = string2 + pos - size1;
dend = end_match_2;
}
DEBUG_PRINT1 ("The compiled pattern is: ");
DEBUG_PRINT_COMPILED_PATTERN (bufp, p, pend);
DEBUG_PRINT1 ("The string to match is: `");
DEBUG_PRINT_DOUBLE_STRING (d, string1, size1, string2, size2);
DEBUG_PRINT1 ("'\n");
for (;;)
{
DEBUG_PRINT2 ("\n0x%x: ", p);
if (p == pend)
{
DEBUG_PRINT1 ("end of pattern ... ");
if (d != end_match_2)
{
boolean same_str_p = (FIRST_STRING_P (match_end)
== MATCHING_IN_FIRST_STRING);
boolean best_match_p;
if (same_str_p)
best_match_p = d > match_end;
else
best_match_p = !MATCHING_IN_FIRST_STRING;
DEBUG_PRINT1 ("backtracking.\n");
if (!FAIL_STACK_EMPTY ())
{
if (!best_regs_set || best_match_p)
{
best_regs_set = true;
match_end = d;
DEBUG_PRINT1 ("\nSAVING match as best so far.\n");
for (mcnt = 1; mcnt < num_regs; mcnt++)
{
best_regstart[mcnt] = regstart[mcnt];
best_regend[mcnt] = regend[mcnt];
}
}
goto fail;
}
else if (best_regs_set && !best_match_p)
{
restore_best_regs:
DEBUG_PRINT1 ("Restoring best registers.\n");
d = match_end;
dend = ((d >= string1 && d <= end1)
? end_match_1 : end_match_2);
for (mcnt = 1; mcnt < num_regs; mcnt++)
{
regstart[mcnt] = best_regstart[mcnt];
regend[mcnt] = best_regend[mcnt];
}
}
}
succeed_label:
DEBUG_PRINT1 ("Accepting match.\n");
if (regs && !bufp->no_sub)
{
if (bufp->regs_allocated == REGS_UNALLOCATED)
{
regs->num_regs = MAX (RE_NREGS, num_regs + 1);
regs->start = TALLOC (regs->num_regs, regoff_t);
regs->end = TALLOC (regs->num_regs, regoff_t);
if (regs->start == NULL || regs->end == NULL)
{
FREE_VARIABLES ();
return -2;
}
bufp->regs_allocated = REGS_REALLOCATE;
}
else if (bufp->regs_allocated == REGS_REALLOCATE)
{
if (regs->num_regs < num_regs + 1)
{
regs->num_regs = num_regs + 1;
RETALLOC (regs->start, regs->num_regs, regoff_t);
RETALLOC (regs->end, regs->num_regs, regoff_t);
if (regs->start == NULL || regs->end == NULL)
{
FREE_VARIABLES ();
return -2;
}
}
}
else
{
assert (bufp->regs_allocated == REGS_FIXED);
}
if (regs->num_regs > 0)
{
regs->start[0] = pos;
regs->end[0] = (MATCHING_IN_FIRST_STRING
? ((regoff_t) (d - string1))
: ((regoff_t) (d - string2 + size1)));
}
for (mcnt = 1; mcnt < MIN (num_regs, regs->num_regs); mcnt++)
{
if (REG_UNSET (regstart[mcnt]) || REG_UNSET (regend[mcnt]))
regs->start[mcnt] = regs->end[mcnt] = -1;
else
{
regs->start[mcnt]
= (regoff_t) POINTER_TO_OFFSET (regstart[mcnt]);
regs->end[mcnt]
= (regoff_t) POINTER_TO_OFFSET (regend[mcnt]);
}
}
for (mcnt = num_regs; mcnt < regs->num_regs; mcnt++)
regs->start[mcnt] = regs->end[mcnt] = -1;
}
DEBUG_PRINT4 ("%u failure points pushed, %u popped (%u remain).\n",
nfailure_points_pushed, nfailure_points_popped,
nfailure_points_pushed - nfailure_points_popped);
DEBUG_PRINT2 ("%u registers pushed.\n", num_regs_pushed);
mcnt = d - pos - (MATCHING_IN_FIRST_STRING
? string1
: string2 - size1);
DEBUG_PRINT2 ("Returning %d from re_match_2.\n", mcnt);
FREE_VARIABLES ();
return mcnt;
}
switch (SWITCH_ENUM_CAST ((re_opcode_t) *p++))
{
case no_op:
DEBUG_PRINT1 ("EXECUTING no_op.\n");
break;
case succeed:
DEBUG_PRINT1 ("EXECUTING succeed.\n");
goto succeed_label;
case exactn:
mcnt = *p++;
DEBUG_PRINT2 ("EXECUTING exactn %d.\n", mcnt);
if (RE_TRANSLATE_P (translate))
{
#ifdef emacs
if (multibyte)
do
{
int pat_charlen, buf_charlen;
unsigned int pat_ch, buf_ch;
PREFETCH ();
pat_ch = STRING_CHAR_AND_LENGTH (p, pend - p, pat_charlen);
buf_ch = STRING_CHAR_AND_LENGTH (d, dend - d, buf_charlen);
if (RE_TRANSLATE (translate, buf_ch)
!= pat_ch)
goto fail;
p += pat_charlen;
d += buf_charlen;
mcnt -= pat_charlen;
}
while (mcnt > 0);
else
#endif
do
{
PREFETCH ();
if ((unsigned char) RE_TRANSLATE (translate, (unsigned char) *d)
!= (unsigned char) *p++)
goto fail;
d++;
}
while (--mcnt);
}
else
{
do
{
PREFETCH ();
if (*d++ != (char) *p++) goto fail;
}
while (--mcnt);
}
SET_REGS_MATCHED ();
break;
case anychar:
{
int buf_charlen;
unsigned int buf_ch;
DEBUG_PRINT1 ("EXECUTING anychar.\n");
PREFETCH ();
#ifdef emacs
if (multibyte)
buf_ch = STRING_CHAR_AND_LENGTH (d, dend - d, buf_charlen);
else
#endif
{
buf_ch = (unsigned char) *d;
buf_charlen = 1;
}
buf_ch = TRANSLATE (buf_ch);
if ((!(bufp->syntax & RE_DOT_NEWLINE)
&& buf_ch == '\n')
|| ((bufp->syntax & RE_DOT_NOT_NULL)
&& buf_ch == '\000'))
goto fail;
SET_REGS_MATCHED ();
DEBUG_PRINT2 ("  Matched `%d'.\n", *d);
d += buf_charlen;
}
break;
case charset:
case charset_not:
{
register unsigned int c;
boolean not = (re_opcode_t) *(p - 1) == charset_not;
int len;
unsigned char *range_table;
int range_table_exists;
int count;
DEBUG_PRINT2 ("EXECUTING charset%s.\n", not ? "_not" : "");
PREFETCH ();
c = (unsigned char) *d;
range_table = CHARSET_RANGE_TABLE (&p[-1]);
range_table_exists = CHARSET_RANGE_TABLE_EXISTS_P (&p[-1]);
if (range_table_exists)
EXTRACT_NUMBER_AND_INCR (count, range_table);
else
count = 0;
if (multibyte && BASE_LEADING_CODE_P (c))
c = STRING_CHAR_AND_LENGTH (d, dend - d, len);
if (SINGLE_BYTE_CHAR_P (c))
{
c = TRANSLATE (c);
len = 1;
if (c < (unsigned) (CHARSET_BITMAP_SIZE (&p[-1]) * BYTEWIDTH)
&& p[1 + c / BYTEWIDTH] & (1 << (c % BYTEWIDTH)))
not = !not;
}
else if (range_table_exists)
CHARSET_LOOKUP_RANGE_TABLE_RAW (not, c, range_table, count);
p = CHARSET_RANGE_TABLE_END (range_table, count);
if (!not) goto fail;
SET_REGS_MATCHED ();
d += len;
break;
}
case start_memory:
DEBUG_PRINT3 ("EXECUTING start_memory %d (%d):\n", *p, p[1]);
p1 = p;
if (REG_MATCH_NULL_STRING_P (reg_info[*p]) == MATCH_NULL_UNSET_VALUE)
REG_MATCH_NULL_STRING_P (reg_info[*p])
= group_match_null_string_p (&p1, pend, reg_info);
old_regstart[*p] = REG_MATCH_NULL_STRING_P (reg_info[*p])
? REG_UNSET (regstart[*p]) ? d : regstart[*p]
: regstart[*p];
DEBUG_PRINT2 ("  old_regstart: %d\n",
POINTER_TO_OFFSET (old_regstart[*p]));
regstart[*p] = d;
DEBUG_PRINT2 ("  regstart: %d\n", POINTER_TO_OFFSET (regstart[*p]));
IS_ACTIVE (reg_info[*p]) = 1;
MATCHED_SOMETHING (reg_info[*p]) = 0;
set_regs_matched_done = 0;
highest_active_reg = *p;
if (lowest_active_reg == NO_LOWEST_ACTIVE_REG)
lowest_active_reg = *p;
p += 2;
just_past_start_mem = p;
break;
case stop_memory:
DEBUG_PRINT3 ("EXECUTING stop_memory %d (%d):\n", *p, p[1]);
old_regend[*p] = REG_MATCH_NULL_STRING_P (reg_info[*p])
? REG_UNSET (regend[*p]) ? d : regend[*p]
: regend[*p];
DEBUG_PRINT2 ("      old_regend: %d\n",
POINTER_TO_OFFSET (old_regend[*p]));
regend[*p] = d;
DEBUG_PRINT2 ("      regend: %d\n", POINTER_TO_OFFSET (regend[*p]));
IS_ACTIVE (reg_info[*p]) = 0;
set_regs_matched_done = 0;
if (lowest_active_reg == highest_active_reg)
{
lowest_active_reg = NO_LOWEST_ACTIVE_REG;
highest_active_reg = NO_HIGHEST_ACTIVE_REG;
}
else
{
unsigned char r = *p - 1;
while (r > 0 && !IS_ACTIVE (reg_info[r]))
r--;
if (r == 0)
{
lowest_active_reg = NO_LOWEST_ACTIVE_REG;
highest_active_reg = NO_HIGHEST_ACTIVE_REG;
}
else
highest_active_reg = r;
}
if ((!MATCHED_SOMETHING (reg_info[*p])
|| just_past_start_mem == p - 1)
&& (p + 2) < pend)
{
boolean is_a_jump_n = false;
p1 = p + 2;
mcnt = 0;
switch ((re_opcode_t) *p1++)
{
case jump_n:
is_a_jump_n = true;
case pop_failure_jump:
case maybe_pop_jump:
case jump:
case dummy_failure_jump:
EXTRACT_NUMBER_AND_INCR (mcnt, p1);
if (is_a_jump_n)
p1 += 2;
break;
default:
;
}
p1 += mcnt;
if (mcnt < 0 && (re_opcode_t) *p1 == on_failure_jump
&& (re_opcode_t) p1[3] == start_memory && p1[4] == *p)
{
if (EVER_MATCHED_SOMETHING (reg_info[*p]))
{
unsigned r;
EVER_MATCHED_SOMETHING (reg_info[*p]) = 0;
for (r = *p; r < *p + *(p + 1); r++)
{
regstart[r] = old_regstart[r];
if (old_regend[r] >= regstart[r])
regend[r] = old_regend[r];
}
}
p1++;
EXTRACT_NUMBER_AND_INCR (mcnt, p1);
PUSH_FAILURE_POINT (p1 + mcnt, d, -2);
goto fail;
}
}
p += 2;
break;
case duplicate:
{
register const char *d2, *dend2;
int regno = *p++;
DEBUG_PRINT2 ("EXECUTING duplicate %d.\n", regno);
if (REG_UNSET (regstart[regno]) || REG_UNSET (regend[regno]))
goto fail;
d2 = regstart[regno];
dend2 = ((FIRST_STRING_P (regstart[regno])
== FIRST_STRING_P (regend[regno]))
? regend[regno] : end_match_1);
for (;;)
{
while (d2 == dend2)
{
if (dend2 == end_match_2) break;
if (dend2 == regend[regno]) break;
d2 = string2;
dend2 = regend[regno];
}
if (d2 == dend2) break;
PREFETCH ();
mcnt = dend - d;
if (mcnt > dend2 - d2)
mcnt = dend2 - d2;
if (RE_TRANSLATE_P (translate)
? bcmp_translate (d, d2, mcnt, translate)
: bcmp (d, d2, mcnt))
goto fail;
d += mcnt, d2 += mcnt;
SET_REGS_MATCHED ();
}
}
break;
case begline:
DEBUG_PRINT1 ("EXECUTING begline.\n");
if (AT_STRINGS_BEG (d))
{
if (!bufp->not_bol) break;
}
else if (d[-1] == '\n' && bufp->newline_anchor)
{
break;
}
goto fail;
case endline:
DEBUG_PRINT1 ("EXECUTING endline.\n");
if (AT_STRINGS_END (d))
{
if (!bufp->not_eol) break;
}
else if ((d == end1 ? *string2 : *d) == '\n'
&& bufp->newline_anchor)
{
break;
}
goto fail;
case begbuf:
DEBUG_PRINT1 ("EXECUTING begbuf.\n");
if (AT_STRINGS_BEG (d))
break;
goto fail;
case endbuf:
DEBUG_PRINT1 ("EXECUTING endbuf.\n");
if (AT_STRINGS_END (d))
break;
goto fail;
case on_failure_keep_string_jump:
DEBUG_PRINT1 ("EXECUTING on_failure_keep_string_jump");
EXTRACT_NUMBER_AND_INCR (mcnt, p);
DEBUG_PRINT3 (" %d (to 0x%x):\n", mcnt, p + mcnt);
PUSH_FAILURE_POINT (p + mcnt, NULL, -2);
break;
case on_failure_jump:
on_failure:
DEBUG_PRINT1 ("EXECUTING on_failure_jump");
#if defined (WINDOWSNT) && defined (emacs)
QUIT;
#endif
EXTRACT_NUMBER_AND_INCR (mcnt, p);
DEBUG_PRINT3 (" %d (to 0x%x)", mcnt, p + mcnt);
p1 = p;
while (p1 < pend && (re_opcode_t) *p1 == no_op)
p1++;
if (p1 < pend && (re_opcode_t) *p1 == start_memory)
{
highest_active_reg = *(p1 + 1) + *(p1 + 2);
if (lowest_active_reg == NO_LOWEST_ACTIVE_REG)
lowest_active_reg = *(p1 + 1);
}
DEBUG_PRINT1 (":\n");
PUSH_FAILURE_POINT (p + mcnt, d, -2);
break;
case maybe_pop_jump:
#if defined (WINDOWSNT) && defined (emacs)
QUIT;
#endif
EXTRACT_NUMBER_AND_INCR (mcnt, p);
DEBUG_PRINT2 ("EXECUTING maybe_pop_jump %d.\n", mcnt);
{
register unsigned char *p2 = p;
while (1)
{
if (p2 + 2 < pend
&& ((re_opcode_t) *p2 == stop_memory
|| (re_opcode_t) *p2 == start_memory))
p2 += 3;
else if (p2 + 6 < pend
&& (re_opcode_t) *p2 == dummy_failure_jump)
p2 += 6;
else
break;
}
p1 = p + mcnt;
if (p2 == pend)
{
p[-3] = (unsigned char) pop_failure_jump;
DEBUG_PRINT1
("  End of pattern: change to `pop_failure_jump'.\n");
}
else if ((re_opcode_t) *p2 == exactn
|| (bufp->newline_anchor && (re_opcode_t) *p2 == endline))
{
register unsigned int c
= *p2 == (unsigned char) endline ? '\n' : p2[2];
if ((re_opcode_t) p1[3] == exactn)
{
if (!(multibyte
&& BASE_LEADING_CODE_P (c))
? c != p1[5]
: (STRING_CHAR (&p2[2], pend - &p2[2])
!= STRING_CHAR (&p1[5], pend - &p1[5])))
{
p[-3] = (unsigned char) pop_failure_jump;
DEBUG_PRINT3 ("  %c != %c => pop_failure_jump.\n",
c, p1[5]);
}
}
else if ((re_opcode_t) p1[3] == charset
|| (re_opcode_t) p1[3] == charset_not)
{
int not = (re_opcode_t) p1[3] == charset_not;
if (multibyte
&& BASE_LEADING_CODE_P (c))
c = STRING_CHAR (&p2[2], pend - &p2[2]);
if (SINGLE_BYTE_CHAR_P (c))
{
if (c < CHARSET_BITMAP_SIZE (&p1[3]) * BYTEWIDTH
&& p1[5 + c / BYTEWIDTH] & (1 << (c % BYTEWIDTH)))
not = !not;
}
else if (CHARSET_RANGE_TABLE_EXISTS_P (&p1[3]))
CHARSET_LOOKUP_RANGE_TABLE (not, c, &p1[3]);
if (!not)
{
p[-3] = (unsigned char) pop_failure_jump;
DEBUG_PRINT1 ("	 No match => pop_failure_jump.\n");
}
}
}
else if ((re_opcode_t) *p2 == charset)
{
if ((re_opcode_t) p1[3] == exactn)
{
register unsigned int c = p1[5];
int not = 0;
if (multibyte && BASE_LEADING_CODE_P (c))
c = STRING_CHAR (&p1[5], pend - &p1[5]);
if (SINGLE_BYTE_CHAR_P (c))
{
if (c < CHARSET_BITMAP_SIZE (p2) * BYTEWIDTH
&& (p2[2 + c / BYTEWIDTH]
& (1 << (c % BYTEWIDTH))))
not = !not;
}
else if (CHARSET_RANGE_TABLE_EXISTS_P (p2))
CHARSET_LOOKUP_RANGE_TABLE (not, c, p2);
if (!not)
{
p[-3] = (unsigned char) pop_failure_jump;
DEBUG_PRINT1 ("	 No match => pop_failure_jump.\n");
}
}
else if (!multibyte || !CHARSET_RANGE_TABLE_EXISTS_P (p2))
{
if ((re_opcode_t) p1[3] == charset_not)
{
int idx;
for (idx = 0; idx < (int) p2[1]; idx++)
if (! (p2[2 + idx] == 0
|| (idx < CHARSET_BITMAP_SIZE (&p1[3])
&& ((p2[2 + idx] & ~ p1[5 + idx]) == 0))))
break;
if (idx == p2[1])
{
p[-3] = (unsigned char) pop_failure_jump;
DEBUG_PRINT1 ("	 No match => pop_failure_jump.\n");
}
}
else if ((re_opcode_t) p1[3] == charset)
{
int idx;
for (idx = 0;
(idx < (int) p2[1]
&& idx < CHARSET_BITMAP_SIZE (&p1[3]));
idx++)
if ((p2[2 + idx] & p1[5 + idx]) != 0)
break;
if (idx == p2[1]
|| idx == CHARSET_BITMAP_SIZE (&p1[3]))
{
p[-3] = (unsigned char) pop_failure_jump;
DEBUG_PRINT1 ("	 No match => pop_failure_jump.\n");
}
}
}
}
}
p -= 2;
if ((re_opcode_t) p[-1] != pop_failure_jump)
{
p[-1] = (unsigned char) jump;
DEBUG_PRINT1 ("  Match => jump.\n");
goto unconditional_jump;
}
case pop_failure_jump:
{
unsigned dummy_low_reg, dummy_high_reg;
unsigned char *pdummy;
const char *sdummy;
DEBUG_PRINT1 ("EXECUTING pop_failure_jump.\n");
POP_FAILURE_POINT (sdummy, pdummy,
dummy_low_reg, dummy_high_reg,
reg_dummy, reg_dummy, reg_info_dummy);
}
case jump:
unconditional_jump:
#if defined (WINDOWSNT) && defined (emacs)
QUIT;
#endif
EXTRACT_NUMBER_AND_INCR (mcnt, p);
DEBUG_PRINT2 ("EXECUTING jump %d ", mcnt);
p += mcnt;
DEBUG_PRINT2 ("(to 0x%x).\n", p);
break;
case jump_past_alt:
DEBUG_PRINT1 ("EXECUTING jump_past_alt.\n");
goto unconditional_jump;
case dummy_failure_jump:
DEBUG_PRINT1 ("EXECUTING dummy_failure_jump.\n");
PUSH_FAILURE_POINT (0, 0, -2);
goto unconditional_jump;
case push_dummy_failure:
DEBUG_PRINT1 ("EXECUTING push_dummy_failure.\n");
PUSH_FAILURE_POINT (0, 0, -2);
break;
case succeed_n:
EXTRACT_NUMBER (mcnt, p + 2);
DEBUG_PRINT2 ("EXECUTING succeed_n %d.\n", mcnt);
assert (mcnt >= 0);
if (mcnt > 0)
{
mcnt--;
p += 2;
STORE_NUMBER_AND_INCR (p, mcnt);
DEBUG_PRINT3 ("	Setting 0x%x to %d.\n", p, mcnt);
}
else if (mcnt == 0)
{
DEBUG_PRINT2 ("  Setting two bytes from 0x%x to no_op.\n", p+2);
p[2] = (unsigned char) no_op;
p[3] = (unsigned char) no_op;
goto on_failure;
}
break;
case jump_n:
EXTRACT_NUMBER (mcnt, p + 2);
DEBUG_PRINT2 ("EXECUTING jump_n %d.\n", mcnt);
if (mcnt)
{
mcnt--;
STORE_NUMBER (p + 2, mcnt);
goto unconditional_jump;
}
else
p += 4;
break;
case set_number_at:
{
DEBUG_PRINT1 ("EXECUTING set_number_at.\n");
EXTRACT_NUMBER_AND_INCR (mcnt, p);
p1 = p + mcnt;
EXTRACT_NUMBER_AND_INCR (mcnt, p);
DEBUG_PRINT3 ("  Setting 0x%x to %d.\n", p1, mcnt);
STORE_NUMBER (p1, mcnt);
break;
}
case wordbound:
DEBUG_PRINT1 ("EXECUTING wordbound.\n");
if (AT_STRINGS_BEG (d) || AT_STRINGS_END (d))
break;
else
{
int c1, c2, s1, s2;
int pos1 = PTR_TO_OFFSET (d - 1);
int charpos;
GET_CHAR_BEFORE_2 (c1, d, string1, end1, string2, end2);
GET_CHAR_AFTER_2 (c2, d, string1, end1, string2, end2);
#ifdef emacs
charpos = SYNTAX_TABLE_BYTE_TO_CHAR (pos1);
UPDATE_SYNTAX_TABLE (charpos);
#endif
s1 = SYNTAX (c1);
#ifdef emacs
UPDATE_SYNTAX_TABLE_FORWARD (charpos + 1);
#endif
s2 = SYNTAX (c2);
if (
((s1 == Sword) != (s2 == Sword))
|| ((s1 == Sword) && WORD_BOUNDARY_P (c1, c2)))
break;
}
goto fail;
case notwordbound:
DEBUG_PRINT1 ("EXECUTING notwordbound.\n");
if (AT_STRINGS_BEG (d) || AT_STRINGS_END (d))
goto fail;
else
{
int c1, c2, s1, s2;
int pos1 = PTR_TO_OFFSET (d - 1);
int charpos;
GET_CHAR_BEFORE_2 (c1, d, string1, end1, string2, end2);
GET_CHAR_AFTER_2 (c2, d, string1, end1, string2, end2);
#ifdef emacs
charpos = SYNTAX_TABLE_BYTE_TO_CHAR (pos1);
UPDATE_SYNTAX_TABLE (charpos);
#endif
s1 = SYNTAX (c1);
#ifdef emacs
UPDATE_SYNTAX_TABLE_FORWARD (charpos + 1);
#endif
s2 = SYNTAX (c2);
if (
((s1 == Sword) != (s2 == Sword))
|| ((s1 == Sword) && WORD_BOUNDARY_P (c1, c2)))
goto fail;
}
break;
case wordbeg:
DEBUG_PRINT1 ("EXECUTING wordbeg.\n");
if (AT_STRINGS_END (d))
goto fail;
else
{
int c1, c2, s1, s2;
int pos1 = PTR_TO_OFFSET (d);
int charpos;
GET_CHAR_AFTER_2 (c2, d, string1, end1, string2, end2);
#ifdef emacs
charpos = SYNTAX_TABLE_BYTE_TO_CHAR (pos1);
UPDATE_SYNTAX_TABLE (charpos);
#endif
s2 = SYNTAX (c2);
if (s2 != Sword)
goto fail;
if (!AT_STRINGS_BEG (d))
{
GET_CHAR_BEFORE_2 (c1, d, string1, end1, string2, end2);
#ifdef emacs
UPDATE_SYNTAX_TABLE_BACKWARD (charpos - 1);
#endif
s1 = SYNTAX (c1);
if ((s1 == Sword) && !WORD_BOUNDARY_P (c1, c2))
goto fail;
}
}
break;
case wordend:
DEBUG_PRINT1 ("EXECUTING wordend.\n");
if (AT_STRINGS_BEG (d))
goto fail;
else
{
int c1, c2, s1, s2;
int pos1 = PTR_TO_OFFSET (d);
int charpos;
GET_CHAR_BEFORE_2 (c1, d, string1, end1, string2, end2);
#ifdef emacs
charpos = SYNTAX_TABLE_BYTE_TO_CHAR (pos1 - 1);
UPDATE_SYNTAX_TABLE (charpos);
#endif
s1 = SYNTAX (c1);
if (s1 != Sword)
goto fail;
if (!AT_STRINGS_END (d))
{
GET_CHAR_AFTER_2 (c2, d, string1, end1, string2, end2);
#ifdef emacs
UPDATE_SYNTAX_TABLE_FORWARD (charpos);
#endif
s2 = SYNTAX (c2);
if ((s2 == Sword) && !WORD_BOUNDARY_P (c1, c2))
goto fail;
}
}
break;
#ifdef emacs
case before_dot:
DEBUG_PRINT1 ("EXECUTING before_dot.\n");
if (PTR_BYTE_POS ((unsigned char *) d) >= PT_BYTE)
goto fail;
break;
case at_dot:
DEBUG_PRINT1 ("EXECUTING at_dot.\n");
if (PTR_BYTE_POS ((unsigned char *) d) != PT_BYTE)
goto fail;
break;
case after_dot:
DEBUG_PRINT1 ("EXECUTING after_dot.\n");
if (PTR_BYTE_POS ((unsigned char *) d) <= PT_BYTE)
goto fail;
break;
case syntaxspec:
DEBUG_PRINT2 ("EXECUTING syntaxspec %d.\n", mcnt);
mcnt = *p++;
goto matchsyntax;
case wordchar:
DEBUG_PRINT1 ("EXECUTING Emacs wordchar.\n");
mcnt = (int) Sword;
matchsyntax:
PREFETCH ();
#ifdef emacs
{
int pos1 = SYNTAX_TABLE_BYTE_TO_CHAR (PTR_TO_OFFSET (d));
UPDATE_SYNTAX_TABLE (pos1);
}
#endif
{
int c, len;
if (multibyte)
c = STRING_CHAR_AND_LENGTH (d, dend - d, len);
else
c = *d, len = 1;
if (SYNTAX (c) != (enum syntaxcode) mcnt)
goto fail;
d += len;
}
SET_REGS_MATCHED ();
break;
case notsyntaxspec:
DEBUG_PRINT2 ("EXECUTING notsyntaxspec %d.\n", mcnt);
mcnt = *p++;
goto matchnotsyntax;
case notwordchar:
DEBUG_PRINT1 ("EXECUTING Emacs notwordchar.\n");
mcnt = (int) Sword;
matchnotsyntax:
PREFETCH ();
#ifdef emacs
{
int pos1 = SYNTAX_TABLE_BYTE_TO_CHAR (PTR_TO_OFFSET (d));
UPDATE_SYNTAX_TABLE (pos1);
}
#endif
{
int c, len;
if (multibyte)
c = STRING_CHAR_AND_LENGTH (d, dend - d, len);
else
c = *d, len = 1;
if (SYNTAX (c) == (enum syntaxcode) mcnt)
goto fail;
d += len;
}
SET_REGS_MATCHED ();
break;
case categoryspec:
DEBUG_PRINT2 ("EXECUTING categoryspec %d.\n", *p);
mcnt = *p++;
PREFETCH ();
{
int c, len;
if (multibyte)
c = STRING_CHAR_AND_LENGTH (d, dend - d, len);
else
c = *d, len = 1;
if (!CHAR_HAS_CATEGORY (c, mcnt))
goto fail;
d += len;
}
SET_REGS_MATCHED ();
break;
case notcategoryspec:
DEBUG_PRINT2 ("EXECUTING notcategoryspec %d.\n", *p);
mcnt = *p++;
PREFETCH ();
{
int c, len;
if (multibyte)
c = STRING_CHAR_AND_LENGTH (d, dend - d, len);
else
c = *d, len = 1;
if (CHAR_HAS_CATEGORY (c, mcnt))
goto fail;
d += len;
}
SET_REGS_MATCHED ();
break;
#else
case wordchar:
DEBUG_PRINT1 ("EXECUTING non-Emacs wordchar.\n");
PREFETCH ();
if (!WORDCHAR_P (d))
goto fail;
SET_REGS_MATCHED ();
d++;
break;
case notwordchar:
DEBUG_PRINT1 ("EXECUTING non-Emacs notwordchar.\n");
PREFETCH ();
if (WORDCHAR_P (d))
goto fail;
SET_REGS_MATCHED ();
d++;
break;
#endif
default:
abort ();
}
continue;
fail:
#if defined (WINDOWSNT) && defined (emacs)
QUIT;
#endif
if (!FAIL_STACK_EMPTY ())
{
DEBUG_PRINT1 ("\nFAIL:\n");
POP_FAILURE_POINT (d, p,
lowest_active_reg, highest_active_reg,
regstart, regend, reg_info);
if (!p)
goto fail;
assert (p <= pend);
if (p < pend)
{
boolean is_a_jump_n = false;
switch ((re_opcode_t) *p)
{
case jump_n:
is_a_jump_n = true;
case maybe_pop_jump:
case pop_failure_jump:
case jump:
p1 = p + 1;
EXTRACT_NUMBER_AND_INCR (mcnt, p1);
p1 += mcnt;
if ((is_a_jump_n && (re_opcode_t) *p1 == succeed_n)
|| (!is_a_jump_n
&& (re_opcode_t) *p1 == on_failure_jump))
goto fail;
break;
default:
;
}
}
if (d >= string1 && d <= end1)
dend = end_match_1;
}
else
break;
}
if (best_regs_set)
goto restore_best_regs;
FREE_VARIABLES ();
return -1;
}
static boolean
group_match_null_string_p (p, end, reg_info)
unsigned char **p, *end;
register_info_type *reg_info;
{
int mcnt;
unsigned char *p1 = *p + 2;
while (p1 < end)
{
switch ((re_opcode_t) *p1)
{
case on_failure_jump:
p1++;
EXTRACT_NUMBER_AND_INCR (mcnt, p1);
if (mcnt >= 0)
{
while ((re_opcode_t) p1[mcnt-3] == jump_past_alt)
{
if (!alt_match_null_string_p (p1, p1 + mcnt - 3,
reg_info))
return false;
p1 += mcnt;
if ((re_opcode_t) *p1 != on_failure_jump)
break;
p1++;
EXTRACT_NUMBER_AND_INCR (mcnt, p1);
if ((re_opcode_t) p1[mcnt-3] != jump_past_alt)
{
p1 -= 3;
break;
}
}
EXTRACT_NUMBER (mcnt, p1 - 2);
if (!alt_match_null_string_p (p1, p1 + mcnt, reg_info))
return false;
p1 += mcnt;
}
break;
case stop_memory:
assert (p1[1] == **p);
*p = p1 + 2;
return true;
default:
if (!common_op_match_null_string_p (&p1, end, reg_info))
return false;
}
}
return false;
}
static boolean
alt_match_null_string_p (p, end, reg_info)
unsigned char *p, *end;
register_info_type *reg_info;
{
int mcnt;
unsigned char *p1 = p;
while (p1 < end)
{
switch ((re_opcode_t) *p1)
{
case on_failure_jump:
p1++;
EXTRACT_NUMBER_AND_INCR (mcnt, p1);
p1 += mcnt;
break;
default:
if (!common_op_match_null_string_p (&p1, end, reg_info))
return false;
}
}
return true;
}
static boolean
common_op_match_null_string_p (p, end, reg_info)
unsigned char **p, *end;
register_info_type *reg_info;
{
int mcnt;
boolean ret;
int reg_no;
unsigned char *p1 = *p;
switch ((re_opcode_t) *p1++)
{
case no_op:
case begline:
case endline:
case begbuf:
case endbuf:
case wordbeg:
case wordend:
case wordbound:
case notwordbound:
#ifdef emacs
case before_dot:
case at_dot:
case after_dot:
#endif
break;
case start_memory:
reg_no = *p1;
assert (reg_no > 0 && reg_no <= MAX_REGNUM);
ret = group_match_null_string_p (&p1, end, reg_info);
if (REG_MATCH_NULL_STRING_P (reg_info[reg_no]) == MATCH_NULL_UNSET_VALUE)
REG_MATCH_NULL_STRING_P (reg_info[reg_no]) = ret;
if (!ret)
return false;
break;
case jump:
EXTRACT_NUMBER_AND_INCR (mcnt, p1);
if (mcnt >= 0)
p1 += mcnt;
else
return false;
break;
case succeed_n:
p1 += 2;
EXTRACT_NUMBER_AND_INCR (mcnt, p1);
if (mcnt == 0)
{
p1 -= 4;
EXTRACT_NUMBER_AND_INCR (mcnt, p1);
p1 += mcnt;
}
else
return false;
break;
case duplicate:
if (!REG_MATCH_NULL_STRING_P (reg_info[*p1]))
return false;
break;
case set_number_at:
p1 += 4;
default:
return false;
}
*p = p1;
return true;
}
static int
bcmp_translate (s1, s2, len, translate)
unsigned char *s1, *s2;
register int len;
RE_TRANSLATE_TYPE translate;
{
register unsigned char *p1 = s1, *p2 = s2;
unsigned char *p1_end = s1 + len;
unsigned char *p2_end = s2 + len;
while (p1 != p1_end && p2 != p2_end)
{
int p1_charlen, p2_charlen;
int p1_ch, p2_ch;
p1_ch = STRING_CHAR_AND_LENGTH (p1, p1_end - p1, p1_charlen);
p2_ch = STRING_CHAR_AND_LENGTH (p2, p2_end - p2, p2_charlen);
if (RE_TRANSLATE (translate, p1_ch)
!= RE_TRANSLATE (translate, p2_ch))
return 1;
p1 += p1_charlen, p2 += p2_charlen;
}
if (p1 != p1_end || p2 != p2_end)
return 1;
return 0;
}
const char *
re_compile_pattern (pattern, length, bufp)
const char *pattern;
int length;
struct re_pattern_buffer *bufp;
{
reg_errcode_t ret;
bufp->regs_allocated = REGS_UNALLOCATED;
bufp->no_sub = 0;
bufp->newline_anchor = 1;
ret = regex_compile (pattern, length, re_syntax_options, bufp);
if (!ret)
return NULL;
return gettext (re_error_msgid[(int) ret]);
}
#if defined (_REGEX_RE_COMP) || defined (_LIBC)
static struct re_pattern_buffer re_comp_buf;
char *
#ifdef _LIBC
weak_function
#endif
re_comp (s)
const char *s;
{
reg_errcode_t ret;
if (!s)
{
if (!re_comp_buf.buffer)
return gettext ("No previous regular expression");
return 0;
}
if (!re_comp_buf.buffer)
{
re_comp_buf.buffer = (unsigned char *) malloc (200);
if (re_comp_buf.buffer == NULL)
return (char *) gettext (re_error_msgid[(int) REG_ESPACE]);
re_comp_buf.allocated = 200;
re_comp_buf.fastmap = (char *) malloc (1 << BYTEWIDTH);
if (re_comp_buf.fastmap == NULL)
return (char *) gettext (re_error_msgid[(int) REG_ESPACE]);
}
re_comp_buf.newline_anchor = 1;
ret = regex_compile (s, strlen (s), re_syntax_options, &re_comp_buf);
if (!ret)
return NULL;
return (char *) gettext (re_error_msgid[(int) ret]);
}
int
#ifdef _LIBC
weak_function
#endif
re_exec (s)
const char *s;
{
const int len = strlen (s);
return
0 <= re_search (&re_comp_buf, s, len, 0, len, (struct re_registers *) 0);
}
#endif
#ifndef emacs
int
regcomp (preg, pattern, cflags)
regex_t *preg;
const char *pattern;
int cflags;
{
reg_errcode_t ret;
unsigned syntax
= (cflags & REG_EXTENDED) ?
RE_SYNTAX_POSIX_EXTENDED : RE_SYNTAX_POSIX_BASIC;
preg->buffer = 0;
preg->allocated = 0;
preg->used = 0;
preg->fastmap = 0;
if (cflags & REG_ICASE)
{
unsigned i;
preg->translate
= (RE_TRANSLATE_TYPE) malloc (CHAR_SET_SIZE
* sizeof (*(RE_TRANSLATE_TYPE)0));
if (preg->translate == NULL)
return (int) REG_ESPACE;
for (i = 0; i < CHAR_SET_SIZE; i++)
preg->translate[i] = ISUPPER (i) ? tolower (i) : i;
}
else
preg->translate = NULL;
if (cflags & REG_NEWLINE)
{
syntax &= ~RE_DOT_NEWLINE;
syntax |= RE_HAT_LISTS_NOT_NEWLINE;
preg->newline_anchor = 1;
}
else
preg->newline_anchor = 0;
preg->no_sub = !!(cflags & REG_NOSUB);
ret = regex_compile (pattern, strlen (pattern), syntax, preg);
if (ret == REG_ERPAREN) ret = REG_EPAREN;
return (int) ret;
}
int
regexec (preg, string, nmatch, pmatch, eflags)
const regex_t *preg;
const char *string;
size_t nmatch;
regmatch_t pmatch[];
int eflags;
{
int ret;
struct re_registers regs;
regex_t private_preg;
int len = strlen (string);
boolean want_reg_info = !preg->no_sub && nmatch > 0;
private_preg = *preg;
private_preg.not_bol = !!(eflags & REG_NOTBOL);
private_preg.not_eol = !!(eflags & REG_NOTEOL);
private_preg.regs_allocated = REGS_FIXED;
if (want_reg_info)
{
regs.num_regs = nmatch;
regs.start = TALLOC (nmatch, regoff_t);
regs.end = TALLOC (nmatch, regoff_t);
if (regs.start == NULL || regs.end == NULL)
return (int) REG_NOMATCH;
}
ret = re_search (&private_preg, string, len,
0, len,
want_reg_info ? &regs : (struct re_registers *) 0);
if (want_reg_info)
{
if (ret >= 0)
{
unsigned r;
for (r = 0; r < nmatch; r++)
{
pmatch[r].rm_so = regs.start[r];
pmatch[r].rm_eo = regs.end[r];
}
}
free (regs.start);
free (regs.end);
}
return ret >= 0 ? (int) REG_NOERROR : (int) REG_NOMATCH;
}
size_t
regerror (errcode, preg, errbuf, errbuf_size)
int errcode;
const regex_t *preg;
char *errbuf;
size_t errbuf_size;
{
const char *msg;
size_t msg_size;
if (errcode < 0
|| errcode >= (sizeof (re_error_msgid) / sizeof (re_error_msgid[0])))
abort ();
msg = gettext (re_error_msgid[errcode]);
msg_size = strlen (msg) + 1;
if (errbuf_size != 0)
{
if (msg_size > errbuf_size)
{
strncpy (errbuf, msg, errbuf_size - 1);
errbuf[errbuf_size - 1] = 0;
}
else
strcpy (errbuf, msg);
}
return msg_size;
}
void
regfree (preg)
regex_t *preg;
{
if (preg->buffer != NULL)
free (preg->buffer);
preg->buffer = NULL;
preg->allocated = 0;
preg->used = 0;
if (preg->fastmap != NULL)
free (preg->fastmap);
preg->fastmap = NULL;
preg->fastmap_accurate = 0;
if (preg->translate != NULL)
free (preg->translate);
preg->translate = NULL;
}
#endif