#include "ctype_.h"
#include "stdio_.h"
#include "string_.h"
#include "gsexit.h"
#include "gsmemory.h"
#include "gsargs.h"
#include "gserrors.h"
void
arg_init(arg_list * pal, const char **argv, int argc,
FILE * (*arg_fopen) (const char *fname, void *fopen_data),
void *fopen_data)
{
pal->expand_ats = true;
pal->arg_fopen = arg_fopen;
pal->fopen_data = fopen_data;
pal->argp = argv + 1;
pal->argn = argc - 1;
pal->depth = 0;
}
int
arg_push_memory_string(arg_list * pal, char *str, gs_memory_t * mem)
{
arg_source *pas;
if (pal->depth == arg_depth_max) {
lprintf("Too much nesting of @-files.\n");
return 1;
}
pas = &pal->sources[pal->depth];
pas->is_file = false;
pas->u.s.chars = str;
pas->u.s.memory = mem;
pas->u.s.str = str;
pal->depth++;
return 0;
}
void
arg_finit(arg_list * pal)
{
while (pal->depth) {
arg_source *pas = &pal->sources[--(pal->depth)];
if (pas->is_file)
fclose(pas->u.file);
else if (pas->u.s.memory)
gs_free_object(pas->u.s.memory, pas->u.s.chars, "arg_finit");
}
}
const char *
arg_next(arg_list * pal, int *code)
{
arg_source *pas;
FILE *f;
const char *astr = 0;
char *cstr;
const char *result;
int endc;
int c, i;
bool in_quote, eol;
top:pas = &pal->sources[pal->depth - 1];
if (pal->depth == 0) {
if (pal->argn <= 0)
return 0;
pal->argn--;
result = *(pal->argp++);
goto at;
}
if (pas->is_file)
f = pas->u.file, endc = EOF;
else
astr = pas->u.s.str, f = NULL, endc = 0;
result = cstr = pal->cstr;
#define cfsgetc() (f == NULL ? (*astr ? *astr++ : 0) : fgetc(f))
#define is_eol(c) (c == '\r' || c == '\n')
i = 0;
in_quote = false;
eol = true;
c = cfsgetc();
for (i = 0;;) {
if (c == endc) {
if (in_quote) {
cstr[i] = 0;
errprintf("Unterminated quote in @-file: %s\n", cstr);
*code = gs_error_Fatal;
return NULL;
}
if (i == 0) {
if (f != NULL)
fclose(f);
else if (pas->u.s.memory)
gs_free_object(pas->u.s.memory, pas->u.s.chars,
"arg_next");
pal->depth--;
goto top;
}
break;
}
if (isspace(c)) {
if (i == 0) {
c = cfsgetc();
continue;
}
if (!in_quote)
break;
}
if (c == '#' && eol) {
do {
c = cfsgetc();
} while (!(c == endc || is_eol(c)));
if (c == '\r')
c = cfsgetc();
if (c == '\n')
c = cfsgetc();
continue;
}
if (c == '\\') {
c = cfsgetc();
if (is_eol(c)) {
if (c == '\r')
c = cfsgetc();
if (c == '\n')
c = cfsgetc();
eol = true;
continue;
}
if (i == arg_str_max - 1) {
cstr[i] = 0;
errprintf("Command too long: %s\n", cstr);
*code = gs_error_Fatal;
return NULL;
}
cstr[i++] = '\\';
eol = false;
continue;
}
if (i == arg_str_max - 1) {
cstr[i] = 0;
errprintf("Command too long: %s\n", cstr);
*code = gs_error_Fatal;
return NULL;
}
if (c == '"' && f != NULL)
in_quote = !in_quote;
else
cstr[i++] = c;
eol = is_eol(c);
c = cfsgetc();
}
cstr[i] = 0;
if (f == NULL)
pas->u.s.str = astr;
at:if (pal->expand_ats && result[0] == '@') {
if (pal->depth == arg_depth_max) {
lprintf("Too much nesting of @-files.\n");
*code = gs_error_Fatal;
return NULL;
}
result++;
f = (*pal->arg_fopen) (result, pal->fopen_data);
if (f == NULL) {
errprintf("Unable to open command line file %s\n", result);
*code = gs_error_Fatal;
return NULL;
}
pal->depth++;
pas++;
pas->is_file = true;
pas->u.file = f;
goto top;
}
return result;
}
char *
arg_copy(const char *str, gs_memory_t * mem)
{
char *sstr = (char *)gs_alloc_bytes(mem, strlen(str) + 1, "arg_copy");
if (sstr == 0) {
lprintf("Out of memory!\n");
return NULL;
}
strcpy(sstr, str);
return sstr;
}