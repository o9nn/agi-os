#include <string.h>
#include "connectors.h"
#include "dict-common/dialect.h"
#include "dict-common/dict-affix.h"
#include "dict-common/dict-common.h"
#include "dict-common/dict-internals.h"
#include "dict-common/dict-utils.h"
#include "dict-common/file-utils.h"
#include "dict-common/idiom.h"
#include "dict-ram/dict-ram.h"
#include "error.h"
#include "externs.h"
#include "print/print.h"
#include "read-dict.h"
#include "string-set.h"
#include "tokenize/tok-structures.h"
#include "utilities.h"
#include "word-file.h"
struct FileCursor_s
{
Dictionary dict;
const char * input;
const char * pin;
bool recursive_error;
bool is_special;
int already_got_it;
char token[MAX_TOKEN_LENGTH];
};
typedef struct FileCursor_s * FileCursor;
static bool link_advance(FileCursor);
static void dict_error2(FileCursor fcurs, const char * s, const char *s2)
{
if (fcurs->recursive_error) return;
fcurs->recursive_error = true;
Dictionary dict = fcurs->dict;
char token[MAX_TOKEN_LENGTH];
strcpy(token, fcurs->token);
bool save_is_special = fcurs->is_special;
const char * save_input = fcurs->input;
const char * save_pin = fcurs->pin;
int save_already_got_it = fcurs->already_got_it;
int save_line_number = dict->line_number;
#define ERRBUFLEN 1024
char tokens[ERRBUFLEN], t[ERRBUFLEN];
int pos = 1;
tokens[0] = '\0';
for (int i=0; i<5 && fcurs->token[0] != '\0'; i++)
{
pos += snprintf(t, ERRBUFLEN, "\"%s\" ", fcurs->token);
strncat(tokens, t, ERRBUFLEN-1-pos);
if (!link_advance(fcurs)) break;
}
tokens[pos] = '\0';
strcpy(fcurs->token, token);
fcurs->is_special = save_is_special;
fcurs->input = save_input;
fcurs->pin = save_pin;
fcurs->already_got_it = save_already_got_it;
dict->line_number = save_line_number;
if (s2)
{
prt_error("Error: While parsing dictionary \"%s\":\n"
"%s \"%s\"\n\t Line %d, next tokens: %s\n",
dict->name, s, s2, dict->line_number, tokens);
}
else
{
prt_error("Error: While parsing dictionary \"%s\":\n"
"%s\n\t Line %d, next tokens: %s\n",
dict->name, s, dict->line_number, tokens);
}
fcurs->recursive_error = false;
}
static void dict_error(FileCursor fcurs, const char * s)
{
dict_error2(fcurs, s, NULL);
}
static void warning(FileCursor fcurs, const char * s)
{
prt_error("Warning: %s\n"
"\tline %d, current token = \"%s\"\n",
s, fcurs->dict->line_number, fcurs->token);
}
#define MAXUTFLEN 7
typedef char utf8char[MAXUTFLEN];
static bool get_character(FileCursor fcurs, int quote_mode, utf8char uc)
{
Dictionary dict = fcurs->dict;
int i = 0;
while (1)
{
char c = *(fcurs->pin++);
if ((c == '%') && (!quote_mode))
{
while ((c != 0x0) && (c != '\n')) c = *(fcurs->pin++);
if (c == 0x0) break;
dict->line_number++;
continue;
}
if (c == '\n')
dict->line_number++;
if ((0 == i) && ((c & 0x80) == 0x0))
{
uc[0] = c;
uc[1] = 0x0;
return true;
}
uc[0] = c;
i = 1;
while (i < MAXUTFLEN-1)
{
c = *(fcurs->pin++);
if (((c & 0x80) == 0x0) || ((c & 0xc0) == 0xc0))
{
fcurs->pin--;
uc[i] = 0x0;
return true;
}
uc[i] = c;
i++;
}
dict_error(fcurs, "UTF8 char is too long.");
return false;
}
uc[0] = 0x0;
return true;
}
#define SPECIAL "(){};[]&^|:"
#define SYM_AND '^'
#define ANY_DIR '$'
#define WILD_TYPE '*'
static bool char_is_special(char c)
{
return (NULL != strchr(SPECIAL, c));
}
NO_SAN_DICT
static bool link_advance(FileCursor fcurs)
{
bool quote_mode = false;
fcurs->is_special = false;
if (fcurs->already_got_it != '\0')
{
fcurs->is_special = char_is_special(fcurs->already_got_it);
if (fcurs->already_got_it == EOF) {
fcurs->token[0] = '\0';
} else {
fcurs->token[0] = (char)fcurs->already_got_it;
fcurs->token[1] = '\0';
}
fcurs->already_got_it = '\0';
return true;
}
utf8char c;
do
{
bool ok = get_character(fcurs, false, c);
if (!ok) return false;
}
while (lg_isspace((unsigned char)c[0]));
int i = 0;
for (;;)
{
if (i > MAX_TOKEN_LENGTH-3) {
dict_error(fcurs, "Token too long.");
return false;
}
if (quote_mode) {
if (c[0] == '"' &&
(*fcurs->pin == ':' || *fcurs->pin == ';' ||
lg_isspace((unsigned char)*fcurs->pin))) {
fcurs->token[i] = '\0';
return true;
}
if (c[0] == '\0')
{
dict_error(fcurs, "EOF while reading quoted token.");
return false;
}
int nr = 0;
while (c[nr]) {fcurs->token[i] = c[nr]; i++; nr++; }
} else {
if ('\0' == c[1] && char_is_special(c[0]))
{
if (i == 0)
{
fcurs->token[0] = c[0];
fcurs->token[1] = '\0';
fcurs->is_special = true;
return true;
}
fcurs->token[i] = '\0';
fcurs->already_got_it = c[0];
return true;
}
if (c[0] == 0x0) {
if (i != 0) fcurs->already_got_it = '\0';
fcurs->token[0] = '\0';
return true;
}
if (lg_isspace((unsigned char)c[0])) {
fcurs->token[i] = '\0';
return true;
}
if (c[0] == '\"') {
quote_mode = true;
} else {
int nr = 0;
while (c[nr]) {fcurs->token[i] = c[nr]; i++; nr++; }
}
}
bool ok = get_character(fcurs, quote_mode, c);
if (!ok) return false;
}
}
static int is_equal(FileCursor fcurs, char c)
{
return (fcurs->is_special &&
c == fcurs->token[0] &&
fcurs->token[1] == '\0');
}
static bool check_connector(FileCursor fcurs, const char * s)
{
int i;
i = strlen(s);
if (i < 1) {
dict_error(fcurs, "Expecting a connector.");
return false;
}
i = s[i-1];
if ((i != '+') && (i != '-') && (i != ANY_DIR)) {
dict_error(fcurs, "A connector must end in a \"+\", \"-\" or \"$\".");
return false;
}
if (*s == '@') s++;
if (('h' == *s) || ('d' == *s)) s++;
if (!is_connector_name_char(*s)) {
dict_error2(fcurs, "Invalid character in connector "
"(connectors must start with an uppercase letter "
"after an optional \"h\" or \"d\"):", (char[]){*s, '\0'});
return false;
}
if (*s == '_')
{
dict_error(fcurs, "Invalid character in connector "
"(an initial \"_\" is reserved for internal use).");
return false;
}
do { s++; } while (is_connector_name_char(*s));
while (s[1]) {
if (!is_connector_subscript_char(*s) && (*s != WILD_TYPE)) {
dict_error2(fcurs, "Invalid character in connector subscript "
"(only lowercase letters, digits, and \"*\" are allowed):",
(char[]){*s, '\0'});
return false;
}
s++;
}
return true;
}
static Exp * make_dir_connector(Dictionary dict, FileCursor fcurs, int i)
{
char *constring;
bool multi = false;
char dir = fcurs->token[i];
fcurs->token[i] = '\0';
if (fcurs->token[0] == '@')
{
constring = fcurs->token+1;
multi = true;
}
else
constring = fcurs->token;
return make_connector_node(dict, dict->Exp_pool,
constring, dir, multi);
}
static unsigned int exptag_macro_add(Dictionary dict, const char *tag)
{
expression_tag *mt = dict->macro_tag;
if (mt == NULL) return 0;
if (mt->num == mt->size)
{
if (mt->num == 0)
mt->size = 128;
else
mt->size *= 2;
mt->name = realloc(mt->name, mt->size * sizeof(*mt->name));
}
mt->name[mt->num] = tag;
return mt->num++;
}
static Exp * make_connector(FileCursor fcurs)
{
Dictionary dict = fcurs->dict;
Exp * n;
int i = strlen(fcurs->token) - 1;
if ((fcurs->token[i] != '+') &&
(fcurs->token[i] != '-') &&
(fcurs->token[i] != ANY_DIR))
{
patch_subscript(fcurs->token);
Dict_node * dn = strict_lookup_list(dict, fcurs->token);
if (dn == NULL)
{
dict_error2(fcurs, "Perhaps missing + or - in a connector.\n"
"Or perhaps you forgot the subscript on a word.\n"
"Or perhaps the word is used before it is defined:",
fcurs->token);
return NULL;
}
if (dn->right != NULL)
{
dict_node_free_list(dn);
dict_error2(fcurs, "Referencing a duplicate word:", fcurs->token);
return NULL;
}
n = make_unary_node(dict->Exp_pool, dn->exp);
n->tag_id = exptag_macro_add(dict, dn->string);
if (n->tag_id != 0) n->tag_type = Exptag_macro;
dict_node_free_list(dn);
}
else
{
if (!check_connector(fcurs, fcurs->token))
{
return NULL;
}
if ((fcurs->token[i] == '+') || (fcurs->token[i] == '-'))
{
n = make_dir_connector(dict, fcurs, i);
if (NULL == n) return NULL;
}
else if (fcurs->token[i] == ANY_DIR)
{
Exp *plu, *min;
fcurs->token[i] = '+';
plu = make_dir_connector(dict, fcurs, i);
if (NULL == plu) return NULL;
fcurs->token[i] = '-';
min = make_dir_connector(dict, fcurs, i);
if (NULL == min) return NULL;
n = make_or_node(dict->Exp_pool, plu, min);
}
else
{
dict_error(fcurs, "Unknown connector direction type.");
return NULL;
}
}
if (!link_advance(fcurs))
{
free(n);
return NULL;
}
return n;
}
static bool is_number(const char * str)
{
if (str[0] == '\0') return false;
if ('+' == str[0] || '-' == str[0]) str++;
size_t numlen = strspn(str, "0123456789.");
return str[numlen] == '\0';
}
static Exp *make_expression(FileCursor fcurs)
{
Dictionary dict = fcurs->dict;
Exp *nl = NULL;
Exp *e_head = NULL;
Exp *e_tail = NULL;
bool is_sym_and = false;
while (true)
{
if (is_equal(fcurs, '('))
{
if (!link_advance(fcurs)) {
return NULL;
}
nl = make_expression(fcurs);
if (nl == NULL) {
return NULL;
}
if (!is_equal(fcurs, ')')) {
dict_error(fcurs, "Expecting a \")\".");
return NULL;
}
if (!link_advance(fcurs)) {
return NULL;
}
}
else if (is_equal(fcurs, '{'))
{
if (!link_advance(fcurs)) {
return NULL;
}
nl = make_expression(fcurs);
if (nl == NULL) {
return NULL;
}
if (!is_equal(fcurs, '}')) {
dict_error(fcurs, "Expecting a \"}\".");
return NULL;
}
if (!link_advance(fcurs)) {
return NULL;
}
nl = make_optional_node(dict->Exp_pool, nl);
}
else if (is_equal(fcurs, '['))
{
if (!link_advance(fcurs)) {
return NULL;
}
nl = make_expression(fcurs);
if (nl == NULL) {
return NULL;
}
if (!is_equal(fcurs, ']')) {
dict_error(fcurs, "Expecting a \"]\".");
return NULL;
}
if (!link_advance(fcurs)) {
return NULL;
}
if (is_number(fcurs->token))
{
float cost;
if (strtofC(fcurs->token, &cost))
{
nl->cost += cost;
}
else
{
warning(fcurs, "Invalid cost (using 1.0)\n");
nl->cost += 1.0F;
}
if (!link_advance(fcurs)) {
return NULL;
}
}
else if ((strcmp(fcurs->token, "or") != 0) &&
(strcmp(fcurs->token, "and") != 0) &&
isalpha((unsigned char)fcurs->token[0]))
{
const char *bad = valid_dialect_name(fcurs->token);
if (bad != NULL)
{
char badchar[] = { *bad, '\0' };
dict_error2(fcurs, "Invalid character in dialect tag name:",
badchar);
return NULL;
}
if ((nl->type == CONNECTOR_type) || (nl->tag_type != Exptag_none))
{
nl = make_unary_node(dict->Exp_pool, nl);
}
nl->tag_id = exptag_dialect_add(dict, fcurs->token);
nl->tag_type = Exptag_dialect;
if (!link_advance(fcurs)) {
return NULL;
}
}
else
{
nl->cost += 1.0F;
}
}
else if (!fcurs->is_special)
{
nl = make_connector(fcurs);
if (nl == NULL) {
return NULL;
}
}
else if (is_equal(fcurs, ')') || is_equal(fcurs, ']'))
{
nl = make_zeroary_node(dict->Exp_pool);
}
else
{
dict_error(fcurs, "Connector, \"(\", \"[\", or \"{\" expected.");
return NULL;
}
if (is_sym_and)
{
Exp *na = make_and_node(dict->Exp_pool,
Exp_create_dup(dict->Exp_pool, e_tail),
Exp_create_dup(dict->Exp_pool, nl));
Exp *nb = make_and_node(dict->Exp_pool,
Exp_create_dup(dict->Exp_pool, nl),
Exp_create_dup(dict->Exp_pool, e_tail));
Exp *or = make_or_node(dict->Exp_pool, na, nb);
*e_tail = *or;
is_sym_and = false;
}
else if (e_tail != NULL)
{
e_tail->operand_next = nl;
e_tail = nl;
}
Exp_type op;
if (is_equal(fcurs, '&') || (strcmp(fcurs->token, "and") == 0))
{
op = AND_type;
}
else if (is_equal(fcurs, '|') || (strcmp(fcurs->token, "or") == 0))
{
op = OR_type;
}
else if (is_equal(fcurs, SYM_AND) || (strcmp(fcurs->token, "sym") == 0))
{
op = AND_type;
is_sym_and = true;
}
else
{
if (e_head != NULL) return e_head;
return nl;
}
if (e_head == NULL)
{
e_head = make_join_node(dict->Exp_pool, nl, NULL, op);
}
else
{
if (e_head->type != op)
{
dict_error(fcurs, "\"and\" and \"or\" at the same level in an expression.");
return NULL;
}
}
if (!link_advance(fcurs)) {
return NULL;
}
if (e_tail == NULL)
e_tail = e_head->operand_first;
}
}
static void add_condesc_length_limit(Dictionary dict, Dict_node *dn,
int length_limit)
{
length_limit_def_t *lld = malloc(sizeof(*lld));
lld->next = NULL;
lld->length_limit = length_limit;
lld->defexp = dn->exp;
lld->defword = dn->string;
*dict->contable.length_limit_def_next = lld;
dict->contable.length_limit_def_next = &lld->next;
}
static void insert_length_limit(Dictionary dict, Dict_node *dn)
{
int length_limit;
if (0 == strcmp(UNLIMITED_CONNECTORS_WORD, dn->string))
{
length_limit = UNLIMITED_LEN;
}
else if (0 == strncmp(LIMITED_CONNECTORS_WORD, dn->string,
sizeof(LIMITED_CONNECTORS_WORD)-1))
{
char *endp;
length_limit =
(int)strtol(dn->string + sizeof(LIMITED_CONNECTORS_WORD)-1, &endp, 10);
if ((length_limit < 0) || (length_limit > MAX_SENTENCE) ||
(('\0' != *endp) && (SUBSCRIPT_MARK != *endp)))
{
prt_error("Warning: Word \"%s\" found near line %d of \"%s\".\n"
"\tThis word should end with a number (1-%d).\n"
"\tThis word will be ignored.\n",
dn->string, dict->line_number, dict->name, MAX_SENTENCE);
return;
}
}
else return;
add_condesc_length_limit(dict, dn, length_limit);
}
void free_insert_list(Dict_node *ilist)
{
Dict_node * n;
while (ilist != NULL)
{
n = ilist->left;
free(ilist);
ilist = n;
}
}
void insert_list(Dictionary dict, Dict_node * p, int l)
{
Dict_node * dn, *dn_second_half;
int k, i;
if (l == 0) return;
k = (l-1)/2;
dn = p;
for (i = 0; i < k; i++)
{
dn = dn->left;
}
dn_second_half = dn->left;
dn->left = dn->right = NULL;
const char *sm = get_word_subscript(dn->string);
if ((NULL != sm) && ('_' == sm[1]))
{
prt_error("Warning: Word \"%s\" found near line %d of \"%s\".\n"
"\tWords ending \"._\" are reserved for internal use.\n"
"\tThis word will be ignored.\n",
dn->string, dict->line_number, dict->name);
free(dn);
}
else
{
if (contains_underbar(dn->string))
{
insert_idiom(dict, dn);
}
dict->root = dict_node_insert(dict, dict->root, dn);
insert_length_limit(dict, dn);
dict->num_entries++;
}
insert_list(dict, p, k);
insert_list(dict, dn_second_half, l-k-1);
}
static bool read_entry(FileCursor fcurs)
{
Dict_node *dnx, *dn = NULL;
while (!is_equal(fcurs, ':'))
{
if (fcurs->is_special)
{
dict_error(fcurs, "I expected a word but didn\'t get it.");
goto syntax_error;
}
if ((fcurs->token[0] == '/') &&
(fcurs->token[1] != '.') && (get_affix_regex_cg(fcurs->token) < 0))
{
Dict_node *new_dn = read_word_file(fcurs->dict, dn, fcurs->token);
if (new_dn == NULL)
{
prt_error("Error: Cannot open word file \"%s\".\n", fcurs->token);
goto syntax_error;
}
dn = new_dn;
}
else if (0 == strcmp(fcurs->token, "#include"))
{
if (!link_advance(fcurs)) goto syntax_error;
char* dict_name = strdupa(fcurs->token);
size_t skip_slash = ('/' == fcurs->token[0]) ? 1 : 0;
char* instr = get_file_contents(dict_name + skip_slash);
if (NULL == instr)
{
Dictionary dict = fcurs->dict;
prt_error("Error: While parsing dictionary \"%s\":\n"
"\t Line %d: Could not open subdictionary \"%s\"\n",
dict->name, dict->line_number-1, dict_name);
goto syntax_error;
}
Dictionary dict = fcurs->dict;
const char * save_name = dict->name;
int save_line_number = dict->line_number;
dict->name = dict_name;
bool rc = read_dictionary(dict, instr);
dict->name = save_name;
dict->line_number = save_line_number;
free_file_contents(instr);
if (!rc) goto syntax_error;
if (!link_advance(fcurs)) goto syntax_error;
if (';' == fcurs->token[0])
{
if (!link_advance(fcurs)) goto syntax_error;
}
return true;
}
else if (0 == strcmp(fcurs->token, "#define"))
{
if (!link_advance(fcurs)) goto syntax_error;
const char *name = strdupa(fcurs->token);
if (!link_advance(fcurs)) goto syntax_error;
add_define(fcurs->dict, name, fcurs->token);
if (!link_advance(fcurs)) goto syntax_error;
if (!is_equal(fcurs, ';'))
{
dict_error(fcurs, "Expecting \";\" at the end of #define.");
goto syntax_error;
}
}
else
{
Dict_node * dn_new = dict_node_new();
dn_new->left = dn;
dn_new->right = NULL;
dn_new->exp = NULL;
dn = dn_new;
dn->file = NULL;
patch_subscript(fcurs->token);
dn->string = string_set_add(fcurs->token, fcurs->dict->string_set);
}
if (!link_advance(fcurs)) goto syntax_error;
}
if (!link_advance(fcurs))
{
goto syntax_error;
}
Exp * n = make_expression(fcurs);
if (n == NULL)
goto syntax_error;
if (!is_equal(fcurs, ';'))
{
dict_error(fcurs, "Expecting \";\" at the end of an entry.");
goto syntax_error;
}
if (dn == NULL)
{
dict_error(fcurs, "Expecting a token before \":\".");
goto syntax_error;
}
int i = 0;
for (dnx = dn; dnx != NULL; dnx = dnx->left)
{
dnx->exp = n;
i++;
}
Dictionary dict = fcurs->dict;
if (IS_GENERATION(dict))
add_category(dict, n, dn, i);
dict->insert_entry(dict, dn, i);
if (!link_advance(fcurs))
{
return false;
}
return true;
syntax_error:
free_insert_list(dn);
return false;
}
static bool fread_dict(FileCursor fcurs)
{
if (!link_advance(fcurs))
return false;
while ('\0' != fcurs->pin[-1])
{
if (!read_entry(fcurs))
return false;
}
Dictionary dict = fcurs->dict;
if (dict->category != NULL)
{
Exp dummy_exp;
add_category(dict, &dummy_exp, NULL, 0);
dict->category[dict->num_categories + 1].num_words = 0;
}
dict->root = dsw_tree_to_vine(dict->root);
dict->root = dsw_vine_to_tree(dict->root, dict->num_entries);
return true;
}
bool read_dictionary(Dictionary dict, const char * input)
{
FileCursor fcurs = alloca(sizeof(struct FileCursor_s));
dict->line_number = 1;
fcurs->dict = dict;
fcurs->input = input;
fcurs->pin = fcurs->input;
fcurs->recursive_error = false;
fcurs->is_special = false;
fcurs->already_got_it = false;
fcurs->token[0] = 0;
return fread_dict(fcurs);
}