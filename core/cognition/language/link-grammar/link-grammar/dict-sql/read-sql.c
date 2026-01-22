#ifdef HAVE_SQLITE3
#define D_SQL 5
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#if HAVE_THREADS_H
#include <threads.h>
#endif
#include <sqlite3.h>
#include "api-structures.h"
#include "connectors.h"
#include "dict-common/dict-affix-impl.h"
#include "dict-common/dict-api.h"
#include "dict-common/dict-common.h"
#include "dict-common/dict-internals.h"
#include "dict-common/dict-locale.h"
#include "dict-common/dict-structures.h"
#include "dict-common/dict-utils.h"
#include "dict-common/file-utils.h"
#include "dict-file/read-dict.h"
#include "error.h"
#include "externs.h"
#include "memory-pool.h"
#include "string-set.h"
#include "tokenize/spellcheck.h"
#include "utilities.h"
#include "read-sql.h"
static const char * make_expression(Dictionary dict,
const char *exp_str, Exp** pex)
{
*pex = NULL;
Exp_type etype = CONNECTOR_type;
const char * p = exp_str;
while (*p && (lg_isspace((unsigned char)*p))) p++;
if (0 == *p) return p;
if ('(' == *p)
{
p = make_expression(dict, ++p, pex);
}
else
{
const char * con_start = p;
while (*p && (isalnum((unsigned char)*p) || '*' == *p)) p++;
assert (('+' == *p) || ('-' == *p),
"Missing direction character in connector string: %s", con_start);
char * constr = NULL;
bool multi = false;
if ('@' == *con_start)
{
constr = strndupa(con_start+1, p-con_start-1);
multi = true;
}
else
constr = strndupa(con_start, p-con_start);
Exp* e = make_connector_node(dict, dict->Exp_pool, constr, *p, multi);
*pex = e;
}
p++;
while (*p && (lg_isspace((unsigned char)*p))) p++;
if (')' == *p || 0 == *p)
{
return p;
}
if ('&' == *p)
{
etype = AND_type; p++;
}
else if ('o' == *p && 'r' == *(p+1))
{
etype = OR_type; p+=2;
}
else
{
assert(false, "Bad rest of expression %s", exp_str);
}
Exp* rest = NULL;
p = make_expression(dict, p, &rest);
assert(NULL != rest, "Badly formed expression %s", exp_str);
Exp* join = make_join_node(dict->Exp_pool, *pex, rest, etype);
*pex = join;
return p;
}
#if HAVE_THREADS_H
static mtx_t global_mutex;
#endif
typedef struct
{
Dictionary dict;
Dict_node* dn;
bool found;
int count;
Exp* exp;
char* classname;
} cbdata;
static int exp_cb(void *user_data, int argc, char **argv, char **colName)
{
cbdata* bs = user_data;
Dictionary dict = bs->dict;
assert(2 == argc, "Bad column count");
assert(argv[0], "NULL column value");
Exp* exp = NULL;
make_expression(dict, argv[0], &exp);
assert(NULL != exp, "Failed expression %s", argv[0]);
if (!strtofC(argv[1], &exp->cost))
{
prt_error("Warning: Invalid cost \"%s\" in expression \"%s\" "
"(using 1.0)\n", argv[1], argv[0]);
exp->cost = 1.0;
}
if (NULL == bs->exp)
{
bs->exp = exp;
return 0;
}
if (OR_type != bs->exp->type)
{
bs->exp = make_or_node(dict->Exp_pool, exp, bs->exp);
return 0;
}
exp->operand_next = bs->exp->operand_first;
bs->exp->operand_first = exp;
return 0;
}
static char * escape_quotes(const char * s)
{
char * q = strchr(s, '\'');
if (NULL == q) return (char *) s;
if ('\'' == *(q+1)) return (char *) s;
char * es = malloc(2 * strlen(s) + 1);
char * p = es;
while (q)
{
strncpy(p, s, q-s+1);
p += q-s+1;
*p = '\'';
p++;
s = q+1;
q = strchr(s, '\'');
}
strcpy(p, s);
return es;
}
static void
db_lookup_exp(Dictionary dict, const char *s, cbdata* bs)
{
#if HAVE_THREADS_H
mtx_lock(&global_mutex);
#endif
sqlite3 *db = dict->db_handle;
dyn_str *qry;
char * es = escape_quotes(s);
qry = dyn_str_new();
dyn_strcat(qry, "SELECT disjunct, cost FROM Disjuncts WHERE classname = \'");
dyn_strcat(qry, es);
dyn_strcat(qry, "\';");
sqlite3_exec(db, qry->str, exp_cb, bs, NULL);
dyn_str_delete(qry);
if (es != s) free(es);
lgdebug(D_SQL+1, "Found expression for class %s: %s\n",
s, exp_stringify(bs->exp));
#if HAVE_THREADS_H
mtx_unlock(&global_mutex);
#endif
}
static int exists_cb(void *user_data, int argc, char **argv, char **colName)
{
cbdata* bs = user_data;
assert(2 == argc, "Bad column count");
assert(argv[0], "NULL column value");
bs->found = true;
return 0;
}
static int morph_cb(void *user_data, int argc, char **argv, char **colName)
{
assert(2 == argc, "Bad column count");
assert(argv[0], "NULL column value");
char * scriword = argv[0];
char * wclass = argv[1];
cbdata* bs = user_data;
bs->exp = NULL;
db_lookup_exp(bs->dict, wclass, bs);
assert(NULL != bs->exp, "Missing disjuncts for word %s %s",
scriword, wclass);
#if HAVE_THREADS_H
mtx_lock(&global_mutex);
#endif
Dict_node *dn = dict_node_new();
dn->string = string_set_add(scriword, bs->dict->string_set);
dn->right = bs->dn;
dn->exp = bs->exp;
bs->dn = dn;
#if HAVE_THREADS_H
mtx_unlock(&global_mutex);
#endif
return 0;
}
static void
db_lookup_common(Dictionary dict, const char *s, const char *equals,
int (*cb)(void *, int, char **, char **),
cbdata* bs)
{
sqlite3 *db = dict->db_handle;
dyn_str *qry;
char * es = escape_quotes(s);
qry = dyn_str_new();
dyn_strcat(qry, "SELECT subscript, classname FROM Morphemes WHERE morpheme ");
dyn_strcat(qry, equals);
dyn_strcat(qry, " \'");
dyn_strcat(qry, es);
dyn_strcat(qry, "\';");
if (es != s) free(es);
sqlite3_exec(db, qry->str, cb, bs, NULL);
dyn_str_delete(qry);
}
static bool db_lookup(Dictionary dict, const char *s)
{
cbdata bs;
bs.dict = dict;
bs.found = false;
db_lookup_common(dict, s, "=", exists_cb, &bs);
return bs.found;
}
static Dict_node * db_lookup_list(Dictionary dict, const char *s)
{
cbdata bs;
bs.dict = dict;
bs.dn = NULL;
db_lookup_common(dict, s, "=", morph_cb, &bs);
if (verbosity_level(D_SQL))
{
if (bs.dn)
{
printf("Found expression for word %s: %s\n",
s, exp_stringify(bs.dn->exp));
}
else
{
printf("No expression for word %s\n", s);
}
}
return bs.dn;
}
static Dict_node * db_lookup_wild(Dictionary dict, const char *s)
{
cbdata bs;
bs.dict = dict;
bs.dn = NULL;
db_lookup_common(dict, s, "GLOB", morph_cb, &bs);
if (verbosity_level(D_SQL))
{
if (bs.dn)
{
printf("Found expression for glob %s: %s\n",
s, exp_stringify(bs.dn->exp));
}
else
{
printf("No expression for glob %s\n", s);
}
}
return bs.dn;
}
static int count_cb(void *user_data, int argc, char **argv, char **colName)
{
cbdata* bs = user_data;
assert(1 == argc, "Bad column count");
bs->count = atol(argv[0]);
return 0;
}
static int classname_cb(void *user_data, int argc, char **argv, char **colName)
{
cbdata* bs = user_data;
Dictionary dict = bs->dict;
if (!dict->generate_walls && is_wall(argv[0])) return 0;
if (is_macro(argv[0])) return 0;
dict->num_categories++;
dict->category[dict->num_categories].num_words = 0;
dict->category[dict->num_categories].word = NULL;
char* esc = escape_quotes(argv[0]);
dict->category[dict->num_categories].name =
string_set_add(esc, dict->string_set);
if (esc != argv[0]) free(esc);
char category_string[16];
snprintf(category_string, sizeof(category_string), " %x",
dict->num_categories);
string_set_add(category_string, dict->string_set);
return 0;
}
static int classword_cb(void *user_data, int argc, char **argv, char **colName)
{
cbdata* bs = user_data;
Dictionary dict = bs->dict;
char *word = strdupa(argv[0]);
patch_subscript(word);
dict->category[dict->num_categories].word[bs->count] =
string_set_add(word, dict->string_set);
bs->count++;
return 0;
}
static void db_add_categories(Dictionary dict)
{
sqlite3 *db = dict->db_handle;
cbdata bs;
bs.dict = dict;
#if HAVE_THREADS_H
mtx_lock(&global_mutex);
#endif
sqlite3_exec(db, "SELECT count(DISTINCT classname) FROM Disjuncts;",
count_cb, &bs, NULL);
dict->num_categories = 0;
dict->num_categories_alloced = 1 + bs.count + 1;
dict->category = malloc(dict->num_categories_alloced *
sizeof(*dict->category));
sqlite3_exec(db, "SELECT DISTINCT classname FROM Disjuncts;",
classname_cb, &bs, NULL);
unsigned int ncat = dict->num_categories;
for (unsigned int i=1; i<=ncat; i++)
{
dyn_str *qry = dyn_str_new();
dyn_strcat(qry,
"SELECT disjunct, cost FROM Disjuncts WHERE classname = \'");
dyn_strcat(qry, dict->category[i].name);
dyn_strcat(qry, "\';");
bs.exp = NULL;
sqlite3_exec(db, qry->str, exp_cb, &bs, NULL);
dyn_str_delete(qry);
dict->category[i].exp = bs.exp;
qry = dyn_str_new();
dyn_strcat(qry,
"SELECT count(*) FROM Morphemes WHERE classname = \'");
dyn_strcat(qry, dict->category[i].name);
dyn_strcat(qry, "\';");
sqlite3_exec(db, qry->str, count_cb, &bs, NULL);
dyn_str_delete(qry);
dict->category[i].num_words = bs.count;
dict->category[i].word =
malloc(bs.count * sizeof(*dict->category[0].word));
qry = dyn_str_new();
dyn_strcat(qry,
"SELECT subscript FROM Morphemes WHERE classname = \'");
dyn_strcat(qry, dict->category[i].name);
dyn_strcat(qry, "\';");
dict->num_categories = i;
bs.count = 0;
sqlite3_exec(db, qry->str, classword_cb, &bs, NULL);
dyn_str_delete(qry);
}
dict->category[dict->num_categories + 1].num_words = 0;
#if HAVE_THREADS_H
mtx_unlock(&global_mutex);
#endif
}
static void* db_open(const char * fullname, const void * user_data)
{
#if HAVE_THREADS_H
mtx_init(&global_mutex, mtx_plain);
#endif
FILE * fh =  fopen(fullname, "r");
if (NULL == fh)
return NULL;
struct stat buf;
int fd = fileno(fh);
fstat(fd, &buf);
fclose(fh);
if (0 == buf.st_size)
return NULL;
sqlite3 *db;
if (sqlite3_open(fullname, &db))
{
prt_error("Error: Can't open database %s: %s\n",
fullname, sqlite3_errmsg(db));
sqlite3_close(db);
return NULL;
}
return (void *) db;
}
static void db_close(Dictionary dict)
{
sqlite3 *db = dict->db_handle;
if (db)
sqlite3_close(db);
dict->db_handle = NULL;
}
static void db_start_lookup(Dictionary dict, Sentence sent)
{
}
static void db_end_lookup(Dictionary dict, Sentence sent)
{
#if HAVE_THREADS_H
mtx_lock(&global_mutex);
#endif
condesc_setup(dict);
#if HAVE_THREADS_H
mtx_unlock(&global_mutex);
#endif
}
Dictionary dictionary_create_from_db(const char *lang)
{
char *dbname;
const char * t;
Dictionary dict;
dict = (Dictionary) malloc(sizeof(struct Dictionary_s));
memset(dict, 0, sizeof(struct Dictionary_s));
dict->string_set = string_set_create();
t = strrchr (lang, '/');
t = (NULL == t) ? lang : t+1;
dict->lang = string_set_add(t, dict->string_set);
lgdebug(D_USER_FILES, "Debug: Language: %s\n", dict->lang);
#if 0
dict->spell_checker = spellcheck_create(dict->lang);
#endif
#if defined HAVE_HUNSPELL || defined HAVE_ASPELL
if (verbosity_level(D_USER_BASIC) && (NULL == dict->spell_checker))
prt_error("Info: %s: Spell checker disabled.\n", dict->lang);
#endif
dict->base_knowledge = NULL;
dict->hpsg_knowledge = NULL;
dbname = join_path (lang, "dict.db");
dict->name = string_set_add(dbname, dict->string_set);
free(dbname);
dict->db_handle = object_open(dict->name, db_open, NULL);
dict->lookup_list = db_lookup_list;
dict->lookup_wild = db_lookup_wild;
dict->free_lookup = dict_node_free_lookup;
dict->exists_lookup = db_lookup;
dict->start_lookup = db_start_lookup;
dict->end_lookup = db_end_lookup;
dict->clear_cache = dict_node_noop;
dict->close = db_close;
dict->dynamic_lookup = true;
condesc_init(dict, 1<<8);
dict->dfine.set = string_id_create();
dict->Exp_pool = pool_new(__func__, "Exp", 4096,
sizeof(Exp), false,
false, false);
char *affix_name = join_path (lang, "4.0.affix");
dict->affix_table = dictionary_six(lang, affix_name, NULL, NULL, NULL, NULL);
if (dict->affix_table == NULL)
{
prt_error("Error: Could not open affix file %s\n", affix_name);
free(affix_name);
goto failure;
}
free(affix_name);
if (!afdict_init(dict))
goto failure;
if (!dictionary_setup_defines(dict))
goto failure;
if (dictionary_generation_request(dict))
db_add_categories(dict);
return dict;
failure:
dictionary_delete(dict);
return NULL;
}
#endif