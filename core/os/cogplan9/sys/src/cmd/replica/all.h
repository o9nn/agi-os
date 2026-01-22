#include <u.h>
#include <libc.h>
#include <bio.h>
#include <disk.h>
typedef struct Avl Avl;
typedef struct Avltree Avltree;
typedef struct Avlwalk Avlwalk;
#pragma incomplete Avltree
#pragma incomplete Avlwalk
struct Avl
{
Avl *p;
Avl *n[2];
int bal;
};
Avltree *mkavltree(int(*cmp)(Avl*, Avl*));
void insertavl(Avltree *tree, Avl *new, Avl **oldp);
Avl *lookupavl(Avltree *tree, Avl *key);
void deleteavl(Avltree *tree, Avl *key, Avl **oldp);
Avlwalk *avlwalk(Avltree *tree);
Avl *avlnext(Avlwalk *walk);
Avl	*avlprev(Avlwalk *walk);
void endwalk(Avlwalk *walk);
typedef struct Db Db;
typedef struct Entry Entry;
struct Entry
{
Avl a;
char *name;
struct {
char *name;
char *uid;
char *gid;
ulong mtime;
ulong mode;
int mark;
vlong length;
} d;
};
typedef struct Db Db;
struct Db
{
Avltree *avl;
int fd;
};
Db *opendb(char*);
int finddb(Db*, char*, Dir*);
void removedb(Db*, char*);
void insertdb(Db*, char*, Dir*);
int markdb(Db*, char*, Dir*);
void *erealloc(void*, int);
void *emalloc(int);
char *estrdup(char*);
char *atom(char*);
char *unroot(char*, char*);
int revrdproto(char*, char*, char*, Protoenum*, Protowarn*, void*);