#include <u.h>
#include <libc.h>
#include <bio.h>
#include <ctype.h>
#include <ndb.h>
#include "ndbhf.h"
static Ndb*	doopen(char*);
static void	hffree(Ndb*);
static char *deffile = "/lib/ndb/local";
Ndb*
ndbopen(char *file)
{
Ndb *db, *first, *last;
Ndbs s;
Ndbtuple *t, *nt;
if(file == nil && (file = getenv("NDBFILE")) == nil)
file = deffile;
db = doopen(file);
if(db == 0)
return 0;
first = last = db;
t = ndbsearch(db, &s, "database", "");
Bseek(&db->b, 0, 0);
if(t == 0)
return db;
for(nt = t; nt; nt = nt->entry){
if(strcmp(nt->attr, "file") != 0)
continue;
if(strcmp(nt->val, file) == 0){
if(first->next == 0)
continue;
if(strcmp(first->file, file) == 0){
db = first;
first = first->next;
last->next = db;
db->next = 0;
last = db;
}
continue;
}
db = doopen(nt->val);
if(db == 0)
continue;
last->next = db;
last = db;
}
ndbfree(t);
return first;
}
static Ndb*
doopen(char *file)
{
Ndb *db;
db = (Ndb*)malloc(sizeof(Ndb));
if(db == 0)
return 0;
memset(db, 0, sizeof(Ndb));
strncpy(db->file, file, sizeof(db->file)-1);
if(ndbreopen(db) < 0){
free(db);
return 0;
}
return db;
}
int
ndbreopen(Ndb *db)
{
int fd;
Dir *d;
if(db->mtime){
_ndbcacheflush(db);
hffree(db);
close(Bfildes(&db->b));
Bterm(&db->b);
db->mtime = 0;
}
fd = open(db->file, OREAD);
if(fd < 0)
return -1;
d = dirfstat(fd);
if(d == nil){
close(fd);
return -1;
}
db->qid = d->qid;
db->mtime = d->mtime;
db->length = d->length;
Binits(&db->b, fd, OREAD, db->buf, sizeof(db->buf));
free(d);
return 0;
}
void
ndbclose(Ndb *db)
{
Ndb *nextdb;
for(; db; db = nextdb){
nextdb = db->next;
_ndbcacheflush(db);
hffree(db);
close(Bfildes(&db->b));
Bterm(&db->b);
free(db);
}
}
static void
hffree(Ndb *db)
{
Ndbhf *hf, *next;
for(hf = db->hf; hf; hf = next){
next = hf->next;
close(hf->fd);
free(hf);
}
db->hf = 0;
}
int
ndbchanged(Ndb *db)
{
Ndb *ndb;
Dir *d;
for(ndb = db; ndb != nil; ndb = ndb->next){
d = dirfstat(Bfildes(&ndb->b));
if(d == nil)
continue;
if(ndb->qid.path != d->qid.path
|| ndb->qid.vers != d->qid.vers){
free(d);
return 1;
}
free(d);
}
return 0;
}