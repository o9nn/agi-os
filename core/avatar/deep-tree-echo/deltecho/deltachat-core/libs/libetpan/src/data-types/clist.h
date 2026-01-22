#ifndef CLIST_H
#define CLIST_H
#ifndef LIBETPAN_CONFIG_H
# include <libetpan/libetpan-config.h>
#endif
#ifdef __cplusplus
extern "C" {
#endif
typedef struct clistcell_s {
void * data;
struct clistcell_s * previous;
struct clistcell_s * next;
} clistcell;
struct clist_s {
clistcell * first;
clistcell * last;
int count;
};
typedef struct clist_s clist;
typedef clistcell clistiter;
LIBETPAN_EXPORT
clist * clist_new(void);
LIBETPAN_EXPORT
void clist_free(clist *);
#ifdef NO_MACROS
int clist_isempty(clist *);
int clist_count(clist *);
clistiter * clist_begin(clist *);
clistiter * clist_end(clist *);
clistiter * clist_next(clistiter *);
clistiter * clist_previous(clistiter *);
void* clist_content(clistiter *);
int clist_prepend(clist *, void *);
int clist_append(clist *, void *);
#else
#define clist_isempty(lst) (((lst)->first==(lst)->last) && ((lst)->last==NULL))
#define clist_count(lst) ((lst)->count)
#define clist_begin(lst) ((lst)->first)
#define clist_end(lst) ((lst)->last)
#define clist_next(iter) (iter ? (iter)->next : NULL)
#define clist_previous(iter) (iter ? (iter)->previous : NULL)
#define clist_content(iter) (iter ? (iter)->data : NULL)
#define clist_prepend(lst, data) (clist_insert_before(lst, (lst)->first, data))
#define clist_append(lst, data) (clist_insert_after(lst, (lst)->last, data))
#endif
LIBETPAN_EXPORT
int clist_insert_before(clist *, clistiter *, void *);
LIBETPAN_EXPORT
int clist_insert_after(clist *, clistiter *, void *);
LIBETPAN_EXPORT
clistiter * clist_delete(clist *, clistiter *);
typedef void (* clist_func)(void *, void *);
LIBETPAN_EXPORT
void clist_foreach(clist * lst, clist_func func, void * data);
LIBETPAN_EXPORT
void clist_concat(clist * dest, clist * src);
LIBETPAN_EXPORT
void * clist_nth_data(clist * lst, int indx);
LIBETPAN_EXPORT
clistiter * clist_nth(clist * lst, int indx);
#ifdef __cplusplus
}
#endif
#endif