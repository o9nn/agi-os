#include "priv.h"
struct aux
{
void (*free_el)(void *);
unsigned refs;
};
struct port_class **trivfs_dynamic_protid_port_classes = 0;
size_t trivfs_num_dynamic_protid_port_classes = 0;
static struct aux *dynamic_protid_port_classes_aux = 0;
static size_t dynamic_protid_port_classes_sz = 0;
struct port_class **trivfs_dynamic_control_port_classes = 0;
size_t trivfs_num_dynamic_control_port_classes = 0;
static struct aux *dynamic_control_port_classes_aux = 0;
static size_t dynamic_control_port_classes_sz = 0;
struct port_bucket **trivfs_dynamic_port_buckets = 0;
size_t trivfs_num_dynamic_port_buckets = 0;
static struct aux *dynamic_port_buckets_aux = 0;
static size_t dynamic_port_buckets_sz = 0;
static pthread_mutex_t dyn_lock = PTHREAD_MUTEX_INITIALIZER;
static error_t
add_el (void *el, void (*free_el)(void *),
void *vec_v, struct aux **aux_vec,
size_t *sz, size_t *num)
{
int i;
size_t new_sz;
void ***vec, **new_vec;
struct aux *new_aux_vec;
if (! el)
return ENOMEM;
pthread_mutex_lock (&dyn_lock);
vec = vec_v;
for (i = 0; i < *sz; i++)
if (! (*vec)[i])
{
(*vec)[i] = el;
(*aux_vec)[i].free_el = free_el;
(*aux_vec)[i].refs = 1;
(*num)++;
pthread_mutex_unlock (&dyn_lock);
return 0;
}
else if ((*vec)[i] == el)
{
(*aux_vec)[i].refs++;
pthread_mutex_unlock (&dyn_lock);
return 0;
}
new_sz = *sz + 4;
new_vec = realloc (*vec, new_sz * sizeof (void *));
new_aux_vec = realloc (*aux_vec, new_sz * sizeof (struct aux));
if (!new_vec || !new_aux_vec)
{
if (free_el)
(*free_el) (el);
free (new_vec);
free (new_aux_vec);
return ENOMEM;
}
for (i = *sz; i < new_sz; i++)
new_vec[i] = 0;
new_vec[*sz] = el;
new_aux_vec[*sz].free_el = free_el;
new_aux_vec[*sz].refs = 1;
(*num)++;
*vec = new_vec;
*aux_vec = new_aux_vec;
*sz = new_sz;
pthread_mutex_unlock (&dyn_lock);
return 0;
}
static void
drop_el (void *el, void *vec_v, struct aux *aux_vec,
size_t sz, size_t *num)
{
int i;
void **vec;
if (! el)
return;
pthread_mutex_lock (&dyn_lock);
vec = vec_v;
for (i = 0; i < sz; i++)
if (vec[i] == el)
{
if (aux_vec[i].refs == 1)
{
if (aux_vec[i].free_el)
(*aux_vec[i].free_el) (el);
vec[i] = 0;
(*num)--;
}
else
aux_vec[i].refs--;
break;
}
pthread_mutex_unlock (&dyn_lock);
}
error_t
trivfs_add_control_port_class (struct port_class **class)
{
if (! *class)
{
*class = ports_create_class (trivfs_clean_cntl, 0);
if (! *class)
return ENOMEM;
}
return
add_el (*class, 0,
&trivfs_dynamic_control_port_classes,
&dynamic_control_port_classes_aux,
&dynamic_control_port_classes_sz,
&trivfs_num_dynamic_control_port_classes);
}
void
trivfs_remove_control_port_class (struct port_class *class)
{
drop_el (class,
trivfs_dynamic_control_port_classes,
dynamic_control_port_classes_aux,
dynamic_control_port_classes_sz,
&trivfs_num_dynamic_control_port_classes);
}
error_t
trivfs_add_protid_port_class (struct port_class **class)
{
if (! *class)
{
*class = ports_create_class (trivfs_clean_protid, 0);
if (! *class)
return ENOMEM;
}
return
add_el (*class, 0,
&trivfs_dynamic_protid_port_classes,
&dynamic_protid_port_classes_aux,
&dynamic_protid_port_classes_sz,
&trivfs_num_dynamic_protid_port_classes);
}
void
trivfs_remove_protid_port_class (struct port_class *class)
{
drop_el (class,
trivfs_dynamic_protid_port_classes,
dynamic_protid_port_classes_aux,
dynamic_protid_port_classes_sz,
&trivfs_num_dynamic_protid_port_classes);
}
error_t
trivfs_add_port_bucket (struct port_bucket **bucket)
{
if (! *bucket)
{
*bucket = ports_create_bucket ();
if (! *bucket)
return ENOMEM;
}
return
add_el (*bucket, 0,
&trivfs_dynamic_port_buckets,
&dynamic_port_buckets_aux,
&dynamic_port_buckets_sz,
&trivfs_num_dynamic_port_buckets);
}
void
trivfs_remove_port_bucket (struct port_bucket *bucket)
{
drop_el (bucket,
trivfs_dynamic_port_buckets,
dynamic_port_buckets_aux,
dynamic_port_buckets_sz,
&trivfs_num_dynamic_port_buckets);
}