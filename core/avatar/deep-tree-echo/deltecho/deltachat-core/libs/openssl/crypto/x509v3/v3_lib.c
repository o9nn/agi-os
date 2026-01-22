#include <stdio.h>
#include "cryptlib.h"
#include <openssl/conf.h>
#include <openssl/x509v3.h>
#include "ext_dat.h"
static STACK_OF(X509V3_EXT_METHOD) *ext_list = NULL;
static int ext_cmp(const X509V3_EXT_METHOD *const *a,
const X509V3_EXT_METHOD *const *b);
static void ext_list_free(X509V3_EXT_METHOD *ext);
int X509V3_EXT_add(X509V3_EXT_METHOD *ext)
{
if (!ext_list && !(ext_list = sk_X509V3_EXT_METHOD_new(ext_cmp))) {
X509V3err(X509V3_F_X509V3_EXT_ADD, ERR_R_MALLOC_FAILURE);
return 0;
}
if (!sk_X509V3_EXT_METHOD_push(ext_list, ext)) {
X509V3err(X509V3_F_X509V3_EXT_ADD, ERR_R_MALLOC_FAILURE);
return 0;
}
return 1;
}
static int ext_cmp(const X509V3_EXT_METHOD *const *a,
const X509V3_EXT_METHOD *const *b)
{
return ((*a)->ext_nid - (*b)->ext_nid);
}
DECLARE_OBJ_BSEARCH_CMP_FN(const X509V3_EXT_METHOD *,
const X509V3_EXT_METHOD *, ext);
IMPLEMENT_OBJ_BSEARCH_CMP_FN(const X509V3_EXT_METHOD *,
const X509V3_EXT_METHOD *, ext);
const X509V3_EXT_METHOD *X509V3_EXT_get_nid(int nid)
{
X509V3_EXT_METHOD tmp;
const X509V3_EXT_METHOD *t = &tmp, *const *ret;
int idx;
if (nid < 0)
return NULL;
tmp.ext_nid = nid;
ret = OBJ_bsearch_ext(&t, standard_exts, STANDARD_EXTENSION_COUNT);
if (ret)
return *ret;
if (!ext_list)
return NULL;
idx = sk_X509V3_EXT_METHOD_find(ext_list, &tmp);
if (idx == -1)
return NULL;
return sk_X509V3_EXT_METHOD_value(ext_list, idx);
}
const X509V3_EXT_METHOD *X509V3_EXT_get(X509_EXTENSION *ext)
{
int nid;
if ((nid = OBJ_obj2nid(ext->object)) == NID_undef)
return NULL;
return X509V3_EXT_get_nid(nid);
}
int X509V3_EXT_add_list(X509V3_EXT_METHOD *extlist)
{
for (; extlist->ext_nid != -1; extlist++)
if (!X509V3_EXT_add(extlist))
return 0;
return 1;
}
int X509V3_EXT_add_alias(int nid_to, int nid_from)
{
const X509V3_EXT_METHOD *ext;
X509V3_EXT_METHOD *tmpext;
if (!(ext = X509V3_EXT_get_nid(nid_from))) {
X509V3err(X509V3_F_X509V3_EXT_ADD_ALIAS,
X509V3_R_EXTENSION_NOT_FOUND);
return 0;
}
if (!
(tmpext =
(X509V3_EXT_METHOD *)OPENSSL_malloc(sizeof(X509V3_EXT_METHOD)))) {
X509V3err(X509V3_F_X509V3_EXT_ADD_ALIAS, ERR_R_MALLOC_FAILURE);
return 0;
}
*tmpext = *ext;
tmpext->ext_nid = nid_to;
tmpext->ext_flags |= X509V3_EXT_DYNAMIC;
return X509V3_EXT_add(tmpext);
}
void X509V3_EXT_cleanup(void)
{
sk_X509V3_EXT_METHOD_pop_free(ext_list, ext_list_free);
ext_list = NULL;
}
static void ext_list_free(X509V3_EXT_METHOD *ext)
{
if (ext->ext_flags & X509V3_EXT_DYNAMIC)
OPENSSL_free(ext);
}
int X509V3_add_standard_extensions(void)
{
return 1;
}
void *X509V3_EXT_d2i(X509_EXTENSION *ext)
{
const X509V3_EXT_METHOD *method;
const unsigned char *p;
if (!(method = X509V3_EXT_get(ext)))
return NULL;
p = ext->value->data;
if (method->it)
return ASN1_item_d2i(NULL, &p, ext->value->length,
ASN1_ITEM_ptr(method->it));
return method->d2i(NULL, &p, ext->value->length);
}
void *X509V3_get_d2i(STACK_OF(X509_EXTENSION) *x, int nid, int *crit,
int *idx)
{
int lastpos, i;
X509_EXTENSION *ex, *found_ex = NULL;
if (!x) {
if (idx)
*idx = -1;
if (crit)
*crit = -1;
return NULL;
}
if (idx)
lastpos = *idx + 1;
else
lastpos = 0;
if (lastpos < 0)
lastpos = 0;
for (i = lastpos; i < sk_X509_EXTENSION_num(x); i++) {
ex = sk_X509_EXTENSION_value(x, i);
if (OBJ_obj2nid(ex->object) == nid) {
if (idx) {
*idx = i;
found_ex = ex;
break;
} else if (found_ex) {
if (crit)
*crit = -2;
return NULL;
}
found_ex = ex;
}
}
if (found_ex) {
if (crit)
*crit = X509_EXTENSION_get_critical(found_ex);
return X509V3_EXT_d2i(found_ex);
}
if (idx)
*idx = -1;
if (crit)
*crit = -1;
return NULL;
}
int X509V3_add1_i2d(STACK_OF(X509_EXTENSION) **x, int nid, void *value,
int crit, unsigned long flags)
{
int extidx = -1;
int errcode;
X509_EXTENSION *ext, *extmp;
unsigned long ext_op = flags & X509V3_ADD_OP_MASK;
if (ext_op != X509V3_ADD_APPEND)
extidx = X509v3_get_ext_by_NID(*x, nid, -1);
if (extidx >= 0) {
if (ext_op == X509V3_ADD_KEEP_EXISTING)
return 1;
if (ext_op == X509V3_ADD_DEFAULT) {
errcode = X509V3_R_EXTENSION_EXISTS;
goto err;
}
if (ext_op == X509V3_ADD_DELETE) {
if (!sk_X509_EXTENSION_delete(*x, extidx))
return -1;
return 1;
}
} else {
if ((ext_op == X509V3_ADD_REPLACE_EXISTING) ||
(ext_op == X509V3_ADD_DELETE)) {
errcode = X509V3_R_EXTENSION_NOT_FOUND;
goto err;
}
}
ext = X509V3_EXT_i2d(nid, crit, value);
if (!ext) {
X509V3err(X509V3_F_X509V3_ADD1_I2D,
X509V3_R_ERROR_CREATING_EXTENSION);
return 0;
}
if (extidx >= 0) {
extmp = sk_X509_EXTENSION_value(*x, extidx);
X509_EXTENSION_free(extmp);
if (!sk_X509_EXTENSION_set(*x, extidx, ext))
return -1;
return 1;
}
if (!*x && !(*x = sk_X509_EXTENSION_new_null()))
return -1;
if (!sk_X509_EXTENSION_push(*x, ext))
return -1;
return 1;
err:
if (!(flags & X509V3_ADD_SILENT))
X509V3err(X509V3_F_X509V3_ADD1_I2D, errcode);
return 0;
}
IMPLEMENT_STACK_OF(X509V3_EXT_METHOD)