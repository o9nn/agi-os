#include <stdio.h>
#include "cryptlib.h"
#include <openssl/bn.h>
#include <openssl/evp.h>
#include <openssl/asn1.h>
#include <openssl/asn1t.h>
#include <openssl/x509.h>
#include <openssl/objects.h>
#include <openssl/buffer.h>
#include <openssl/pem.h>
X509_REQ *X509_to_X509_REQ(X509 *x, EVP_PKEY *pkey, const EVP_MD *md)
{
X509_REQ *ret;
X509_REQ_INFO *ri;
int i;
EVP_PKEY *pktmp;
ret = X509_REQ_new();
if (ret == NULL) {
X509err(X509_F_X509_TO_X509_REQ, ERR_R_MALLOC_FAILURE);
goto err;
}
ri = ret->req_info;
ri->version->length = 1;
ri->version->data = (unsigned char *)OPENSSL_malloc(1);
if (ri->version->data == NULL)
goto err;
ri->version->data[0] = 0;
if (!X509_REQ_set_subject_name(ret, X509_get_subject_name(x)))
goto err;
pktmp = X509_get_pubkey(x);
if (pktmp == NULL)
goto err;
i = X509_REQ_set_pubkey(ret, pktmp);
EVP_PKEY_free(pktmp);
if (!i)
goto err;
if (pkey != NULL) {
if (!X509_REQ_sign(ret, pkey, md))
goto err;
}
return (ret);
err:
X509_REQ_free(ret);
return (NULL);
}
EVP_PKEY *X509_REQ_get_pubkey(X509_REQ *req)
{
if ((req == NULL) || (req->req_info == NULL))
return (NULL);
return (X509_PUBKEY_get(req->req_info->pubkey));
}
int X509_REQ_check_private_key(X509_REQ *x, EVP_PKEY *k)
{
EVP_PKEY *xk = NULL;
int ok = 0;
xk = X509_REQ_get_pubkey(x);
switch (EVP_PKEY_cmp(xk, k)) {
case 1:
ok = 1;
break;
case 0:
X509err(X509_F_X509_REQ_CHECK_PRIVATE_KEY,
X509_R_KEY_VALUES_MISMATCH);
break;
case -1:
X509err(X509_F_X509_REQ_CHECK_PRIVATE_KEY, X509_R_KEY_TYPE_MISMATCH);
break;
case -2:
#ifndef OPENSSL_NO_EC
if (k->type == EVP_PKEY_EC) {
X509err(X509_F_X509_REQ_CHECK_PRIVATE_KEY, ERR_R_EC_LIB);
break;
}
#endif
#ifndef OPENSSL_NO_DH
if (k->type == EVP_PKEY_DH) {
X509err(X509_F_X509_REQ_CHECK_PRIVATE_KEY,
X509_R_CANT_CHECK_DH_KEY);
break;
}
#endif
X509err(X509_F_X509_REQ_CHECK_PRIVATE_KEY, X509_R_UNKNOWN_KEY_TYPE);
}
EVP_PKEY_free(xk);
return (ok);
}
static int ext_nid_list[] = { NID_ext_req, NID_ms_ext_req, NID_undef };
static int *ext_nids = ext_nid_list;
int X509_REQ_extension_nid(int req_nid)
{
int i, nid;
for (i = 0;; i++) {
nid = ext_nids[i];
if (nid == NID_undef)
return 0;
else if (req_nid == nid)
return 1;
}
}
int *X509_REQ_get_extension_nids(void)
{
return ext_nids;
}
void X509_REQ_set_extension_nids(int *nids)
{
ext_nids = nids;
}
STACK_OF(X509_EXTENSION) *X509_REQ_get_extensions(X509_REQ *req)
{
X509_ATTRIBUTE *attr;
ASN1_TYPE *ext = NULL;
int idx, *pnid;
const unsigned char *p;
if ((req == NULL) || (req->req_info == NULL) || !ext_nids)
return (NULL);
for (pnid = ext_nids; *pnid != NID_undef; pnid++) {
idx = X509_REQ_get_attr_by_NID(req, *pnid, -1);
if (idx == -1)
continue;
attr = X509_REQ_get_attr(req, idx);
if (attr->single)
ext = attr->value.single;
else if (sk_ASN1_TYPE_num(attr->value.set))
ext = sk_ASN1_TYPE_value(attr->value.set, 0);
break;
}
if (!ext || (ext->type != V_ASN1_SEQUENCE))
return NULL;
p = ext->value.sequence->data;
return (STACK_OF(X509_EXTENSION) *)
ASN1_item_d2i(NULL, &p, ext->value.sequence->length,
ASN1_ITEM_rptr(X509_EXTENSIONS));
}
int X509_REQ_add_extensions_nid(X509_REQ *req, STACK_OF(X509_EXTENSION) *exts,
int nid)
{
ASN1_TYPE *at = NULL;
X509_ATTRIBUTE *attr = NULL;
if (!(at = ASN1_TYPE_new()) || !(at->value.sequence = ASN1_STRING_new()))
goto err;
at->type = V_ASN1_SEQUENCE;
at->value.sequence->length =
ASN1_item_i2d((ASN1_VALUE *)exts,
&at->value.sequence->data,
ASN1_ITEM_rptr(X509_EXTENSIONS));
if (!(attr = X509_ATTRIBUTE_new()))
goto err;
if (!(attr->value.set = sk_ASN1_TYPE_new_null()))
goto err;
if (!sk_ASN1_TYPE_push(attr->value.set, at))
goto err;
at = NULL;
attr->single = 0;
attr->object = OBJ_nid2obj(nid);
if (!req->req_info->attributes) {
if (!(req->req_info->attributes = sk_X509_ATTRIBUTE_new_null()))
goto err;
}
if (!sk_X509_ATTRIBUTE_push(req->req_info->attributes, attr))
goto err;
return 1;
err:
X509_ATTRIBUTE_free(attr);
ASN1_TYPE_free(at);
return 0;
}
int X509_REQ_add_extensions(X509_REQ *req, STACK_OF(X509_EXTENSION) *exts)
{
return X509_REQ_add_extensions_nid(req, exts, NID_ext_req);
}
int X509_REQ_get_attr_count(const X509_REQ *req)
{
return X509at_get_attr_count(req->req_info->attributes);
}
int X509_REQ_get_attr_by_NID(const X509_REQ *req, int nid, int lastpos)
{
return X509at_get_attr_by_NID(req->req_info->attributes, nid, lastpos);
}
int X509_REQ_get_attr_by_OBJ(const X509_REQ *req, ASN1_OBJECT *obj,
int lastpos)
{
return X509at_get_attr_by_OBJ(req->req_info->attributes, obj, lastpos);
}
X509_ATTRIBUTE *X509_REQ_get_attr(const X509_REQ *req, int loc)
{
return X509at_get_attr(req->req_info->attributes, loc);
}
X509_ATTRIBUTE *X509_REQ_delete_attr(X509_REQ *req, int loc)
{
return X509at_delete_attr(req->req_info->attributes, loc);
}
int X509_REQ_add1_attr(X509_REQ *req, X509_ATTRIBUTE *attr)
{
if (X509at_add1_attr(&req->req_info->attributes, attr))
return 1;
return 0;
}
int X509_REQ_add1_attr_by_OBJ(X509_REQ *req,
const ASN1_OBJECT *obj, int type,
const unsigned char *bytes, int len)
{
if (X509at_add1_attr_by_OBJ(&req->req_info->attributes, obj,
type, bytes, len))
return 1;
return 0;
}
int X509_REQ_add1_attr_by_NID(X509_REQ *req,
int nid, int type,
const unsigned char *bytes, int len)
{
if (X509at_add1_attr_by_NID(&req->req_info->attributes, nid,
type, bytes, len))
return 1;
return 0;
}
int X509_REQ_add1_attr_by_txt(X509_REQ *req,
const char *attrname, int type,
const unsigned char *bytes, int len)
{
if (X509at_add1_attr_by_txt(&req->req_info->attributes, attrname,
type, bytes, len))
return 1;
return 0;
}