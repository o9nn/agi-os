#ifndef _GS2_TOKEN_H_
#define _GS2_TOKEN_H_ 1
#include <config.h>
#include <gssapi/gssapi.h>
#ifndef KRB5_HEIMDAL
#ifdef HAVE_GSSAPI_GSSAPI_EXT_H
#include <gssapi/gssapi_ext.h>
#endif
#endif
#ifndef HAVE_GSS_DECAPSULATE_TOKEN
OM_uint32
gs2_decapsulate_token(const gss_buffer_t input_token,
const gss_OID token_oid,
gss_buffer_t output_token);
#define gss_decapsulate_token gs2_decapsulate_token
#endif
#ifndef HAVE_GSS_ENCAPSULATE_TOKEN
OM_uint32
gs2_encapsulate_token(const gss_buffer_t input_token,
const gss_OID token_oid,
gss_buffer_t output_token);
#define gss_encapsulate_token gs2_encapsulate_token
#endif
#ifndef HAVE_GSS_OID_EQUAL
int
gs2_oid_equal(const gss_OID o1, const gss_OID o2);
#define gss_oid_equal gs2_oid_equal
#endif
#endif