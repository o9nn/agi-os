#include <config.h>
#include <string.h>
#include <stdlib.h>
#include "gs2_token.h"
#ifndef HAVE_GSS_ENCAPSULATE_TOKEN
static size_t
der_length_size(size_t length)
{
if (length < (1<<7))
return 1;
else if (length < (1<<8))
return 2;
#if INT_MAX == 0x7fff
else
return 3;
#else
else if (length < (1<<16))
return 3;
else if (length < (1<<24))
return 4;
else
return 5;
#endif
}
static void
der_write_length(unsigned char **buf, size_t length)
{
if (length < (1<<7)) {
*(*buf)++ = (unsigned char)length;
} else {
*(*buf)++ = (unsigned char)(der_length_size(length)+127);
#if INT_MAX > 0x7fff
if (length >= (1<<24))
*(*buf)++ = (unsigned char)(length>>24);
if (length >= (1<<16))
*(*buf)++ = (unsigned char)((length>>16)&0xff);
#endif
if (length >= (1<<8))
*(*buf)++ = (unsigned char)((length>>8)&0xff);
*(*buf)++ = (unsigned char)(length&0xff);
}
}
static size_t
token_size(const gss_OID_desc *mech, size_t body_size)
{
body_size += 2 + (size_t) mech->length;
return 1 + der_length_size(body_size) + body_size;
}
static void
make_token_header(
const gss_OID_desc *mech,
size_t body_size,
unsigned char **buf)
{
*(*buf)++ = 0x60;
der_write_length(buf, 2 + mech->length + body_size);
*(*buf)++ = 0x06;
*(*buf)++ = (unsigned char)mech->length;
memcpy(*buf, mech->elements, mech->length);
*buf += mech->length;
}
OM_uint32
gs2_encapsulate_token(const gss_buffer_t input_token,
const gss_OID token_oid,
gss_buffer_t output_token)
{
size_t tokenSize;
unsigned char *buf;
if (input_token == GSS_C_NO_BUFFER || token_oid == GSS_C_NO_OID)
return GSS_S_CALL_INACCESSIBLE_READ;
if (output_token == GSS_C_NO_BUFFER)
return GSS_S_CALL_INACCESSIBLE_WRITE;
tokenSize = token_size(token_oid, input_token->length);
output_token->value = malloc(tokenSize);
if (output_token->value == NULL)
return GSS_S_FAILURE;
buf = output_token->value;
make_token_header(token_oid, input_token->length, &buf);
memcpy(buf, input_token->value, input_token->length);
output_token->length = tokenSize;
return GSS_S_COMPLETE;
}
#endif
#ifndef HAVE_GSS_DECAPSULATE_TOKEN
static int
der_read_length(unsigned char **buf, ssize_t *bufsize)
{
unsigned char sf;
int ret;
if (*bufsize < 1)
return -1;
sf = *(*buf)++;
(*bufsize)--;
if (sf & 0x80) {
if ((sf &= 0x7f) > ((*bufsize)-1))
return -1;
if (sf > sizeof(int))
return -1;
ret = 0;
for (; sf; sf--) {
ret = (ret<<8) + (*(*buf)++);
(*bufsize)--;
}
} else {
ret = sf;
}
return ret;
}
static OM_uint32
verify_token_header(OM_uint32 *minor,
const gss_OID mech,
size_t *body_size,
unsigned char **buf_in,
size_t toksize_in)
{
unsigned char *buf = *buf_in;
ssize_t seqsize;
gss_OID_desc toid;
ssize_t toksize = (ssize_t)toksize_in;
*minor = 0;
if ((toksize -= 1) < 0)
return GSS_S_DEFECTIVE_TOKEN;
if (*buf++ != 0x60)
return GSS_S_DEFECTIVE_TOKEN;
seqsize = der_read_length(&buf, &toksize);
if (seqsize < 0)
return GSS_S_DEFECTIVE_TOKEN;
if (seqsize != toksize)
return GSS_S_DEFECTIVE_TOKEN;
if ((toksize -= 1) < 0)
return GSS_S_DEFECTIVE_TOKEN;
if (*buf++ != 0x06)
return GSS_S_DEFECTIVE_TOKEN;
if ((toksize -= 1) < 0)
return GSS_S_DEFECTIVE_TOKEN;
toid.length = *buf++;
if ((toksize -= toid.length) < 0)
return GSS_S_DEFECTIVE_TOKEN;
toid.elements = buf;
buf += toid.length;
if (!gss_oid_equal(&toid, mech))
return GSS_S_DEFECTIVE_TOKEN;
*buf_in = buf;
*body_size = toksize;
return GSS_S_COMPLETE;
}
OM_uint32
gs2_decapsulate_token(const gss_buffer_t input_token,
const gss_OID token_oid,
gss_buffer_t output_token)
{
OM_uint32 major, minor;
size_t body_size = 0;
unsigned char *buf_in;
if (input_token == GSS_C_NO_BUFFER || token_oid == GSS_C_NO_OID)
return GSS_S_CALL_INACCESSIBLE_READ;
if (output_token == GSS_C_NO_BUFFER)
return GSS_S_CALL_INACCESSIBLE_WRITE;
buf_in = input_token->value;
major = verify_token_header(&minor, token_oid, &body_size, &buf_in,
input_token->length);
if (minor != 0)
return GSS_S_DEFECTIVE_TOKEN;
output_token->value = malloc(body_size);
if (output_token->value == NULL)
return GSS_S_FAILURE;
memcpy(output_token->value, buf_in, body_size);
output_token->length = body_size;
return GSS_S_COMPLETE;
}
#endif
#ifndef HAVE_GSS_OID_EQUAL
int
gs2_oid_equal(const gss_OID o1, const gss_OID o2)
{
return o1->length == o2->length &&
(memcmp(o1->elements, o2->elements, o1->length) == 0);
}
#endif