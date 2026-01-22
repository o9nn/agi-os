#include <config.h>
#ifndef macintosh
#ifdef WIN32
# include <winsock2.h>
#else
# include <sys/socket.h>
# include <netinet/in.h>
# include <arpa/inet.h>
# include <netdb.h>
# include <sys/utsname.h>
#endif
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>
#include <sasl.h>
#include <saslutil.h>
#include <saslplug.h>
#include <errno.h>
#include <ctype.h>
#include <stdio.h>
#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif
#include "plugin_common.h"
static void sockaddr_unmapped(
#ifdef IN6_IS_ADDR_V4MAPPED
struct sockaddr *sa, socklen_t *len
#else
struct sockaddr *sa __attribute__((unused)),
socklen_t *len __attribute__((unused))
#endif
)
{
#ifdef IN6_IS_ADDR_V4MAPPED
struct sockaddr_in6 *sin6;
struct sockaddr_in *sin4;
uint32_t addr;
int port;
if (sa->sa_family != AF_INET6)
return;
sin6 = (struct sockaddr_in6 *)sa;
if (!IN6_IS_ADDR_V4MAPPED((&sin6->sin6_addr)))
return;
sin4 = (struct sockaddr_in *)sa;
addr = *(uint32_t *)&sin6->sin6_addr.s6_addr[12];
port = sin6->sin6_port;
memset(sin4, 0, sizeof(struct sockaddr_in));
sin4->sin_addr.s_addr = addr;
sin4->sin_port = port;
sin4->sin_family = AF_INET;
#ifdef HAVE_SOCKADDR_SA_LEN
sin4->sin_len = sizeof(struct sockaddr_in);
#endif
*len = sizeof(struct sockaddr_in);
#else
return;
#endif
}
int _plug_ipfromstring(const sasl_utils_t *utils, const char *addr,
struct sockaddr *out, socklen_t outlen)
{
int i, j;
socklen_t len;
struct sockaddr_storage ss;
struct addrinfo hints, *ai = NULL;
char hbuf[NI_MAXHOST];
if(!utils || !addr || !out) {
if(utils) PARAMERROR( utils );
return SASL_BADPARAM;
}
for (i = 0; addr[i] != '\0' && addr[i] != ';'; i++) {
if (i >= NI_MAXHOST) {
if(utils) PARAMERROR( utils );
return SASL_BADPARAM;
}
hbuf[i] = addr[i];
}
hbuf[i] = '\0';
if (addr[i] == ';')
i++;
for (j = i; addr[j] != '\0'; j++)
if (!isdigit((int)(addr[j]))) {
PARAMERROR( utils );
return SASL_BADPARAM;
}
memset(&hints, 0, sizeof(hints));
hints.ai_family = PF_UNSPEC;
hints.ai_socktype = SOCK_STREAM;
hints.ai_flags = AI_PASSIVE | AI_NUMERICHOST;
if (getaddrinfo(hbuf, &addr[i], &hints, &ai) != 0) {
PARAMERROR( utils );
return SASL_BADPARAM;
}
len = (socklen_t) ai->ai_addrlen;
memcpy(&ss, ai->ai_addr, len);
freeaddrinfo(ai);
sockaddr_unmapped((struct sockaddr *)&ss, &len);
if (outlen < len) {
PARAMERROR( utils );
return SASL_BUFOVER;
}
memcpy(out, &ss, len);
return SASL_OK;
}
int _plug_iovec_to_buf(const sasl_utils_t *utils, const struct iovec *vec,
unsigned numiov, buffer_info_t **output)
{
unsigned i;
int ret;
buffer_info_t *out;
char *pos;
if(!utils || !vec || !output) {
if(utils) PARAMERROR( utils );
return SASL_BADPARAM;
}
if(!(*output)) {
*output = utils->malloc(sizeof(buffer_info_t));
if(!*output) {
MEMERROR(utils);
return SASL_NOMEM;
}
memset(*output,0,sizeof(buffer_info_t));
}
out = *output;
out->curlen = 0;
for(i=0; i<numiov; i++)
out->curlen += vec[i].iov_len;
ret = _plug_buf_alloc(utils, &out->data, &out->reallen, out->curlen);
if(ret != SASL_OK) {
MEMERROR(utils);
return SASL_NOMEM;
}
memset(out->data, 0, out->reallen);
pos = out->data;
for(i=0; i<numiov; i++) {
memcpy(pos, vec[i].iov_base, vec[i].iov_len);
pos += vec[i].iov_len;
}
return SASL_OK;
}
int _plug_buf_alloc(const sasl_utils_t *utils, char **rwbuf,
unsigned *curlen, unsigned newlen)
{
if(!utils || !rwbuf || !curlen) {
PARAMERROR(utils);
return SASL_BADPARAM;
}
if(!(*rwbuf)) {
*rwbuf = utils->malloc(newlen);
if (*rwbuf == NULL) {
*curlen = 0;
MEMERROR(utils);
return SASL_NOMEM;
}
*curlen = newlen;
} else if(*rwbuf && *curlen < newlen) {
unsigned needed = 2*(*curlen);
while(needed < newlen)
needed *= 2;
*rwbuf = utils->realloc(*rwbuf, needed);
if (*rwbuf == NULL) {
*curlen = 0;
MEMERROR(utils);
return SASL_NOMEM;
}
*curlen = needed;
}
return SASL_OK;
}
int _plug_strdup(const sasl_utils_t * utils, const char *in,
char **out, int *outlen)
{
size_t len = strlen(in);
if(!utils || !in || !out) {
if(utils) PARAMERROR(utils);
return SASL_BADPARAM;
}
*out = utils->malloc(len + 1);
if (!*out) {
MEMERROR(utils);
return SASL_NOMEM;
}
strcpy((char *) *out, in);
if (outlen)
*outlen = (int) len;
return SASL_OK;
}
void _plug_free_string(const sasl_utils_t *utils, char **str)
{
size_t len;
if (!utils || !str || !(*str)) return;
len = strlen(*str);
utils->erasebuffer(*str, (unsigned int) len);
utils->free(*str);
*str=NULL;
}
void _plug_free_secret(const sasl_utils_t *utils, sasl_secret_t **secret)
{
if(!utils || !secret || !(*secret)) return;
utils->erasebuffer((char *)(*secret)->data, (*secret)->len);
utils->free(*secret);
*secret = NULL;
}
sasl_interact_t *_plug_find_prompt(sasl_interact_t **promptlist,
unsigned int lookingfor)
{
sasl_interact_t *prompt;
if (promptlist && *promptlist) {
for (prompt = *promptlist; prompt->id != SASL_CB_LIST_END; ++prompt) {
if (prompt->id==lookingfor)
return prompt;
}
}
return NULL;
}
int _plug_get_simple(const sasl_utils_t *utils, unsigned int id, int required,
const char **result, sasl_interact_t **prompt_need)
{
int ret = SASL_FAIL;
sasl_getsimple_t *simple_cb;
void *simple_context;
sasl_interact_t *prompt;
*result = NULL;
prompt = _plug_find_prompt(prompt_need, id);
if (prompt != NULL) {
if (required && !prompt->result) {
SETERROR(utils, "Unexpectedly missing a prompt result");
return SASL_BADPARAM;
}
*result = prompt->result;
return SASL_OK;
}
ret = utils->getcallback(utils->conn, id, (sasl_callback_ft *)&simple_cb, &simple_context);
if (ret == SASL_FAIL && !required)
return SASL_OK;
if (ret == SASL_OK && simple_cb) {
ret = simple_cb(simple_context, id, result, NULL);
if (ret != SASL_OK)
return ret;
if (required && !*result) {
PARAMERROR(utils);
return SASL_BADPARAM;
}
}
return ret;
}
int _plug_get_password(const sasl_utils_t *utils, sasl_secret_t **password,
unsigned int *iscopy, sasl_interact_t **prompt_need)
{
int ret = SASL_FAIL;
sasl_getsecret_t *pass_cb;
void *pass_context;
sasl_interact_t *prompt;
*password = NULL;
*iscopy = 0;
prompt = _plug_find_prompt(prompt_need, SASL_CB_PASS);
if (prompt != NULL) {
if (!prompt->result) {
SETERROR(utils, "Unexpectedly missing a prompt result");
return SASL_BADPARAM;
}
*password = (sasl_secret_t *) utils->malloc(sizeof(sasl_secret_t) +
prompt->len + 1);
if (!*password) {
MEMERROR(utils);
return SASL_NOMEM;
}
(*password)->len=prompt->len;
memcpy((*password)->data, prompt->result, prompt->len);
(*password)->data[(*password)->len]=0;
*iscopy = 1;
return SASL_OK;
}
ret = utils->getcallback(utils->conn, SASL_CB_PASS,
(sasl_callback_ft *)&pass_cb, &pass_context);
if (ret == SASL_OK && pass_cb) {
ret = pass_cb(utils->conn, pass_context, SASL_CB_PASS, password);
if (ret != SASL_OK)
return ret;
if (!*password) {
PARAMERROR(utils);
return SASL_BADPARAM;
}
}
return ret;
}
int _plug_challenge_prompt(const sasl_utils_t *utils, unsigned int id,
const char *challenge, const char *promptstr,
const char **result, sasl_interact_t **prompt_need)
{
int ret = SASL_FAIL;
sasl_chalprompt_t *chalprompt_cb;
void *chalprompt_context;
sasl_interact_t *prompt;
*result = NULL;
prompt = _plug_find_prompt(prompt_need, id);
if (prompt != NULL) {
if (!prompt->result) {
SETERROR(utils, "Unexpectedly missing a prompt result");
return SASL_BADPARAM;
}
*result = prompt->result;
return SASL_OK;
}
ret = utils->getcallback(utils->conn, id,
(sasl_callback_ft *)&chalprompt_cb, &chalprompt_context);
if (ret == SASL_OK && chalprompt_cb) {
ret = chalprompt_cb(chalprompt_context, id,
challenge, promptstr, NULL, result, NULL);
if (ret != SASL_OK)
return ret;
if (!*result) {
PARAMERROR(utils);
return SASL_BADPARAM;
}
}
return ret;
}
int _plug_get_realm(const sasl_utils_t *utils, const char **availrealms,
const char **realm, sasl_interact_t **prompt_need)
{
int ret = SASL_FAIL;
sasl_getrealm_t *realm_cb;
void *realm_context;
sasl_interact_t *prompt;
*realm = NULL;
prompt = _plug_find_prompt(prompt_need, SASL_CB_GETREALM);
if (prompt != NULL) {
if (!prompt->result) {
SETERROR(utils, "Unexpectedly missing a prompt result");
return SASL_BADPARAM;
}
*realm = prompt->result;
return SASL_OK;
}
ret = utils->getcallback(utils->conn, SASL_CB_GETREALM,
(sasl_callback_ft *)&realm_cb, &realm_context);
if (ret == SASL_OK && realm_cb) {
ret = realm_cb(realm_context, SASL_CB_GETREALM, availrealms, realm);
if (ret != SASL_OK)
return ret;
if (!*realm) {
PARAMERROR(utils);
return SASL_BADPARAM;
}
}
return ret;
}
int _plug_make_prompts(const sasl_utils_t *utils,
sasl_interact_t **prompts_res,
const char *user_prompt, const char *user_def,
const char *auth_prompt, const char *auth_def,
const char *pass_prompt, const char *pass_def,
const char *echo_chal,
const char *echo_prompt, const char *echo_def,
const char *realm_chal,
const char *realm_prompt, const char *realm_def)
{
int num = 1;
int alloc_size;
sasl_interact_t *prompts;
if (user_prompt) num++;
if (auth_prompt) num++;
if (pass_prompt) num++;
if (echo_prompt) num++;
if (realm_prompt) num++;
if (num == 1) {
SETERROR( utils, "make_prompts() called with no actual prompts" );
return SASL_FAIL;
}
alloc_size = sizeof(sasl_interact_t)*num;
prompts = utils->malloc(alloc_size);
if (!prompts) {
MEMERROR( utils );
return SASL_NOMEM;
}
memset(prompts, 0, alloc_size);
*prompts_res = prompts;
if (user_prompt) {
(prompts)->id = SASL_CB_USER;
(prompts)->challenge = "Authorization Name";
(prompts)->prompt = user_prompt;
(prompts)->defresult = user_def;
prompts++;
}
if (auth_prompt) {
(prompts)->id = SASL_CB_AUTHNAME;
(prompts)->challenge = "Authentication Name";
(prompts)->prompt = auth_prompt;
(prompts)->defresult = auth_def;
prompts++;
}
if (pass_prompt) {
(prompts)->id = SASL_CB_PASS;
(prompts)->challenge = "Password";
(prompts)->prompt = pass_prompt;
(prompts)->defresult = pass_def;
prompts++;
}
if (echo_prompt) {
(prompts)->id = SASL_CB_ECHOPROMPT;
(prompts)->challenge = echo_chal;
(prompts)->prompt = echo_prompt;
(prompts)->defresult = echo_def;
prompts++;
}
if (realm_prompt) {
(prompts)->id = SASL_CB_GETREALM;
(prompts)->challenge = realm_chal;
(prompts)->prompt = realm_prompt;
(prompts)->defresult = realm_def;
prompts++;
}
(prompts)->id = SASL_CB_LIST_END;
(prompts)->challenge = NULL;
(prompts)->prompt = NULL;
(prompts)->defresult = NULL;
return SASL_OK;
}
void _plug_decode_init(decode_context_t *text,
const sasl_utils_t *utils, unsigned int in_maxbuf)
{
memset(text, 0, sizeof(decode_context_t));
text->utils = utils;
text->needsize = 4;
text->in_maxbuf = in_maxbuf;
}
int _plug_decode(decode_context_t *text,
const char *input, unsigned inputlen,
char **output,
unsigned *outputsize,
unsigned *outputlen,
int (*decode_pkt)(void *rock,
const char *input, unsigned inputlen,
char **output, unsigned *outputlen),
void *rock)
{
unsigned int tocopy;
unsigned diff;
char *tmp;
unsigned tmplen;
int ret;
*outputlen = 0;
while (inputlen) {
if (text->needsize) {
tocopy = (inputlen > text->needsize) ? text->needsize : inputlen;
memcpy(text->sizebuf + 4 - text->needsize, input, tocopy);
text->needsize -= tocopy;
input += tocopy;
inputlen -= tocopy;
if (!text->needsize) {
memcpy(&(text->size), text->sizebuf, 4);
text->size = ntohl(text->size);
if (!text->size)
return SASL_FAIL;
if (text->size > text->in_maxbuf) {
text->utils->log(NULL, SASL_LOG_ERR,
"encoded packet size too big (%d > %d)",
text->size, text->in_maxbuf);
return SASL_FAIL;
}
if (!text->buffer)
text->buffer = text->utils->malloc(text->in_maxbuf);
if (text->buffer == NULL) return SASL_NOMEM;
text->cursize = 0;
} else {
return SASL_OK;
}
}
diff = text->size - text->cursize;
if (inputlen < diff) {
memcpy(text->buffer + text->cursize, input, inputlen);
text->cursize += inputlen;
return SASL_OK;
}
memcpy(text->buffer + text->cursize, input, diff);
input += diff;
inputlen -= diff;
ret = decode_pkt(rock, text->buffer, text->size, &tmp, &tmplen);
if (ret != SASL_OK) return ret;
ret = _plug_buf_alloc(text->utils, output, outputsize,
*outputlen + tmplen + 1);
if (ret != SASL_OK) return ret;
memcpy(*output + *outputlen, tmp, tmplen);
*outputlen += tmplen;
*(*output + *outputlen) = '\0';
text->needsize = 4;
}
return SASL_OK;
}
void _plug_decode_free(decode_context_t *text)
{
if (text->buffer) text->utils->free(text->buffer);
}
int _plug_parseuser(const sasl_utils_t *utils,
char **user, char **realm, const char *user_realm,
const char *serverFQDN, const char *input)
{
int ret;
char *r;
if(!user || !serverFQDN) {
PARAMERROR( utils );
return SASL_BADPARAM;
}
r = strchr(input, '@');
if (!r) {
if(user_realm && user_realm[0]) {
ret = _plug_strdup(utils, user_realm, realm, NULL);
} else {
ret = _plug_strdup(utils, serverFQDN, realm, NULL);
}
if (ret == SASL_OK) {
ret = _plug_strdup(utils, input, user, NULL);
}
} else {
r++;
ret = _plug_strdup(utils, r, realm, NULL);
*--r = '\0';
*user = utils->malloc(r - input + 1);
if (*user) {
strncpy(*user, input, r - input +1);
} else {
MEMERROR( utils );
ret = SASL_NOMEM;
}
*r = '@';
}
return ret;
}
int _plug_make_fulluser(const sasl_utils_t *utils,
char **fulluser,
const char * useronly,
const char *realm)
{
if(!fulluser || !useronly || !realm) {
PARAMERROR( utils );
return (SASL_BADPARAM);
}
*fulluser = utils->malloc (strlen(useronly) + strlen(realm) + 2);
if (*fulluser == NULL) {
MEMERROR( utils );
return (SASL_NOMEM);
}
strcpy (*fulluser, useronly);
strcat (*fulluser, "@");
strcat (*fulluser, realm);
return (SASL_OK);
}
char * _plug_get_error_message (const sasl_utils_t *utils,
#ifdef WIN32
DWORD error
#else
int error
#endif
)
{
char * return_value;
#ifdef WIN32
LPVOID lpMsgBuf;
FormatMessage(
FORMAT_MESSAGE_ALLOCATE_BUFFER |
FORMAT_MESSAGE_FROM_SYSTEM |
FORMAT_MESSAGE_IGNORE_INSERTS,
NULL,
error,
MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
(LPTSTR) &lpMsgBuf,
0,
NULL
);
if (_plug_strdup (utils, lpMsgBuf, &return_value, NULL) != SASL_OK) {
return_value = NULL;
}
LocalFree( lpMsgBuf );
#else
if (_plug_strdup (utils, strerror(error), &return_value, NULL) != SASL_OK) {
return_value = NULL;
}
#endif
return (return_value);
}
void _plug_snprintf_os_info (char * osbuf, int osbuf_len)
{
#ifdef WIN32
OSVERSIONINFOEX versioninfo;
char *sysname;
versioninfo.dwOSVersionInfoSize = sizeof (versioninfo);
sysname = "Unknown Windows";
if (GetVersionEx ((OSVERSIONINFO *) &versioninfo) == FALSE) {
snprintf(osbuf, osbuf_len, "%s", sysname);
goto SKIP_OS_INFO;
}
switch (versioninfo.dwPlatformId) {
case VER_PLATFORM_WIN32s:
sysname = "Win32s on Windows 3.1";
break;
case VER_PLATFORM_WIN32_WINDOWS:
switch (versioninfo.dwMinorVersion) {
case 0:
sysname = "Windows 95";
break;
case 10:
sysname = "Windows 98";
break;
case 90:
sysname = "Windows Me";
break;
default:
sysname = "Unknown Windows 9X/ME series";
break;
}
versioninfo.dwBuildNumber &= 0xFFFF;
break;
case VER_PLATFORM_WIN32_NT:
if (versioninfo.dwMinorVersion > 99) {
} else {
switch (versioninfo.dwMajorVersion * 100 + versioninfo.dwMinorVersion) {
case 351:
sysname = "Windows NT 3.51";
break;
case 400:
sysname = "Windows NT 4.0";
break;
case 500:
sysname = "Windows 2000";
break;
case 501:
sysname = "Windows XP/.NET";
break;
default:
sysname = "Unknown Windows NT series";
break;
}
}
break;
default:
break;
}
snprintf(osbuf, osbuf_len,
"%s %s (Build %u)",
sysname,
versioninfo.szCSDVersion,
versioninfo.dwBuildNumber
);
SKIP_OS_INFO:
;
#else
struct utsname os;
uname(&os);
snprintf(osbuf, osbuf_len, "%s %s", os.sysname, os.release);
#endif
}
#if defined(WIN32)
unsigned int plug_sleep (unsigned int seconds)
{
long dwSec = seconds*1000;
Sleep (dwSec);
return 0;
}
#endif