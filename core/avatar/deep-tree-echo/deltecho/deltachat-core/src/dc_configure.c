#include <dirent.h>
#include <unistd.h>
#include "dc_context.h"
#include "dc_loginparam.h"
#include "dc_imap.h"
#include "dc_smtp.h"
#include "dc_saxparser.h"
#include "dc_job.h"
#include "dc_oauth2.h"
int dc_connect_to_configured_imap(dc_context_t* context, dc_imap_t* imap)
{
int              ret_connected = DC_NOT_CONNECTED;
dc_loginparam_t* param = dc_loginparam_new();
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || imap==NULL) {
dc_log_warning(imap->context, 0, "Cannot connect to IMAP: Bad parameters.");
goto cleanup;
}
if (dc_imap_is_connected(imap)) {
ret_connected = DC_ALREADY_CONNECTED;
goto cleanup;
}
if (dc_sqlite3_get_config_int(imap->context->sql, "configured", 0)==0) {
dc_log_warning(imap->context, 0, "Not configured, cannot connect.");
goto cleanup;
}
dc_loginparam_read(param, imap->context->sql,
"configured_" );
if (!dc_imap_connect(imap, param)) {
goto cleanup;
}
ret_connected = DC_JUST_CONNECTED;
cleanup:
dc_loginparam_unref(param);
return ret_connected;
}
typedef struct moz_autoconfigure_t
{
const dc_loginparam_t* in;
char*                  in_emaildomain;
char*                  in_emaillocalpart;
dc_loginparam_t*       out;
int                    out_imap_set;
int                    out_smtp_set;
#define                MOZ_SERVER_IMAP 1
#define                MOZ_SERVER_SMTP 2
int                    tag_server;
#define                MOZ_HOSTNAME    10
#define                MOZ_PORT        11
#define                MOZ_USERNAME    12
#define                MOZ_SOCKETTYPE  13
int                    tag_config;
} moz_autoconfigure_t;
static char* read_autoconf_file(dc_context_t* context, const char* url)
{
char* filecontent = NULL;
dc_log_info(context, 0, "Testing %s ...", url);
filecontent = (char*)context->cb(context, DC_EVENT_HTTP_GET, (uintptr_t)url, 0);
if (filecontent==NULL || filecontent[0]==0) {
free(filecontent);
dc_log_info(context, 0, "Can't read file.");
return NULL;
}
return filecontent;
}
static void moz_autoconfigure_starttag_cb(void* userdata, const char* tag, char** attr)
{
moz_autoconfigure_t* moz_ac = (moz_autoconfigure_t*)userdata;
const char*          p1 = NULL;
if (strcmp(tag, "incomingserver")==0) {
moz_ac->tag_server = (moz_ac->out_imap_set==0 && (p1=dc_attr_find(attr, "type"))!=NULL && strcasecmp(p1, "imap")==0)? MOZ_SERVER_IMAP : 0;
moz_ac->tag_config = 0;
}
else if (strcmp(tag, "outgoingserver")==0) {
moz_ac->tag_server = moz_ac->out_smtp_set==0? MOZ_SERVER_SMTP : 0;
moz_ac->tag_config = 0;
}
else if (strcmp(tag, "hostname")==0) {
moz_ac->tag_config = MOZ_HOSTNAME;
}
else if (strcmp(tag, "port")==0 ) {
moz_ac->tag_config = MOZ_PORT;
}
else if (strcmp(tag, "sockettype")==0) {
moz_ac->tag_config = MOZ_SOCKETTYPE;
}
else if (strcmp(tag, "username")==0) {
moz_ac->tag_config = MOZ_USERNAME;
}
}
static void moz_autoconfigure_text_cb(void* userdata, const char* text, int len)
{
moz_autoconfigure_t* moz_ac = (moz_autoconfigure_t*)userdata;
char* val = dc_strdup(text);
dc_trim(val);
dc_str_replace(&val, "%EMAILADDRESS%",   moz_ac->in->addr);
dc_str_replace(&val, "%EMAILLOCALPART%", moz_ac->in_emaillocalpart);
dc_str_replace(&val, "%EMAILDOMAIN%",    moz_ac->in_emaildomain);
if (moz_ac->tag_server==MOZ_SERVER_IMAP)
{
switch (moz_ac->tag_config) {
case MOZ_HOSTNAME:
free(moz_ac->out->mail_server);
moz_ac->out->mail_server = val;
val = NULL;
break;
case MOZ_PORT:
moz_ac->out->mail_port = atoi(val);
break;
case MOZ_USERNAME:
free(moz_ac->out->mail_user);
moz_ac->out->mail_user = val;
val = NULL;
break;
case MOZ_SOCKETTYPE:
if (strcasecmp(val, "ssl")==0)      { moz_ac->out->server_flags |=DC_LP_IMAP_SOCKET_SSL; }
if (strcasecmp(val, "starttls")==0) { moz_ac->out->server_flags |=DC_LP_IMAP_SOCKET_STARTTLS; }
if (strcasecmp(val, "plain")==0)    { moz_ac->out->server_flags |=DC_LP_IMAP_SOCKET_PLAIN; }
break;
}
}
else if (moz_ac->tag_server==MOZ_SERVER_SMTP)
{
switch (moz_ac->tag_config) {
case MOZ_HOSTNAME:
free(moz_ac->out->send_server);
moz_ac->out->send_server = val;
val = NULL;
break;
case MOZ_PORT:
moz_ac->out->send_port = atoi(val);
break;
case MOZ_USERNAME:
free(moz_ac->out->send_user);
moz_ac->out->send_user = val;
val = NULL;
break;
case MOZ_SOCKETTYPE:
if (strcasecmp(val, "ssl")==0)      { moz_ac->out->server_flags |=DC_LP_SMTP_SOCKET_SSL; }
if (strcasecmp(val, "starttls")==0) { moz_ac->out->server_flags |=DC_LP_SMTP_SOCKET_STARTTLS; }
if (strcasecmp(val, "plain")==0)    { moz_ac->out->server_flags |=DC_LP_SMTP_SOCKET_PLAIN; }
break;
}
}
free(val);
}
static void moz_autoconfigure_endtag_cb(void* userdata, const char* tag)
{
moz_autoconfigure_t* moz_ac = (moz_autoconfigure_t*)userdata;
if (strcmp(tag, "incomingserver")==0) {
moz_ac->tag_server = 0;
moz_ac->tag_config = 0;
moz_ac->out_imap_set = 1;
}
else if (strcmp(tag, "outgoingserver")==0) {
moz_ac->tag_server = 0;
moz_ac->tag_config = 0;
moz_ac->out_smtp_set = 1;
}
else {
moz_ac->tag_config = 0;
}
}
static dc_loginparam_t* moz_autoconfigure(dc_context_t* context, const char* url, const dc_loginparam_t* param_in)
{
char*               xml_raw = NULL;
moz_autoconfigure_t moz_ac;
memset(&moz_ac, 0, sizeof(moz_autoconfigure_t));
if ((xml_raw=read_autoconf_file(context, url))==NULL) {
goto cleanup;
}
moz_ac.in                = param_in;
moz_ac.in_emaillocalpart = dc_strdup(param_in->addr); char* p = strchr(moz_ac.in_emaillocalpart, '@'); if (p==NULL) { goto cleanup; } *p = 0;
moz_ac.in_emaildomain    = dc_strdup(p+1);
moz_ac.out               = dc_loginparam_new();
dc_saxparser_t                saxparser;
dc_saxparser_init            (&saxparser, &moz_ac);
dc_saxparser_set_tag_handler (&saxparser, moz_autoconfigure_starttag_cb, moz_autoconfigure_endtag_cb);
dc_saxparser_set_text_handler(&saxparser, moz_autoconfigure_text_cb);
dc_saxparser_parse           (&saxparser, xml_raw);
if (moz_ac.out->mail_server==NULL
|| moz_ac.out->mail_port  ==0
|| moz_ac.out->send_server==NULL
|| moz_ac.out->send_port  ==0)
{
{ char* r = dc_loginparam_get_readable(moz_ac.out); dc_log_warning(context, 0, "Bad or incomplete autoconfig: %s", r); free(r); }
dc_loginparam_unref(moz_ac.out);
moz_ac.out = NULL;
goto cleanup;
}
cleanup:
free(xml_raw);
free(moz_ac.in_emaildomain);
free(moz_ac.in_emaillocalpart);
return moz_ac.out;
}
typedef struct outlk_autodiscover_t
{
const dc_loginparam_t* in;
dc_loginparam_t*       out;
int                    out_imap_set;
int                    out_smtp_set;
#define                OUTLK_TYPE         1
#define                OUTLK_SERVER       2
#define                OUTLK_PORT         3
#define                OUTLK_SSL          4
#define                OUTLK_REDIRECTURL  5
#define                _OUTLK_CNT_        6
int                    tag_config;
char*                  config[_OUTLK_CNT_];
char*                  redirect;
} outlk_autodiscover_t;
static void outlk_clean_config(outlk_autodiscover_t* outlk_ad)
{
int i;
for (i = 0; i < _OUTLK_CNT_; i++) {
free(outlk_ad->config[i]);
outlk_ad->config[i] = NULL;
}
}
static void outlk_autodiscover_starttag_cb(void* userdata, const char* tag, char** attr)
{
outlk_autodiscover_t* outlk_ad = (outlk_autodiscover_t*)userdata;
if (strcmp(tag, "protocol")==0)    { outlk_clean_config(outlk_ad); }
else if (strcmp(tag, "type")==0)        { outlk_ad->tag_config = OUTLK_TYPE; }
else if (strcmp(tag, "server")==0)      { outlk_ad->tag_config = OUTLK_SERVER; }
else if (strcmp(tag, "port")==0)        { outlk_ad->tag_config = OUTLK_PORT; }
else if (strcmp(tag, "ssl")==0)         { outlk_ad->tag_config = OUTLK_SSL; }
else if (strcmp(tag, "redirecturl")==0) { outlk_ad->tag_config = OUTLK_REDIRECTURL; }
}
static void outlk_autodiscover_text_cb(void* userdata, const char* text, int len)
{
outlk_autodiscover_t* outlk_ad = (outlk_autodiscover_t*)userdata;
char* val = dc_strdup(text);
dc_trim(val);
free(outlk_ad->config[outlk_ad->tag_config]);
outlk_ad->config[outlk_ad->tag_config] = val;
}
static void outlk_autodiscover_endtag_cb(void* userdata, const char* tag)
{
outlk_autodiscover_t* outlk_ad = (outlk_autodiscover_t*)userdata;
if (strcmp(tag, "protocol")==0)
{
if (outlk_ad->config[OUTLK_TYPE])
{
int port    = dc_atoi_null_is_0(outlk_ad->config[OUTLK_PORT]),
ssl_on  = (outlk_ad->config[OUTLK_SSL] && strcasecmp(outlk_ad->config[OUTLK_SSL], "on")==0),
ssl_off = (outlk_ad->config[OUTLK_SSL] && strcasecmp(outlk_ad->config[OUTLK_SSL], "off")==0);
if (strcasecmp(outlk_ad->config[OUTLK_TYPE], "imap")==0 && outlk_ad->out_imap_set==0)
{
outlk_ad->out->mail_server = dc_strdup_keep_null(outlk_ad->config[OUTLK_SERVER]);
outlk_ad->out->mail_port   = port;
if (ssl_on)  { outlk_ad->out->server_flags |= DC_LP_IMAP_SOCKET_SSL; }
else if (ssl_off) { outlk_ad->out->server_flags |= DC_LP_IMAP_SOCKET_PLAIN; }
outlk_ad->out_imap_set = 1;
}
else if (strcasecmp(outlk_ad->config[OUTLK_TYPE], "smtp")==0 && outlk_ad->out_smtp_set==0)
{
outlk_ad->out->send_server = dc_strdup_keep_null(outlk_ad->config[OUTLK_SERVER]);
outlk_ad->out->send_port   = port;
if (ssl_on)  { outlk_ad->out->server_flags |= DC_LP_SMTP_SOCKET_SSL; }
else if (ssl_off) { outlk_ad->out->server_flags |= DC_LP_SMTP_SOCKET_PLAIN; }
outlk_ad->out_smtp_set = 1;
}
}
outlk_clean_config(outlk_ad);
}
outlk_ad->tag_config = 0;
}
static dc_loginparam_t* outlk_autodiscover(dc_context_t* context, const char* url__, const dc_loginparam_t* param_in)
{
char*                 xml_raw = NULL;
char*                 url = dc_strdup(url__);
outlk_autodiscover_t  outlk_ad;
int                   i;
for (i = 0; i < 10 ; i++)
{
memset(&outlk_ad, 0, sizeof(outlk_autodiscover_t));
if ((xml_raw=read_autoconf_file(context, url))==NULL) {
goto cleanup;
}
outlk_ad.in                = param_in;
outlk_ad.out               = dc_loginparam_new();
dc_saxparser_t                 saxparser;
dc_saxparser_init            (&saxparser, &outlk_ad);
dc_saxparser_set_tag_handler (&saxparser, outlk_autodiscover_starttag_cb, outlk_autodiscover_endtag_cb);
dc_saxparser_set_text_handler(&saxparser, outlk_autodiscover_text_cb);
dc_saxparser_parse           (&saxparser, xml_raw);
if (outlk_ad.config[OUTLK_REDIRECTURL] && outlk_ad.config[OUTLK_REDIRECTURL][0]) {
free(url);
url = dc_strdup(outlk_ad.config[OUTLK_REDIRECTURL]);
dc_loginparam_unref(outlk_ad.out);
outlk_clean_config(&outlk_ad);
free(xml_raw);
xml_raw = NULL;
}
else {
break;
}
}
if (outlk_ad.out->mail_server==NULL
|| outlk_ad.out->mail_port  ==0
|| outlk_ad.out->send_server==NULL
|| outlk_ad.out->send_port  ==0)
{
{ char* r = dc_loginparam_get_readable(outlk_ad.out); dc_log_warning(context, 0, "Bad or incomplete autoconfig: %s", r); free(r); }
dc_loginparam_unref(outlk_ad.out);
outlk_ad.out = NULL;
goto cleanup;
}
cleanup:
free(url);
free(xml_raw);
outlk_clean_config(&outlk_ad);
return outlk_ad.out;
}
typedef struct dc_imapfolder_t
{
char* name_to_select;
char* name_utf8;
#define MEANING_UNKNOWN      0
#define MEANING_SENT_OBJECTS 1
#define MEANING_OTHER_KNOWN  2
int     meaning;
} dc_imapfolder_t;
static int get_folder_meaning(struct mailimap_mbx_list_flags* flags)
{
int ret_meaning = MEANING_UNKNOWN;
if (flags)
{
clistiter* iter2;
for (iter2=clist_begin(flags->mbf_oflags); iter2!=NULL; iter2=clist_next(iter2))
{
struct mailimap_mbx_list_oflag* oflag = (struct mailimap_mbx_list_oflag*)clist_content(iter2);
switch (oflag->of_type)
{
case MAILIMAP_MBX_LIST_OFLAG_FLAG_EXT:
if (strcasecmp(oflag->of_flag_ext, "spam")==0
|| strcasecmp(oflag->of_flag_ext, "trash")==0
|| strcasecmp(oflag->of_flag_ext, "drafts")==0
|| strcasecmp(oflag->of_flag_ext, "junk")==0)
{
ret_meaning = MEANING_OTHER_KNOWN;
}
else if (strcasecmp(oflag->of_flag_ext, "sent")==0)
{
ret_meaning = MEANING_SENT_OBJECTS;
}
break;
}
}
}
return ret_meaning;
}
static int get_folder_meaning_by_name(const char* folder_name)
{
int ret_meaning = MEANING_UNKNOWN;
static const char* sent_names =
",sent,sent objects,gesendet,";
char* lower = dc_mprintf(",%s,", folder_name);
dc_strlower_in_place(lower);
if (strstr(sent_names, lower)!=NULL) {
ret_meaning = MEANING_SENT_OBJECTS;
}
free(lower);
return ret_meaning;
}
static clist* list_folders(dc_imap_t* imap)
{
clist*     imap_list = NULL;
clistiter* iter1 = NULL;
clist *    ret_list = clist_new();
int        r = 0;
int        xlist_works = 0;
if (imap==NULL || imap->etpan==NULL) {
goto cleanup;
}
if (imap->has_xlist)  {
r = mailimap_xlist(imap->etpan, "", "*", &imap_list);
}
else {
r = mailimap_list(imap->etpan, "", "*", &imap_list);
}
if (dc_imap_is_error(imap, r) || imap_list==NULL) {
imap_list = NULL;
dc_log_warning(imap->context, 0, "Cannot get folder list.");
goto cleanup;
}
if (clist_count(imap_list)<=0) {
dc_log_warning(imap->context, 0, "Folder list is empty.");
goto cleanup;
}
imap->imap_delimiter = '.';
for (iter1 = clist_begin(imap_list); iter1!=NULL ; iter1 = clist_next(iter1))
{
struct mailimap_mailbox_list* imap_folder =
(struct mailimap_mailbox_list*)clist_content(iter1);
if (imap_folder->mb_delimiter) {
imap->imap_delimiter = imap_folder->mb_delimiter;
}
dc_imapfolder_t* ret_folder = calloc(1, sizeof(dc_imapfolder_t));
if (strcasecmp(imap_folder->mb_name, "INBOX")==0) {
ret_folder->name_to_select = dc_strdup("INBOX");
}
else {
ret_folder->name_to_select = dc_strdup(imap_folder->mb_name);
}
ret_folder->name_utf8 = dc_decode_modified_utf7(imap_folder->mb_name, 0);
ret_folder->meaning = get_folder_meaning(imap_folder->mb_flag);
if (ret_folder->meaning==MEANING_OTHER_KNOWN
|| ret_folder->meaning==MEANING_SENT_OBJECTS ) {
xlist_works = 1;
}
clist_append(ret_list, (void*)ret_folder);
}
if (!xlist_works) {
for (iter1 = clist_begin(ret_list); iter1!=NULL ; iter1 = clist_next(iter1))
{
dc_imapfolder_t* ret_folder = (struct dc_imapfolder_t*)clist_content(iter1);
ret_folder->meaning = get_folder_meaning_by_name(ret_folder->name_utf8);
}
}
cleanup:
if (imap_list) {
mailimap_list_result_free(imap_list);
}
return ret_list;
}
static void free_folders(clist* folders)
{
if (folders) {
clistiter* iter1;
for (iter1 = clist_begin(folders); iter1!=NULL ; iter1 = clist_next(iter1)) {
dc_imapfolder_t* ret_folder = (struct dc_imapfolder_t*)clist_content(iter1);
free(ret_folder->name_to_select);
free(ret_folder->name_utf8);
free(ret_folder);
}
clist_free(folders);
}
}
void dc_configure_folders(dc_context_t* context, dc_imap_t* imap, int flags)
{
#define DC_DEF_MVBOX "DeltaChat"
clist*     folder_list = NULL;
clistiter* iter;
char*      mvbox_folder = NULL;
char*      sentbox_folder = NULL;
char*      fallback_folder = NULL;
if (imap==NULL || imap->etpan==NULL) {
goto cleanup;
}
dc_log_info(context, 0, "Configuring IMAP-folders.");
folder_list = list_folders(imap);
fallback_folder = dc_mprintf("INBOX%c%s", imap->imap_delimiter, DC_DEF_MVBOX);
for (iter=clist_begin(folder_list); iter!=NULL; iter=clist_next(iter))
{
dc_imapfolder_t* folder = (struct dc_imapfolder_t*)clist_content(iter);
if (strcmp(folder->name_utf8, DC_DEF_MVBOX)==0
|| strcmp(folder->name_utf8, fallback_folder)==0) {
if (mvbox_folder==NULL) {
mvbox_folder = dc_strdup(folder->name_to_select);
}
}
if (folder->meaning==MEANING_SENT_OBJECTS) {
if(sentbox_folder==NULL) {
sentbox_folder = dc_strdup(folder->name_to_select);
}
}
}
if (mvbox_folder==NULL && (flags&DC_CREATE_MVBOX))
{
dc_log_info(context, 0, "Creating MVBOX-folder \"%s\"...", DC_DEF_MVBOX);
int r = mailimap_create(imap->etpan, DC_DEF_MVBOX);
if (dc_imap_is_error(imap, r)) {
dc_log_warning(context, 0, "Cannot create MVBOX-folder, using trying INBOX subfolder.");
r = mailimap_create(imap->etpan, fallback_folder);
if (dc_imap_is_error(imap, r)) {
dc_log_warning(context, 0, "Cannot create MVBOX-folder.");
}
else {
mvbox_folder = dc_strdup(fallback_folder);
dc_log_info(context, 0, "MVBOX-folder created as INBOX subfolder.");
}
}
else {
mvbox_folder = dc_strdup(DC_DEF_MVBOX);
dc_log_info(context, 0, "MVBOX-folder created.");
}
mailimap_subscribe(imap->etpan, mvbox_folder);
}
dc_sqlite3_set_config_int(context->sql, "folders_configured", DC_FOLDERS_CONFIGURED_VERSION);
dc_sqlite3_set_config(context->sql, "configured_mvbox_folder", mvbox_folder);
dc_sqlite3_set_config(context->sql, "configured_sentbox_folder", sentbox_folder);
cleanup:
free_folders(folder_list);
free(mvbox_folder);
free(fallback_folder);
}
void dc_job_do_DC_JOB_CONFIGURE_IMAP(dc_context_t* context, dc_job_t* job)
{
int              success = 0;
int              imap_connected_here = 0;
int              smtp_connected_here = 0;
int              ongoing_allocated_here = 0;
char*            mvbox_folder = NULL;
dc_loginparam_t* param = NULL;
char*            param_domain = NULL;
char*            param_addr_urlencoded = NULL;
dc_loginparam_t* param_autoconfig = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
goto cleanup;
}
if (!dc_alloc_ongoing(context)) {
goto cleanup;
}
ongoing_allocated_here = 1;
#define PROGRESS(p) \
if (context->shall_stop_ongoing) { goto cleanup; } \
context->cb(context, DC_EVENT_CONFIGURE_PROGRESS, (p)<1? 1 : ((p)>999? 999 : (p)), 0);
if (!dc_sqlite3_is_open(context->sql)) {
dc_log_error(context, 0, "Cannot configure, database not opened.");
goto cleanup;
}
dc_imap_disconnect(context->inbox);
dc_imap_disconnect(context->sentbox_thread.imap);
dc_imap_disconnect(context->mvbox_thread.imap);
dc_smtp_disconnect(context->smtp);
context->smtp->log_connect_errors = 1;
context->inbox->log_connect_errors = 1;
context->sentbox_thread.imap->log_connect_errors = 1;
context->mvbox_thread.imap->log_connect_errors = 1;
dc_log_info(context, 0, "Configure ...");
PROGRESS(0)
param = dc_loginparam_new();
dc_loginparam_read(param, context->sql, "");
if (param->addr==NULL) {
dc_log_error(context, 0, "Please enter the email address.");
goto cleanup;
}
dc_trim(param->addr);
if (param->server_flags & DC_LP_AUTH_OAUTH2)
{
PROGRESS(10)
char* oauth2_addr = dc_get_oauth2_addr(context, param->addr, param->mail_pw);
if (oauth2_addr) {
free(param->addr);
param->addr = oauth2_addr;
dc_sqlite3_set_config(context->sql, "addr", param->addr);
}
PROGRESS(20)
}
param_domain = strchr(param->addr, '@');
if (param_domain==NULL || param_domain[0]==0) {
dc_log_error(context, 0, "Bad email-address.");
goto cleanup;
}
param_domain++;
param_addr_urlencoded = dc_urlencode(param->addr);
if (param->mail_pw==NULL) {
param->mail_pw = dc_strdup(NULL);
}
PROGRESS(200)
if (param->mail_server ==NULL
&& param->mail_port   ==0
&& param->send_server ==NULL
&& param->send_port   ==0
&& param->send_user   ==NULL
&& (param->server_flags & (~DC_LP_AUTH_OAUTH2))==0
)
{
int keep_flags = param->server_flags & DC_LP_AUTH_OAUTH2;
if (param_autoconfig==NULL) {
char* url = dc_mprintf("https:
param_autoconfig = moz_autoconfigure(context, url, param);
free(url);
PROGRESS(300)
}
if (param_autoconfig==NULL) {
char* url = dc_mprintf("https:
param_autoconfig = moz_autoconfigure(context, url, param);
free(url);
PROGRESS(310)
}
for (int i = 0; i <= 1; i++) {
if (param_autoconfig==NULL) {
char* url = dc_mprintf("https:
param_autoconfig = outlk_autodiscover(context, url, param);
free(url);
PROGRESS(320+i*10)
}
}
if (param_autoconfig==NULL) {
char* url = dc_mprintf("http:
param_autoconfig = moz_autoconfigure(context, url, param);
free(url);
PROGRESS(340)
}
if (param_autoconfig==NULL) {
char* url = dc_mprintf("http:
param_autoconfig = moz_autoconfigure(context, url, param);
free(url);
PROGRESS(350)
}
if (param_autoconfig==NULL)
{
char* url = dc_mprintf("https:
param_autoconfig = moz_autoconfigure(context, url, param);
free(url);
PROGRESS(500)
}
if (param_autoconfig)
{
{ char* r = dc_loginparam_get_readable(param_autoconfig); dc_log_info(context, 0, "Got autoconfig: %s", r); free(r); }
if (param_autoconfig->mail_user) {
free(param->mail_user);
param->mail_user= dc_strdup_keep_null(param_autoconfig->mail_user);
}
param->mail_server  = dc_strdup_keep_null(param_autoconfig->mail_server);
param->mail_port    =                  param_autoconfig->mail_port;
param->send_server  = dc_strdup_keep_null(param_autoconfig->send_server);
param->send_port    =                  param_autoconfig->send_port;
param->send_user    = dc_strdup_keep_null(param_autoconfig->send_user);
param->server_flags =                  param_autoconfig->server_flags;
}
param->server_flags |= keep_flags;
}
#define TYPICAL_IMAP_SSL_PORT       993
#define TYPICAL_IMAP_STARTTLS_PORT  143
#define TYPICAL_SMTP_SSL_PORT       465
#define TYPICAL_SMTP_STARTTLS_PORT  587
#define TYPICAL_SMTP_PLAIN_PORT      25
if (param->mail_server==NULL) {
param->mail_server = dc_mprintf("imap.%s", param_domain);
}
if (param->mail_port==0) {
param->mail_port = (param->server_flags&(DC_LP_IMAP_SOCKET_STARTTLS|DC_LP_IMAP_SOCKET_PLAIN))?  TYPICAL_IMAP_STARTTLS_PORT : TYPICAL_IMAP_SSL_PORT;
}
if (param->mail_user==NULL) {
param->mail_user = dc_strdup(param->addr);
}
if (param->send_server==NULL && param->mail_server) {
param->send_server = dc_strdup(param->mail_server);
if (strncmp(param->send_server, "imap.", 5)==0) {
memcpy(param->send_server, "smtp", 4);
}
}
if (param->send_port==0) {
param->send_port = (param->server_flags&DC_LP_SMTP_SOCKET_STARTTLS)?  TYPICAL_SMTP_STARTTLS_PORT :
((param->server_flags&DC_LP_SMTP_SOCKET_PLAIN)? TYPICAL_SMTP_PLAIN_PORT : TYPICAL_SMTP_SSL_PORT);
}
if (param->send_user==NULL && param->mail_user) {
param->send_user = dc_strdup(param->mail_user);
}
if (param->send_pw==NULL && param->mail_pw) {
param->send_pw = dc_strdup(param->mail_pw);
}
if (!dc_exactly_one_bit_set(param->server_flags&DC_LP_AUTH_FLAGS))
{
param->server_flags &= ~DC_LP_AUTH_FLAGS;
param->server_flags |= DC_LP_AUTH_NORMAL;
}
if (!dc_exactly_one_bit_set(param->server_flags&DC_LP_IMAP_SOCKET_FLAGS))
{
param->server_flags &= ~DC_LP_IMAP_SOCKET_FLAGS;
param->server_flags |= (param->send_port==TYPICAL_IMAP_STARTTLS_PORT?  DC_LP_IMAP_SOCKET_STARTTLS : DC_LP_IMAP_SOCKET_SSL);
}
if (!dc_exactly_one_bit_set(param->server_flags&DC_LP_SMTP_SOCKET_FLAGS))
{
param->server_flags &= ~DC_LP_SMTP_SOCKET_FLAGS;
param->server_flags |= ( param->send_port==TYPICAL_SMTP_STARTTLS_PORT?  DC_LP_SMTP_SOCKET_STARTTLS :
(param->send_port==TYPICAL_SMTP_PLAIN_PORT? DC_LP_SMTP_SOCKET_PLAIN: DC_LP_SMTP_SOCKET_SSL));
}
if (param->addr        ==NULL
|| param->mail_server ==NULL
|| param->mail_port   ==0
|| param->mail_user   ==NULL
|| param->mail_pw     ==NULL
|| param->send_server ==NULL
|| param->send_port   ==0
|| param->send_user   ==NULL
|| param->send_pw     ==NULL
|| param->server_flags==0)
{
dc_log_error(context, 0, "Account settings incomplete.");
goto cleanup;
}
PROGRESS(600)
for (int username_variation=0; username_variation<=1; username_variation++)
{
{ char* r = dc_loginparam_get_readable(param); dc_log_info(context, 0, "Trying: %s", r); free(r); }
if (dc_imap_connect(context->inbox, param)) {
break;
}
if (param_autoconfig) {
goto cleanup;
}
PROGRESS(650+username_variation*30)
param->server_flags &= ~DC_LP_IMAP_SOCKET_FLAGS;
param->server_flags |=  DC_LP_IMAP_SOCKET_STARTTLS;
{ char* r = dc_loginparam_get_readable(param); dc_log_info(context, 0, "Trying: %s", r); free(r); }
if (dc_imap_connect(context->inbox, param)) {
break;
}
PROGRESS(660+username_variation*30)
param->mail_port = TYPICAL_IMAP_STARTTLS_PORT;
{ char* r = dc_loginparam_get_readable(param); dc_log_info(context, 0, "Trying: %s", r); free(r); }
if (dc_imap_connect(context->inbox, param)) {
break;
}
else if (username_variation) {
goto cleanup;
}
PROGRESS(670+username_variation*30)
param->server_flags &= ~DC_LP_IMAP_SOCKET_FLAGS;
param->server_flags |=  DC_LP_IMAP_SOCKET_SSL;
param->mail_port    =   TYPICAL_IMAP_SSL_PORT;
char* at = strchr(param->mail_user, '@');
if (at) { *at = 0; }
at = strchr(param->send_user, '@');
if (at) { *at = 0; }
}
imap_connected_here = 1;
PROGRESS(800)
if (!dc_smtp_connect(context->smtp, param))  {
if (param_autoconfig) {
goto cleanup;
}
PROGRESS(850)
param->server_flags &= ~DC_LP_SMTP_SOCKET_FLAGS;
param->server_flags |=  DC_LP_SMTP_SOCKET_STARTTLS;
param->send_port    =   TYPICAL_SMTP_STARTTLS_PORT;
{ char* r = dc_loginparam_get_readable(param); dc_log_info(context, 0, "Trying: %s", r); free(r); }
if (!dc_smtp_connect(context->smtp, param)) {
PROGRESS(860)
param->server_flags &= ~DC_LP_SMTP_SOCKET_FLAGS;
param->server_flags |=  DC_LP_SMTP_SOCKET_STARTTLS;
param->send_port    =   TYPICAL_SMTP_PLAIN_PORT;
{ char* r = dc_loginparam_get_readable(param); dc_log_info(context, 0, "Trying: %s", r); free(r); }
if (!dc_smtp_connect(context->smtp, param)) {
goto cleanup;
}
}
}
smtp_connected_here = 1;
PROGRESS(900)
int flags =
( dc_sqlite3_get_config_int(context->sql, "mvbox_watch", DC_MVBOX_WATCH_DEFAULT)
|| dc_sqlite3_get_config_int(context->sql, "mvbox_move", DC_MVBOX_MOVE_DEFAULT) ) ? DC_CREATE_MVBOX : 0;
dc_configure_folders(context, context->inbox, flags);
PROGRESS(910);
dc_loginparam_write(param, context->sql, "configured_" );
dc_sqlite3_set_config_int(context->sql, "configured", 1);
PROGRESS(920)
dc_ensure_secret_key_exists(context);
success = 1;
dc_log_info(context, 0, "Configure completed.");
PROGRESS(940)
cleanup:
if (imap_connected_here) {
dc_imap_disconnect(context->inbox);
}
if (smtp_connected_here) {
dc_smtp_disconnect(context->smtp);
}
dc_loginparam_unref(param);
dc_loginparam_unref(param_autoconfig);
free(param_addr_urlencoded);
if (ongoing_allocated_here) {
dc_free_ongoing(context);
}
free(mvbox_folder);
context->cb(context, DC_EVENT_CONFIGURE_PROGRESS, success? 1000 : 0, 0);
}
void dc_configure(dc_context_t* context)
{
if (dc_has_ongoing(context)) {
dc_log_warning(context, 0, "There is already another ongoing process running.");
return;
}
dc_job_kill_action(context, DC_JOB_CONFIGURE_IMAP);
dc_job_add(context, DC_JOB_CONFIGURE_IMAP, 0, NULL, 0);
}
int dc_is_configured(const dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return 0;
}
return dc_sqlite3_get_config_int(context->sql, "configured", 0)? 1 : 0;
}
int dc_has_ongoing(dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return 0;
}
return (context->ongoing_running || context->shall_stop_ongoing==0)? 1 : 0;
}
int dc_alloc_ongoing(dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return 0;
}
if (dc_has_ongoing(context)) {
dc_log_warning(context, 0, "There is already another ongoing process running.");
return 0;
}
context->ongoing_running    = 1;
context->shall_stop_ongoing = 0;
return 1;
}
void dc_free_ongoing(dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return;
}
context->ongoing_running    = 0;
context->shall_stop_ongoing = 1;
}
void dc_stop_ongoing_process(dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return;
}
if (context->ongoing_running && context->shall_stop_ongoing==0)
{
dc_log_info(context, 0, "Signaling the ongoing process to stop ASAP.");
context->shall_stop_ongoing = 1;
}
else
{
dc_log_info(context, 0, "No ongoing process to stop.");
}
}