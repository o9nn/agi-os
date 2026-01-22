#include "lib.h"
#include "net.h"
#include "imap-url.h"
#include "test-common.h"
struct valid_imap_url_test {
const char *url;
enum imap_url_parse_flags flags;
struct imap_url url_base;
struct imap_url url_parsed;
const char *url_parsed_end;
};
static const struct valid_imap_url_test valid_url_tests[] = {
{
.url = "imap:
.url_parsed = {
.host = { .name = "localhost" },
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "localhost" },
.userid = "user"
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "localhost" },
.userid = "user",
.auth_type = "PLAIN",
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "localhost" },
.auth_type = "PLAIN"
},
},
{
.url = "imap:
"%65%78%61%6d%70%6c%65.com",
.url_parsed = {
.host = { .name = "example.com" },
.userid = "hendrik",
.auth_type = "GSSAPI",
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "localhost" },
.userid = "user",
.port = 993,
},
},
{
.url = "imap:
.url_parsed = {
.host = {
.name = "127.0.0.1",
.ip = { .family = AF_INET }
},
.userid = "user",
},
},
{
.url = "imap:
.url_parsed = {
.host = {
.name = "[::1]",
.ip = { .family = AF_INET6 }
},
.userid = "user",
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "4example.com" },
.userid = "user",
.port = 423,
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "666.4example.com" },
.userid = "beelzebub",
.port = 999,
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = NULL,
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = NULL,
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX",
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX",
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX",
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX",
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = NULL,
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX.Trash",
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX;Trash",
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX",
.uidvalidity = 1341,
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX",
.uidvalidity = 23423,
},
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Drafts",
.uidvalidity = 6567,
},
},
{
.url = "imap:
"/INBOX/Drafts;UIDVALIDITY=788/;UID=16",
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Drafts",
.uidvalidity = 788,
.uid = 16,
},
},
{
.url = "imap:
"/INBOX/Drafts;UIDVALIDITY=788/;UID=16/..",
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Drafts",
.uidvalidity = 788,
.uid = 0,
},
},
{
.url = "imap:
"/INBOX/Drafts;UIDVALIDITY=788/;UID=16/../..",
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX",
.uidvalidity = 0,
.uid = 0,
},
},
{
.url = "imap:
";UID=434/;SECTION=HEADER",
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Junk",
.uidvalidity = 27667,
.uid = 434,
.section = "HEADER",
},
},
{
.url = "imap:
";UID=437/;SECTION=1.2.MIME",
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Important",
.uid = 437,
.section = "1.2.MIME",
},
},
{
.url = "imap:
"/INBOX/Important/;UID=56/;SECTION=AA/BB",
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Important",
.uid = 56, .section = "AA/BB",
},
},
{
.url = "imap:
"/INBOX/Important/;UID=56/;SECTION=AA/BB/..",
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Important",
.uid = 56, .section = "AA/",
},
},
{
.url = "imap:
";SECTION=AA/BB/../..",
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Important",
.uid = 56,
.section = NULL,
},
},
{
.url = "imap:
";SECTION=HEADER.FIELDS%20(%22To%22%20%22From%22)",
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Important",
.uid = 234,
.section = "HEADER.FIELDS (\"To\" \"From\")",
},
},
{
.url = "imap:
";PARTIAL=10.250",
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Important",
.uid = 234,
.section = NULL,
.partial_offset = 10,
.partial_size = 250,
},
},
{
.url = "imap:
";SECTION=1.3.TEXT/;PARTIAL=0.34254",
.url_parsed = {
.host = { .name = "example.com" },
.userid = "hendrik",
.mailbox = "INBOX/Important",
.uid = 34534,
.section = "1.3.TEXT",
.partial_offset = 0,
.partial_size = 34254,
},
},
{
.url = "imap:
";UIDVALIDITY=534?SUBJECT%20%22Frop?%22",
.url_parsed = {
.host = { .name = "example.com" },
.userid = "hendrik",
.mailbox = "INBOX/Sent",
.uidvalidity = 534,
.search_program = "SUBJECT \"Frop?\"",
},
},
{
.url = "
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
},
.url_parsed = {
.host = { .name = "example.org" },
.userid = "hendrik",
.mailbox = "INBOX/Trash",
},
},
{
.url = "/INBOX/Trash",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
},
},
{
.url = "user@example.com",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Accounts",
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Accounts/user@example.com",
},
},
{
.url = "Drafts",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/",
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Drafts",
},
},
{
.url = "../Drafts",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Drafts",
},
},
{
.url = "../Junk",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
.uidvalidity = 23452,
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Junk",
.uidvalidity = 0 }
},
{
.url = "../Junk;UIDVALIDITY=23",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
.uidvalidity = 23452,
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Junk",
.uidvalidity = 23 }
},
{
.url = "../../%23shared;UIDVALIDITY=23452",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
.uidvalidity = 764,
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "#shared",
.uidvalidity = 23452,
},
},
{
.url = "../../%23news;UIDVALIDITY=546/;UID=456",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
.uidvalidity = 23452,
.uid = 65,
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "#news",
.uidvalidity = 546,
.uid = 456,
},
},
{
.url = "",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
.uidvalidity = 23452,
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
.uidvalidity = 23452,
},
},
{
.url = "",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
.uidvalidity = 23452,
.uid = 65,
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
.uidvalidity = 23452,
.uid = 65,
},
},
{
.url = "",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
.uidvalidity = 23452,
.uid = 65,
.section = "AA/BB",
.have_partial = TRUE,
.partial_offset = 1024,
.partial_size = 1024,
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
.uidvalidity = 23452,
.uid = 65,
.section = "AA/BB",
.have_partial = TRUE,
.partial_offset = 1024,
.partial_size = 1024,
},
},
{
.url = "",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
.uidvalidity = 23452,
.uid = 65,
.have_partial = TRUE,
.partial_offset = 1024,
.partial_size = 1024,
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
.uidvalidity = 23452,
.uid = 65,
.have_partial = TRUE,
.partial_offset = 1024,
.partial_size = 1024,
},
},
{
.url = ";UID=4767",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
.uidvalidity = 23452,
.uid = 65,
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
.uidvalidity = 23452,
.uid = 4767,
}
},
{
.url = ";UID=4767",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
.uidvalidity = 23452,
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
.uidvalidity = 23452,
.uid = 4767,
},
},
{
.url = "../;UID=4767",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
.uidvalidity = 23452,
.uid = 65,
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX",
.uidvalidity = 0,
.uid = 4767,
}
},
{
.url = "../;UID=4767/;SECTION=TEXT",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
.uidvalidity = 23452,
.uid = 65,
.section = "1.2.3.MIME",
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Trash",
.uidvalidity = 23452,
.uid = 4767,
.section = "TEXT",
}
},
{
.url = ";SECTION=TEXT",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Drafts",
.uidvalidity = 769,
.uid = 43,
.section = "1.2.3.MIME",
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Drafts",
.uidvalidity = 769,
.uid = 43,
.section = "TEXT",
},
},
{
.url = "..",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Drafts",
.uidvalidity = 769,
.uid = 43,
.section = "AA/BB",
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Drafts",
.uidvalidity = 769,
.uid = 43,
},
},
{
.url = "../;SECTION=CC",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Drafts",
.uidvalidity = 769,
.uid = 43,
.section = "AA/BB",
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Drafts",
.uidvalidity = 769,
.uid = 43,
.section = "CC",
},
},
{
.url = "CC",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Drafts",
.uidvalidity = 769,
.uid = 43,
.section = "AA/BB",
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Drafts",
.uidvalidity = 769,
.uid = 43,
.section = "AA/CC",
},
},
{
.url = ";PARTIAL=1024.1024",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Drafts",
.uidvalidity = 769,
.uid = 43,
.have_partial = TRUE,
.partial_offset = 0,
.partial_size = 1024,
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Drafts",
.uidvalidity = 769,
.uid = 43,
.have_partial = TRUE,
.partial_offset = 1024,
.partial_size = 1024,
},
},
{
.url = "../CC/;PARTIAL=0.512",
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Drafts",
.uidvalidity = 769,
.uid = 43,
.section = "AA/BB",
.have_partial = TRUE,
.partial_offset = 1024,
.partial_size = 1024,
},
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Drafts",
.uidvalidity = 769,
.uid = 43,
.section = "AA/CC",
.have_partial = TRUE,
.partial_offset = 0,
.partial_size = 512,
},
},
{
.url = "imap:
"/INBOX/;UID=377;URLAUTH=anonymous",
.flags = IMAP_URL_PARSE_ALLOW_URLAUTH,
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX",
.uid = 377,
.uauth_rumpurl = "imap:
"/INBOX/;UID=377;URLAUTH=anonymous",
.uauth_access_application = "anonymous",
}
},
{
.url = "imap:
"/INBOX/;UID=377;URLAUTH=anonymous:internal:"
"4142434445464748494A4B4C4D4E4F5051525354",
.flags = IMAP_URL_PARSE_ALLOW_URLAUTH,
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX",
.uid = 377,
.uauth_rumpurl = "imap:
"/INBOX/;UID=377;URLAUTH=anonymous",
.uauth_access_application = "anonymous",
.uauth_mechanism = "internal",
.uauth_token =
(const unsigned char *)"ABCDEFGHIJKLMNOPQRST",
.uauth_token_size = 20,
}
},
{
.url = "imap:
";EXPIRE=2011-02-12T12:45:14+01:00"
";URLAUTH=user+frop:internal:"
"4142434445464748494A4B4C4D4E4F5051525354",
.flags = IMAP_URL_PARSE_ALLOW_URLAUTH,
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX",
.uid = 377,
.uauth_rumpurl = "imap:
"/INBOX/;UID=377"
";EXPIRE=2011-02-12T12:45:14+01:00"
";URLAUTH=user+frop",
.uauth_access_application = "user",
.uauth_access_user = "frop",
.uauth_mechanism = "internal",
.uauth_token =
(const unsigned char *)"ABCDEFGHIJKLMNOPQRST",
.uauth_token_size = 20,
}
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "localhost" },
},
.url_parsed_end = "] Referral",
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "localhost" },
.userid = "user"
},
.url_parsed_end = "] Referral",
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "localhost" },
.userid = "user",
.auth_type = "PLAIN",
},
.url_parsed_end = "] Referral",
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "localhost" },
.auth_type = "PLAIN"
},
.url_parsed_end = "] Referral",
},
{
.url = "imap:
.url_parsed = {
.host = { .name = "localhost" },
.userid = "user",
.port = 993,
},
.url_parsed_end = "] Referral",
},
{
.url = "imap:
.url_parsed = {
.host = {
.name = "127.0.0.1",
.ip = { .family = AF_INET }
},
.userid = "user",
},
.url_parsed_end = "] Referral",
},
{
.url = "imap:
.url_parsed = {
.host = {
.name = "[::1]",
.ip = { .family = AF_INET6 }
},
.userid = "user",
},
.url_parsed_end = "] Referral",
},
{
.url = "imap:
"/INBOX/Drafts;UIDVALIDITY=788/;UID=16] Referral",
.url_parsed = {
.host = { .name = "example.com" },
.userid = "user",
.mailbox = "INBOX/Drafts",
.uidvalidity = 788,
.uid = 16,
},
.url_parsed_end = "] Referral",
},
};
static const unsigned int valid_url_test_count = N_ELEMENTS(valid_url_tests);
static void
test_imap_url_valid_check(const struct imap_url *urlt,
const struct imap_url *urlp)
{
if (urlp->host.name == NULL || urlt->host.name == NULL) {
test_out_quiet(t_strdup_printf("url->host.name = %s",
urlp->host.name),
urlp->host.name == urlt->host.name);
} else {
test_out_quiet(t_strdup_printf("url->host.name = %s",
urlp->host.name),
strcmp(urlp->host.name, urlt->host.name) == 0);
}
if (urlp->userid == NULL || urlt->userid == NULL) {
test_out_quiet(t_strdup_printf("url->userid = %s",
urlp->userid),
urlp->userid == urlt->userid);
} else {
test_out_quiet(t_strdup_printf("url->userid = %s",
urlp->userid),
strcmp(urlp->userid, urlt->userid) == 0);
}
if (urlp->auth_type == NULL || urlt->auth_type == NULL) {
test_out_quiet(t_strdup_printf("url->auth_type = %s",
urlp->auth_type),
urlp->auth_type == urlt->auth_type);
} else {
test_out_quiet(t_strdup_printf("url->auth_type = %s",
urlp->auth_type),
strcmp(urlp->auth_type, urlt->auth_type) == 0);
}
if (urlp->port == 0) {
test_out_quiet("url->port = (unspecified)",
urlp->port == urlt->port);
} else {
test_out_quiet(t_strdup_printf("url->port = %u", urlp->port),
urlp->port == urlt->port);
}
if (urlp->host.ip.family == 0) {
test_out_quiet("url->host.ip = (unspecified)",
urlp->host.ip.family == urlt->host.ip.family);
} else {
test_out_quiet("url->host.ip = (valid)",
urlp->host.ip.family == urlt->host.ip.family);
}
if (urlp->mailbox == NULL || urlt->mailbox == NULL) {
test_out_quiet(t_strdup_printf("url->mailbox = %s",
urlp->mailbox),
urlp->mailbox == urlt->mailbox);
} else {
test_out_quiet(t_strdup_printf("url->mailbox = %s",
urlp->mailbox),
strcmp(urlp->mailbox, urlt->mailbox) == 0);
}
test_out_quiet(t_strdup_printf("url->uidvalidity = %u",
urlp->uidvalidity),
urlp->uidvalidity == urlt->uidvalidity);
test_out_quiet(t_strdup_printf("url->uid = %u", urlp->uid),
urlp->uid == urlt->uid);
if (urlp->section == NULL || urlt->section == NULL) {
test_out_quiet(t_strdup_printf("url->section = %s",
urlp->section),
urlp->section == urlt->section);
} else {
test_out_quiet(t_strdup_printf("url->section = %s",
urlp->section),
strcmp(urlp->section, urlt->section) == 0);
}
test_out_quiet(t_strdup_printf(
"url->partial = %"PRIuUOFF_T".%"PRIuUOFF_T,
urlp->partial_offset, urlp->partial_size),
urlp->partial_offset == urlt->partial_offset &&
urlp->partial_size == urlt->partial_size);
if (urlp->search_program == NULL || urlt->search_program == NULL) {
test_out_quiet(t_strdup_printf(
"url->search_program = %s",
urlp->search_program),
urlp->search_program == urlt->search_program);
} else {
test_out_quiet(t_strdup_printf(
"url->search_program = %s",
urlp->search_program),
strcmp(urlp->search_program,
urlt->search_program) == 0);
}
if (urlt->uauth_rumpurl != NULL) {
if (urlp->uauth_rumpurl == NULL) {
test_out_quiet("url->uauth_rumpurl = NULL", FALSE);
} else {
test_out_quiet(t_strdup_printf(
"url->uauth_rumpurl = %s",
urlp->uauth_rumpurl),
strcmp(urlp->uauth_rumpurl,
urlt->uauth_rumpurl) == 0);
}
if (urlp->uauth_access_application == NULL ||
urlt->uauth_access_application == NULL) {
test_out_quiet(
t_strdup_printf(
"url->uauth_access_application = %s",
urlp->uauth_access_application),
(urlp->uauth_access_application ==
urlt->uauth_access_application));
} else {
test_out_quiet(
t_strdup_printf(
"url->uauth_access_application = %s",
urlp->uauth_access_application),
strcmp(urlp->uauth_access_application,
urlt->uauth_access_application) == 0);
}
if (urlp->uauth_access_user == NULL ||
urlt->uauth_access_user == NULL) {
test_out_quiet(
t_strdup_printf(
"url->uauth_access_user = %s",
urlp->uauth_access_user),
(urlp->uauth_access_user ==
urlt->uauth_access_user));
} else {
test_out_quiet(t_strdup_printf(
"url->uauth_access_user = %s",
urlp->uauth_access_user),
strcmp(urlp->uauth_access_user,
urlt->uauth_access_user) == 0);
}
if (urlp->uauth_mechanism == NULL ||
urlt->uauth_mechanism == NULL) {
test_out_quiet(t_strdup_printf(
"url->uauth_mechanism = %s",
urlp->uauth_mechanism),
(urlp->uauth_mechanism ==
urlt->uauth_mechanism));
} else {
test_out_quiet(t_strdup_printf(
"url->uauth_mechanism = %s",
urlp->uauth_mechanism),
strcmp(urlp->uauth_mechanism,
urlt->uauth_mechanism) == 0);
}
if (urlp->uauth_token == NULL || urlt->uauth_token == NULL) {
test_out_quiet(t_strdup_printf(
"url->uauth_token = %s",
urlp->uauth_token),
urlp->uauth_token == urlt->uauth_token);
} else {
bool equal = (urlp->uauth_token_size ==
urlt->uauth_token_size);
size_t i;
test_out_quiet(t_strdup_printf(
"url->uauth_token_size = %zu",
urlp->uauth_token_size), equal);
if (equal) {
for (i = 0; i < urlp->uauth_token_size; i++) {
if (urlp->uauth_token[i] !=
urlt->uauth_token[i]) {
equal = FALSE;
break;
}
}
test_out_quiet(t_strdup_printf(
"url->uauth_token [index=%d]", (int)i),
equal);
}
}
}
}
static void test_imap_url_valid(void)
{
unsigned int i;
for (i = 0; i < valid_url_test_count; i++) T_BEGIN {
const struct valid_imap_url_test *test = &valid_url_tests[i];
const char *url = test->url;
enum imap_url_parse_flags flags = test->flags;
const struct imap_url *urlt = &test->url_parsed;
const struct imap_url *urlb = &test->url_base;
struct imap_url *urlp;
const char *end = NULL, *error = NULL;
const char **end_p = (test->url_parsed_end == NULL ?
NULL : &end);
test_begin(t_strdup_printf("imap url valid [%d]", i));
if (urlb->host.name == NULL) urlb = NULL;
if (imap_url_parse_prefix(url, urlb, flags,
end_p, &urlp, &error) < 0)
urlp = NULL;
test_out_reason(t_strdup_printf("imap_url_parse(%s)",
valid_url_tests[i].url), urlp != NULL, error);
if (urlp != NULL) {
test_imap_url_valid_check(urlt, urlp);
test_out_quiet(t_strdup_printf("end = %s", end),
null_strcmp(end,
test->url_parsed_end) == 0);
}
test_end();
} T_END;
}
struct invalid_imap_url_test {
const char *url;
enum imap_url_parse_flags flags;
struct imap_url url_base;
};
static const struct invalid_imap_url_test invalid_url_tests[] = {
{
.url = "http:
},
{
.url = "imap:/INBOX",
},
{
.url = "imap:
},
{
.url = "imap:
.flags = IMAP_URL_PARSE_REQUIRE_RELATIVE,
.url_base = {
.host = { .name = "example.com" },
.userid = "user",
},
},
{
.url = "",
},
{
.url = "/INBOX/;UID=377",
},
{
.url = "imap:
"/INBOX/;UID=377/;SECTION=TEXT?ALL",
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
"/;UID=34/;PARTIAL=0.23409823409820938409823",
},
{
.url = "imap:
},
{
.url = "imap:
},
{
.url = "imap:
"/INBOX/;UID=377;EXPIRE=2010-02-02T12:00:12Z",
},
{
.url = "imap:
";URLAUTH=anonymous:internal:"
"0ad89fafd79f54afe4523f45aadf2afe",
},
{
.url = "imap:
";UID=377;EXPIRE=2011-15-02T00:00:00Z"
";URLAUTH=anonymous:internal:"
"0ad89fafd79f54afe4523f45aadf2afe",
.flags = IMAP_URL_PARSE_ALLOW_URLAUTH,
},
{
.url = "imap:
";UID=377;EXPIRE=2011-10-02T00:00:00Z",
.flags = IMAP_URL_PARSE_ALLOW_URLAUTH,
},
{
.url = "/INBOX/;UID=377;EXPIRE=2011-10-02T00:00:00Z"
";URLAUTH=anonymous:internal:"
"0ad89fafd79f54afe4523f45aadf2afe",
.flags = IMAP_URL_PARSE_ALLOW_URLAUTH,
},
{
.url = "imap:
.flags = IMAP_URL_PARSE_ALLOW_URLAUTH,
},
{
.url = "imap:
";URLAUTH=:internal:0ad89fafd79f54afe4523f45aadf2afe",
.flags = IMAP_URL_PARSE_ALLOW_URLAUTH,
},
{
.url = "imap:
";URLAUTH=user+:internal:"
"0ad89fafd79f54afe4523f45aadf2afe",
.flags = IMAP_URL_PARSE_ALLOW_URLAUTH,
},
{
.url = "imap:
";URLAUTH=+frop:internal:"
"0ad89fafd79f54afe4523f45aadf2afe",
.flags = IMAP_URL_PARSE_ALLOW_URLAUTH,
},
{
.url = "imap:
.flags = IMAP_URL_PARSE_ALLOW_URLAUTH,
},
{
.url = "imap:
";URLAUTH=anonymous::0ad89fafd79f54afe4523f45aadf2afe",
.flags = IMAP_URL_PARSE_ALLOW_URLAUTH,
},
{
.url = "imap:
";UID=377;URLAUTH=anonymous:internal:",
.flags = IMAP_URL_PARSE_ALLOW_URLAUTH,
},
{
.url = "imap:
";URLAUTH=anonymous:internal:fd79f54afe4523",
.flags = IMAP_URL_PARSE_ALLOW_URLAUTH,
},
{
.url = "imap:
";UID=377;EXPIRE=2011-10-02T00:00:00Z"
";URLAUTH=anonymous:internal:"
"0ad89fafd79f54afe4523q45aadf2afe",
.flags = IMAP_URL_PARSE_ALLOW_URLAUTH,
},
};
static const unsigned int invalid_url_test_count = N_ELEMENTS(invalid_url_tests);
static void test_imap_url_invalid(void)
{
unsigned int i;
for (i = 0; i < invalid_url_test_count; i++) T_BEGIN {
const char *url = invalid_url_tests[i].url;
enum imap_url_parse_flags flags = invalid_url_tests[i].flags;
const struct imap_url *urlb = &invalid_url_tests[i].url_base;
struct imap_url *urlp;
const char *error = NULL;
if (urlb->host.name == NULL)
urlb = NULL;
test_begin(t_strdup_printf("imap url invalid [%d]", i));
if (imap_url_parse(url, urlb, flags, &urlp, &error) < 0)
urlp = NULL;
test_assert(urlp != NULL || error != NULL);
test_out_reason(t_strdup_printf("parse %s", url), urlp == NULL, error);
test_end();
} T_END;
}
static const char *parse_create_url_tests[] = {
"imap:
"imap:
"imap:
"imap:
"imap:
"imap:
"imap:
"imap:
"imap:
"imap:
"imap:
"/;SECTION=TEXT",
"imap:
"/;SECTION=TEXT/;PARTIAL=1",
"imap:
"/;SECTION=TEXT/;PARTIAL=1.14",
"imap:
"/;SECTION=TEXT/;PARTIAL=1.14;URLAUTH=anonymous",
"imap:
"/;SECTION=TEXT/;PARTIAL=1.14;URLAUTH=user+username",
"imap:
"imap:
"imap:
"imap:
"imap:
"imap:
"imap:
"/;SECTION=HEADER.FIELDS%20(DATE%20FROM)",
"imap:
"/;SECTION=TEXT/;PARTIAL=1.14;URLAUTH=user+user%3bname",
};
static const unsigned int
parse_create_url_test_count = N_ELEMENTS(parse_create_url_tests);
static void test_imap_url_parse_create(void)
{
unsigned int i;
for (i = 0; i < parse_create_url_test_count; i++) T_BEGIN {
const char *url = parse_create_url_tests[i];
struct imap_url *urlp;
const char *error = NULL;
test_begin(t_strdup_printf("imap url parse/create [%d]", i));
if (imap_url_parse(url, NULL, IMAP_URL_PARSE_ALLOW_URLAUTH,
&urlp, &error) < 0)
urlp = NULL;
test_out_reason(t_strdup_printf("parse  %s", url),
urlp != NULL, error);
if (urlp != NULL) {
const char *urlnew = imap_url_create(urlp);
test_out(t_strdup_printf("create %s", urlnew),
strcmp(url, urlnew) == 0);
}
test_end();
} T_END;
}
int main(void)
{
static void (*const test_functions[])(void) = {
test_imap_url_valid,
test_imap_url_invalid,
test_imap_url_parse_create,
NULL
};
return test_run(test_functions);
}