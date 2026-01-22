#include "lib.h"
#include "net.h"
#include "http-url.h"
#include "test-common.h"
struct valid_http_url_test {
const char *url;
enum http_url_parse_flags flags;
struct http_url url_base;
struct http_url url_parsed;
};
static struct valid_http_url_test valid_url_tests[] = {
{
.url = "http:
.url_parsed = {
.host = { .name = "localhost" },
},
},
{
.url = "http:
.url_parsed = {
.host = { .name = "www.example.com" },
},
},
{
.url = "http:
.url_parsed = {
.host = { .name = "www.dovecot.org" },
.port = 8080,
},
},
{
.url = "http:
.url_parsed = {
.host = {
.name = "127.0.0.1",
.ip = { .family = AF_INET },
},
},
},
{
.url = "http:
.url_parsed = {
.host = {
.name = "[::1]",
.ip = { .family = AF_INET6 },
},
},
},
{
.url = "http:
.url_parsed = {
.host = {
.name = "[::1]",
.ip = { .family = AF_INET6 },
},
.port = 8080,
},
},
{
.url = "http:
.flags = HTTP_URL_ALLOW_USERINFO_PART,
.url_parsed = {
.host = { .name = "api.dovecot.org" },
.user = "user",
},
},
{
.url = "http:
.flags = HTTP_URL_ALLOW_USERINFO_PART,
.url_parsed = {
.host = { .name = "api.dovecot.org" },
.user = "userid",
.password = "secret",
},
},
{
.url = "http:
.flags = HTTP_URL_ALLOW_USERINFO_PART,
.url_parsed = {
.host = { .name = "api.dovecot.org" },
.user = "su:userid",
.password = "secret",
},
},
{
.url = "http:
"?question=What%20are%20you%20doing%3f&answer=Nothing.",
.url_parsed = {
.path = "/",
.host = { .name = "www.example.com" },
.enc_query = "question=What%20are%20you%20doing%3f&answer=Nothing.",
},
},
{
.url = "http:
.url_parsed = {
.path = "
.host = { .name = "target" },
},
},
{
.url = "http:
.url_parsed = {
.path = "
.host = { .name = "target" },
},
},
{
.url = "http:
.url_parsed = {
.path = "
.host = { .name = "target" },
},
},
{
.url = "http:
.url_parsed = {
.path = "
.host = { .name = "target" },
},
},
{
.url = "http:
.url_parsed = {
.path = "
.host = { .name = "target" },
},
},
{
.url = "http:
.url_parsed = {
.path = "
.host = { .name = "target" },
},
},
{
.url = "/index.php",
.url_base = {
.host = { .name = "target" },
},
.url_parsed = {
.host = { .name = "target" },
.path = "/index.php",
},
},
{
.url = "
.url_base = {
.host = { .name = "target" },
},
.url_parsed = {
.host = { .name = "index.php" },
},
},
{
.url = "/path/to/index.php",
.url_base = {
.host = { .name = "target" },
},
.url_parsed = {
.host = { .name = "target" },
.path = "/path/to/index.php",
},
},
{
.url = "
.url_base = {
.host = { .name = "target" },
},
.url_parsed = {
.host = { .name = "path" },
.path = "
},
},
{
.url = "http:
.url_parsed = {
.path = "/that/reverts/to/DNS",
.host = { .name = "256.0.0.1" },
},
},
{
.url = "http:
.url_parsed = {
.path = "/this/also/reverts/to/DNS",
.host = { .name = "127.0.0.284" },
},
},
{
.url = "http:
.flags = HTTP_URL_ALLOW_FRAGMENT_PART,
.url_parsed = {
.path = "/",
.host = { .name = "www.example.com" },
.enc_fragment = "Status%20of%20development",
},
},
{
.url = "g",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/g",
},
},
{
.url = "./g",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/g",
},
},
{
.url = "g/",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/g/",
},
},
{
.url = "/g",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/g",
},
},
{
.url = "
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "g" },
},
},
{
.url = "?y",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "y",
},
},
{
.url = "g?y",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/g",
.enc_query = "y",
},
},
{
.url = "#s",
.flags = HTTP_URL_ALLOW_FRAGMENT_PART,
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
.enc_fragment = "s",
},
},
{
.url = "g#s",
.flags = HTTP_URL_ALLOW_FRAGMENT_PART,
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/g",
.enc_fragment = "s",
},
},
{
.url = "g?y#s",
.flags = HTTP_URL_ALLOW_FRAGMENT_PART,
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/g",
.enc_query = "y",
.enc_fragment = "s",
},
},
{
.url = ";x",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/;x",
},
},
{
.url = "g;x",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/g;x",
},
},
{
.url = "g;x?y#s",
.flags = HTTP_URL_ALLOW_FRAGMENT_PART,
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/g;x",
.enc_query = "y",
.enc_fragment = "s",
},
},
{
.url = "",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
},
{
.url = ".",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/",
},
},
{
.url = "./",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/",
},
},
{
.url = "..",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/",
},
},
{
.url = "../",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/",
},
},
{
.url = "../g",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/g",
},
},
{
.url = "../..",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/",
},
},
{
.url = "../../",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/",
},
},
{
.url = "../../g",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/g",
},
},
{
.url = "../../../g",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/g",
},
},
{
.url = "../../../../g",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/g",
},
},
{
.url = "/./g",
.url_base = {
.host = {.name = "a"},
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = {.name = "a"},
.path = "/g",
},
},
{
.url = "/../g",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/g",
},
},
{
.url = "g.",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/g.",
},
},
{
.url = ".g",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/.g",
},
},
{
.url = "g..",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/g..",
},
},
{
.url = "..g",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/..g",
},
},
{
.url = "./../g",
.url_base = {
.host = {.name = "a"},
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = {.name = "a"},
.path = "/b/g",
},
},
{
.url = "./g/.",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/g/",
},
},
{
.url = "g/./h",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/g/h",
},
},
{
.url = "g/../h",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/h",
},
},
{
.url = "g;x=1/./y",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/g;x=1/y",
},
},
{
.url = "g;x=1/../y",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/y",
},
},
{
.url = "g?y/./x",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/g",
.enc_query = "y/./x",
},
},
{
.url = "g?y/../x",
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed = {
.host = { .name = "a" },
.path = "/b/c/g",
.enc_query = "y/../x",
},
},
{
.url = "g#s/./x",
.flags = HTTP_URL_ALLOW_FRAGMENT_PART,
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed =
{
.host = { .name = "a" },
.path = "/b/c/g",
.enc_fragment = "s/./x",
},
},
{
.url = "g#s/../x",
.flags = HTTP_URL_ALLOW_FRAGMENT_PART,
.url_base = {
.host = { .name = "a" },
.path = "/b/c/d;p",
.enc_query = "q",
},
.url_parsed =
{
.host = { .name = "a" },
.path = "/b/c/g",
.enc_fragment = "s/../x",
},
}
};
static unsigned int valid_url_test_count = N_ELEMENTS(valid_url_tests);
static void
test_http_url_equal(struct http_url *urlt, struct http_url *urlp)
{
if (urlp->host.name == NULL || urlt->host.name == NULL) {
test_assert(urlp->host.name == urlt->host.name);
} else {
test_assert(strcmp(urlp->host.name, urlt->host.name) == 0);
}
test_assert(urlp->port == urlt->port);
test_assert(urlp->host.ip.family == urlt->host.ip.family);
if (urlp->user == NULL || urlt->user == NULL) {
test_assert(urlp->user == urlt->user);
} else {
test_assert(strcmp(urlp->user, urlt->user) == 0);
}
if (urlp->password == NULL || urlt->password == NULL) {
test_assert(urlp->password == urlt->password);
} else {
test_assert(strcmp(urlp->password, urlt->password) == 0);
}
if (urlp->path == NULL || urlt->path == NULL) {
test_assert(urlp->path == urlt->path);
} else {
test_assert(strcmp(urlp->path, urlt->path) == 0);
}
if (urlp->enc_query == NULL || urlt->enc_query == NULL) {
test_assert(urlp->enc_query == urlt->enc_query);
} else {
test_assert(strcmp(urlp->enc_query, urlt->enc_query) == 0);
}
if (urlp->enc_fragment == NULL || urlt->enc_fragment == NULL) {
test_assert(urlp->enc_fragment == urlt->enc_fragment);
} else {
test_assert(strcmp(urlp->enc_fragment,
urlt->enc_fragment) == 0);
}
}
static void test_http_url_valid(void)
{
unsigned int i;
for (i = 0; i < valid_url_test_count; i++) T_BEGIN {
const char *url = valid_url_tests[i].url;
enum http_url_parse_flags flags = valid_url_tests[i].flags;
struct http_url *urlt = &valid_url_tests[i].url_parsed;
struct http_url *urlb = &valid_url_tests[i].url_base;
struct http_url *urlp;
const char *error = NULL;
test_begin(t_strdup_printf("http url valid [%d]", i));
if (urlb->host.name == NULL) urlb = NULL;
if (http_url_parse(url, urlb, flags, pool_datastack_create(),
&urlp, &error) < 0)
urlp = NULL;
test_out_reason(t_strdup_printf("http_url_parse(%s)",
valid_url_tests[i].url), urlp != NULL, error);
if (urlp != NULL)
test_http_url_equal(urlt, urlp);
test_end();
} T_END;
}
struct invalid_http_url_test {
const char *url;
enum http_url_parse_flags flags;
struct http_url url_base;
};
static struct invalid_http_url_test invalid_url_tests[] = {
{
.url = "imap:
},
{
.url = "http:/www.example.com"
},
{
.url = "http:
},
{
.url = ""
},
{
.url = "/index.html"
},
{
.url = "http:
},
{
.url = "http:
},
{
.url = "http:
},
{
.url = "http:
},
{
.url = "http:
},
{
.url = "http:
},
{
.url = "http:
},
{
.url = "http:
},
{
.url = "http:
},
{
.url = "http:
},
{
.url = "http:
},
{
.url = "http:
},
{
.url = "http:
},
{
.url = "http:
},
{
.url = "http:
},
{
.url = "http:
.flags = HTTP_URL_ALLOW_FRAGMENT_PART
}
};
static unsigned int invalid_url_test_count = N_ELEMENTS(invalid_url_tests);
static void test_http_url_invalid(void)
{
unsigned int i;
for (i = 0; i < invalid_url_test_count; i++) T_BEGIN {
const char *url = invalid_url_tests[i].url;
enum http_url_parse_flags flags = invalid_url_tests[i].flags;
struct http_url *urlb = &invalid_url_tests[i].url_base;
struct http_url *urlp;
const char *error = NULL;
if (urlb->host.name == NULL)
urlb = NULL;
test_begin(t_strdup_printf("http url invalid [%d]", i));
if (http_url_parse(url, urlb, flags,
pool_datastack_create(), &urlp, &error) < 0)
urlp = NULL;
test_out_reason(t_strdup_printf("parse %s", url),
urlp == NULL, error);
test_end();
} T_END;
}
static const char *parse_create_url_tests[] = {
"http:
"http:
"http:
"http:
"http:
"http:
"http:
"http:
"http:
};
static unsigned int
parse_create_url_test_count = N_ELEMENTS(parse_create_url_tests);
static void test_http_url_parse_create(void)
{
unsigned int i;
for (i = 0; i < parse_create_url_test_count; i++) T_BEGIN {
const char *url = parse_create_url_tests[i];
struct http_url *urlp;
const char *error = NULL;
test_begin(t_strdup_printf("http url parse/create [%d]", i));
if (http_url_parse
(url, NULL, HTTP_URL_ALLOW_FRAGMENT_PART,
pool_datastack_create(), &urlp, &error) < 0)
urlp = NULL;
test_out_reason(t_strdup_printf("parse  %s", url),
urlp != NULL, error);
if (urlp != NULL) {
const char *urlnew = http_url_create(urlp);
test_out(t_strdup_printf("create %s", urlnew),
strcmp(url, urlnew) == 0);
}
test_end();
} T_END;
}
int main(void)
{
static void (*const test_functions[])(void) = {
test_http_url_valid,
test_http_url_invalid,
test_http_url_parse_create,
NULL
};
return test_run(test_functions);
}