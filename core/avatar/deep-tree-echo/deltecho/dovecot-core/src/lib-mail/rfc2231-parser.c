#include "lib.h"
#include "array.h"
#include "str.h"
#include "rfc822-parser.h"
#include "rfc2231-parser.h"
struct rfc2231_parameter {
const char *key, *value;
unsigned int idx;
bool extended;
};
static int rfc2231_parameter_cmp(const struct rfc2231_parameter *r1,
const struct rfc2231_parameter *r2)
{
int ret;
ret = strcmp(r1->key, r2->key);
if (ret != 0)
return ret;
return r1->idx < r2->idx ? -1 :
(r1-> idx > r2->idx ? 1 : 0);
}
static void rfc2231_escape(string_t *dest, const char *src)
{
for (; *src != '\0'; src++) {
if (*src == '%')
str_append(dest, "%25");
else
str_append_c(dest, *src);
}
}
int rfc2231_parse(struct rfc822_parser_context *ctx,
const char *const **result_r)
{
ARRAY_TYPE(const_string) result;
ARRAY(struct rfc2231_parameter) rfc2231_params_arr;
struct rfc2231_parameter rfc2231_param;
const struct rfc2231_parameter *rfc2231_params;
const char *key, *p, *p2;
string_t *str;
unsigned int i, j, count, next, next_idx;
bool ok, have_extended, broken = FALSE;
const char *prev_replacement_str;
int ret;
prev_replacement_str = ctx->nul_replacement_str;
ctx->nul_replacement_str = RFC822_NUL_REPLACEMENT_STR;
i_zero(&rfc2231_param);
t_array_init(&result, 8);
t_array_init(&rfc2231_params_arr, 8);
str = t_str_new(64);
while ((ret = rfc822_parse_content_param(ctx, &key, str)) != 0) {
if (ret < 0) {
broken = TRUE;
if (ctx->data >= ctx->end)
break;
ctx->data++;
continue;
}
p = strchr(key, '*');
if (p != NULL) {
p2 = p;
if (p[1] != '\0') {
p++;
rfc2231_param.idx = 0;
for (; *p >= '0' && *p <= '9'; p++) {
rfc2231_param.idx =
rfc2231_param.idx*10 + *p - '0';
}
}
if (*p != '*')
rfc2231_param.extended = FALSE;
else {
rfc2231_param.extended = TRUE;
p++;
}
if (*p != '\0')
p = NULL;
else {
rfc2231_param.key = t_strdup_until(key, p2);
rfc2231_param.value = t_strdup(str_c(str));
array_push_back(&rfc2231_params_arr,
&rfc2231_param);
}
}
if (p == NULL) {
const char *value = t_strdup(str_c(str));
array_push_back(&result, &key);
array_push_back(&result, &value);
}
}
ctx->nul_replacement_str = prev_replacement_str;
if (array_count(&rfc2231_params_arr) == 0) {
array_append_zero(&result);
*result_r = array_front(&result);
return broken ? -1 : 0;
}
array_sort(&rfc2231_params_arr, rfc2231_parameter_cmp);
rfc2231_params = array_get(&rfc2231_params_arr, &count);
for (i = 0; i < count; i = next) {
ok = TRUE;
have_extended = FALSE;
next_idx = 0;
for (j = i; j < count; j++) {
if (strcasecmp(rfc2231_params[i].key,
rfc2231_params[j].key) != 0)
break;
if (rfc2231_params[j].idx != next_idx) {
ok = FALSE;
}
if (rfc2231_params[j].extended)
have_extended = TRUE;
next_idx++;
}
next = j;
if (!ok) {
for (j = i; j < next; j++) {
key = t_strdup_printf(
rfc2231_params[j].extended ?
"%s*%u*" : "%s*%u",
rfc2231_params[j].key,
rfc2231_params[j].idx);
array_push_back(&result, &key);
array_push_back(&result,
&rfc2231_params[j].value);
}
} else {
str_truncate(str, 0);
if (!rfc2231_params[i].extended && have_extended)
str_append(str, "''");
for (j = i; j < next; j++) {
if (!rfc2231_params[j].extended &&
have_extended) {
rfc2231_escape(str,
rfc2231_params[j].value);
} else {
str_append(str,
rfc2231_params[j].value);
}
}
key = rfc2231_params[i].key;
if (have_extended)
key = t_strconcat(key, "*", NULL);
const char *value = t_strdup(str_c(str));
array_push_back(&result, &key);
array_push_back(&result, &value);
}
}
array_append_zero(&result);
*result_r = array_front(&result);
return broken ? -1 : 0;
}