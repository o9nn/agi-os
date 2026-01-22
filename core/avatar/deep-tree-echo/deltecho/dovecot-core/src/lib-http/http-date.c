#include "lib.h"
#include "str.h"
#include "utc-mktime.h"
#include "http-date.h"
#include <ctype.h>
static const char *month_names[] = {
"Jan", "Feb", "Mar", "Apr", "May", "Jun",
"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};
static const char *weekday_names[] = {
"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
};
static const char *weekday_names_long[] = {
"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
};
struct http_date_parser {
const unsigned char *cur, *end;
struct tm tm;
int timezone_offset;
};
static inline int
http_date_parse_sp(struct http_date_parser *parser)
{
if (parser->cur >= parser->end)
return -1;
if (parser->cur[0] != ' ')
return 0;
parser->cur++;
return 1;
}
static inline int
http_date_parse_number(struct http_date_parser *parser,
int digits, int *number_r)
{
int i;
if (parser->cur >= parser->end || !i_isdigit(parser->cur[0]))
return 0;
*number_r = parser->cur[0] - '0';
parser->cur++;
for (i=0; i < digits-1; i++) {
if (parser->cur >= parser->end || !i_isdigit(parser->cur[0]))
return -1;
*number_r = ((*number_r) * 10) + parser->cur[0] - '0';
parser->cur++;
}
return 1;
}
static inline int
http_date_parse_word(struct http_date_parser *parser,
int maxchars, string_t **word_r)
{
string_t *word;
int i;
if (parser->cur >= parser->end || !i_isalpha(parser->cur[0]))
return 0;
word = t_str_new(maxchars);
str_append_c(word, parser->cur[0]);
parser->cur++;
for (i=0; i < maxchars-1; i++) {
if (parser->cur >= parser->end || !i_isalpha(parser->cur[0]))
break;
str_append_c(word, parser->cur[0]);
parser->cur++;
}
if (parser->cur < parser->end && i_isalpha(parser->cur[0]))
return -1;
*word_r = word;
return 1;
}
static inline int
http_date_parse_year(struct http_date_parser *parser)
{
if (http_date_parse_number(parser, 4, &parser->tm.tm_year) <= 0)
return -1;
if (parser->tm.tm_year < 1900)
return -1;
parser->tm.tm_year -= 1900;
return 1;
}
static inline int
http_date_parse_month(struct http_date_parser *parser)
{
string_t *month;
int i;
if (http_date_parse_word(parser, 3, &month) <= 0 || str_len(month) != 3)
return -1;
for (i = 0; i < 12; i++) {
if (strcmp(month_names[i], str_c(month)) == 0) {
break;
}
}
if (i >= 12)
return -1;
parser->tm.tm_mon = i;
return 1;
}
static inline int
http_date_parse_day(struct http_date_parser *parser)
{
if (http_date_parse_number(parser, 2, &parser->tm.tm_mday) <= 0)
return -1;
return 1;
}
static int
http_date_parse_time_of_day(struct http_date_parser *parser)
{
if (http_date_parse_number(parser, 2, &parser->tm.tm_hour) <= 0)
return -1;
if (parser->cur >= parser->end || parser->cur[0] != ':')
return -1;
parser->cur++;
if (http_date_parse_number(parser, 2, &parser->tm.tm_min) <= 0)
return -1;
if (parser->cur >= parser->end || parser->cur[0] != ':')
return -1;
parser->cur++;
if (http_date_parse_number(parser, 2, &parser->tm.tm_sec) <= 0)
return -1;
return 1;
}
static inline int
http_date_parse_time_gmt(struct http_date_parser *parser)
{
string_t *gmt;
if (http_date_parse_sp(parser) <= 0)
return -1;
if (http_date_parse_time_of_day(parser) <= 0)
return -1;
if (http_date_parse_sp(parser) <= 0)
return -1;
if (http_date_parse_word(parser, 3, &gmt) <= 0 ||
strcmp("GMT", str_c(gmt)) != 0)
return -1;
return 1;
}
static int
http_date_parse_format_imf_fixdate(struct http_date_parser *parser)
{
if (http_date_parse_sp(parser) <= 0)
return -1;
if (http_date_parse_day(parser) <= 0)
return -1;
if (http_date_parse_sp(parser) <= 0)
return -1;
if (http_date_parse_month(parser) <= 0)
return -1;
if (http_date_parse_sp(parser) <= 0)
return -1;
if (http_date_parse_year(parser) <= 0)
return -1;
return http_date_parse_time_gmt(parser);
}
static int
http_date_parse_format_rfc850(struct http_date_parser *parser)
{
if (parser->cur >= parser->end || parser->cur[0] != ',')
return -1;
parser->cur++;
if (http_date_parse_sp(parser) <= 0)
return -1;
if (http_date_parse_day(parser) <= 0)
return -1;
if (parser->cur >= parser->end || parser->cur[0] != '-')
return -1;
parser->cur++;
if (http_date_parse_month(parser) <= 0)
return -1;
if (parser->cur >= parser->end || parser->cur[0] != '-')
return -1;
parser->cur++;
if (http_date_parse_number(parser, 2, &parser->tm.tm_year) <= 0)
return -1;
if (parser->tm.tm_year < 70)
parser->tm.tm_year += 100;
return http_date_parse_time_gmt(parser);
}
static int
http_date_parse_format_asctime(struct http_date_parser *parser)
{
int ret;
if (http_date_parse_month(parser) <= 0)
return -1;
if (http_date_parse_sp(parser) <= 0)
return -1;
if ((ret=http_date_parse_sp(parser)) < 0)
return -1;
if (ret == 0) {
if (http_date_parse_number(parser, 2, &parser->tm.tm_mday) <= 0)
return -1;
} else {
if (http_date_parse_number(parser, 1, &parser->tm.tm_mday) <= 0)
return -1;
}
if (http_date_parse_sp(parser) <= 0)
return -1;
if (http_date_parse_time_of_day(parser) <= 0)
return -1;
if (http_date_parse_sp(parser) <= 0)
return -1;
return http_date_parse_year(parser);
}
static int
http_date_parse_format_any(struct http_date_parser *parser)
{
string_t *dayname;
int i;
if (http_date_parse_word(parser, 9, &dayname) <= 0)
return -1;
if (str_len(dayname) > 3) {
for (i = 0; i < 7; i++) {
if (strcmp(weekday_names_long[i], str_c(dayname)) == 0) {
break;
}
}
if (i >= 7)
return -1;
return http_date_parse_format_rfc850(parser);
}
for (i = 0; i < 7; i++) {
if (strcmp(weekday_names[i], str_c(dayname)) == 0) {
break;
}
}
if (i >= 7 || parser->cur >= parser->end)
return -1;
if (parser->cur[0] == ' ') {
parser->cur++;
return http_date_parse_format_asctime(parser);
}
if (parser->cur[0] != ',')
return -1;
parser->cur++;
return http_date_parse_format_imf_fixdate(parser);
}
bool http_date_parse(const unsigned char *data, size_t size,
time_t *timestamp_r)
{
struct http_date_parser parser;
time_t timestamp;
i_zero(&parser);
parser.cur = data;
parser.end = data + size;
if (http_date_parse_format_any(&parser) <= 0)
return FALSE;
if (parser.cur != parser.end)
return FALSE;
parser.tm.tm_isdst = -1;
timestamp = utc_mktime(&parser.tm);
if (timestamp == (time_t)-1)
return FALSE;
*timestamp_r = timestamp;
return TRUE;
}
bool http_date_parse_tm(const unsigned char *data, size_t size,
struct tm *tm_r)
{
time_t timestamp;
struct tm *tm;
if (!http_date_parse(data, size, &timestamp))
return FALSE;
tm = gmtime(&timestamp);
*tm_r = *tm;
return TRUE;
}
const char *http_date_create_tm(struct tm *tm)
{
return t_strdup_printf("%s, %02d %s %04d %02d:%02d:%02d GMT",
weekday_names[tm->tm_wday],
tm->tm_mday,
month_names[tm->tm_mon],
tm->tm_year+1900,
tm->tm_hour, tm->tm_min, tm->tm_sec);
}
const char *http_date_create(time_t timestamp)
{
struct tm *tm;
tm = gmtime(&timestamp);
return http_date_create_tm(tm);
}