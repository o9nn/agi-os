#include <stdlib.h>
#include <string.h>
#include "dc_context.h"
#include "dc_simplify.h"
#include "dc_tools.h"
#include "dc_dehtml.h"
#include "dc_mimeparser.h"
#include "dc_strbuilder.h"
static int is_empty_line(const char* buf)
{
const unsigned char* p1 = (const unsigned char*)buf;
while (*p1) {
if (*p1 > ' ') {
return 0;
}
p1++;
}
return 1;
}
static int is_plain_quote(const char* buf)
{
if (buf[0]=='>') {
return 1;
}
return 0;
}
static int is_quoted_headline(const char* buf)
{
int buf_len = strlen(buf);
if (buf_len > 80) {
return 0;
}
if (buf_len > 0 && buf[buf_len-1]==':') {
return 1;
}
return 0;
}
dc_simplify_t* dc_simplify_new()
{
dc_simplify_t* simplify = NULL;
if ((simplify=calloc(1, sizeof(dc_simplify_t)))==NULL) {
exit(31);
}
return simplify;
}
void dc_simplify_unref(dc_simplify_t* simplify)
{
if (simplify==NULL) {
return;
}
free(simplify);
}
static char* dc_simplify_simplify_plain_text(dc_simplify_t* simplify,
const char* buf_terminated,
int is_msgrmsg)
{
carray* lines = dc_split_into_lines(buf_terminated);
int l = 0;
int l_first = 0;
int l_last = carray_count(lines)-1;
char* line = NULL;
{
int footer_mark = 0;
for (l = l_first; l <= l_last; l++)
{
line = (char*)carray_get(lines, l);
if (strcmp(line, "-- ")==0
|| strcmp(line, "--  ")==0) {
footer_mark = 1;
}
if (strcmp(line, "--")==0
|| strcmp(line, "---")==0
|| strcmp(line, "----")==0) {
footer_mark = 1;
simplify->is_cut_at_end = 1;
}
if (footer_mark) {
l_last = l - 1;
break;
}
}
}
if ((l_last-l_first+1) >= 3) {
char* line0 = (char*)carray_get(lines, l_first);
char* line1 = (char*)carray_get(lines, l_first+1);
char* line2 = (char*)carray_get(lines, l_first+2);
if (strcmp(line0, "---------- Forwarded message ----------")==0
&& strncmp(line1, "From: ", 6)==0
&& line2[0]==0)
{
simplify->is_forwarded = 1;
l_first += 3;
}
}
for (l = l_first; l <= l_last; l++)
{
line = (char*)carray_get(lines, l);
if (strncmp(line, "-----", 5)==0
|| strncmp(line, "_____", 5)==0
|| strncmp(line, "=====", 5)==0
|| strncmp(line, "*****", 5)==0
|| strncmp(line, "~~~~~", 5)==0)
{
l_last = l - 1;
simplify->is_cut_at_end = 1;
break;
}
}
if (!is_msgrmsg)
{
int l_lastQuotedLine = -1;
for (l = l_last; l >= l_first; l--) {
line = (char*)carray_get(lines, l);
if (is_plain_quote(line)) {
l_lastQuotedLine = l;
}
else if (!is_empty_line(line)) {
break;
}
}
if (l_lastQuotedLine != -1)
{
l_last = l_lastQuotedLine-1;
simplify->is_cut_at_end = 1;
if (l_last > 0) {
if (is_empty_line((char*)carray_get(lines, l_last))) {
l_last--;
}
}
if (l_last > 0) {
line = (char*)carray_get(lines, l_last);
if (is_quoted_headline(line)) {
l_last--;
}
}
}
}
if (!is_msgrmsg)
{
int l_lastQuotedLine = -1;
int hasQuotedHeadline = 0;
for (l = l_first; l <= l_last; l++) {
line = (char*)carray_get(lines, l);
if (is_plain_quote(line)) {
l_lastQuotedLine = l;
}
else if (!is_empty_line(line)) {
if (is_quoted_headline(line) && !hasQuotedHeadline && l_lastQuotedLine==-1) {
hasQuotedHeadline = 1;
}
else {
break;
}
}
}
if (l_lastQuotedLine != -1)
{
l_first = l_lastQuotedLine + 1;
simplify->is_cut_at_begin = 1;
}
}
dc_strbuilder_t ret;
dc_strbuilder_init(&ret, strlen(buf_terminated));
if (simplify->is_cut_at_begin) {
dc_strbuilder_cat(&ret, DC_EDITORIAL_ELLIPSE " ");
}
int pending_linebreaks = 0;
int content_lines_added = 0;
for (l = l_first; l <= l_last; l++)
{
line = (char*)carray_get(lines, l);
if (is_empty_line(line))
{
pending_linebreaks++;
}
else
{
if (content_lines_added)
{
if (pending_linebreaks > 2) { pending_linebreaks = 2; }
while (pending_linebreaks) {
dc_strbuilder_cat(&ret, "\n");
pending_linebreaks--;
}
}
dc_strbuilder_cat(&ret, line);
content_lines_added++;
pending_linebreaks = 1;
}
}
if (simplify->is_cut_at_end
&& (!simplify->is_cut_at_begin || content_lines_added) ) {
dc_strbuilder_cat(&ret, " " DC_EDITORIAL_ELLIPSE);
}
dc_free_splitted_lines(lines);
return ret.buf;
}
char* dc_simplify_simplify(dc_simplify_t* simplify, const char* in_unterminated,
int in_bytes, int is_html, int is_msgrmsg)
{
char* out = NULL;
char* temp = NULL;
if (simplify==NULL || in_unterminated==NULL || in_bytes <= 0) {
return dc_strdup("");
}
simplify->is_forwarded = 0;
simplify->is_cut_at_begin = 0;
simplify->is_cut_at_end = 0;
out = strndup((char*)in_unterminated, in_bytes);
if (out==NULL) {
return dc_strdup("");
}
if (is_html) {
if ((temp = dc_dehtml(out)) != NULL) {
free(out);
out = temp;
}
}
dc_remove_cr_chars(out);
if ((temp = dc_simplify_simplify_plain_text(simplify, out, is_msgrmsg)) != NULL) {
free(out);
out = temp;
}
dc_remove_cr_chars(out);
return out;
}