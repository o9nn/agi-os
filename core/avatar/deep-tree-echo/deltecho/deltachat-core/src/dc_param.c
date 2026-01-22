#include <stdlib.h>
#include <string.h>
#include "dc_context.h"
#include "dc_tools.h"
static char* find_param(char* haystack, int key, char** ret_p2)
{
char* p1 = NULL;
char* p2 = NULL;
p1 = haystack;
while (1) {
if (p1==NULL || *p1==0) {
return NULL;
}
else if (*p1==key && p1[1]=='=') {
break;
}
else {
p1 = strchr(p1, '\n');
if (p1) {
p1++;
}
}
}
p2 = strchr(p1, '\n');
if (p2==NULL) {
p2 = &p1[strlen(p1)];
}
*ret_p2 = p2;
return p1;
}
dc_param_t* dc_param_new()
{
dc_param_t* param = NULL;
if ((param=calloc(1, sizeof(dc_param_t)))==NULL) {
exit(28);
}
param->packed = calloc(1, 1);
return param;
}
void dc_param_unref(dc_param_t* param)
{
if (param==NULL) {
return;
}
dc_param_empty(param);
free(param->packed);
free(param);
}
void dc_param_empty(dc_param_t* param)
{
if (param==NULL) {
return;
}
param->packed[0] = 0;
}
void dc_param_set_packed(dc_param_t* param, const char* packed)
{
if (param==NULL) {
return;
}
dc_param_empty(param);
if (packed) {
free(param->packed);
param->packed = dc_strdup(packed);
}
}
void dc_param_set_urlencoded(dc_param_t* param, const char* urlencoded)
{
if (param==NULL) {
return;
}
dc_param_empty(param);
if (urlencoded) {
free(param->packed);
param->packed = dc_strdup(urlencoded);
dc_str_replace(&param->packed, "&", "\n");
}
}
int dc_param_exists(dc_param_t* param, int key)
{
char *p2 = NULL;
if (param==NULL || key==0) {
return 0;
}
return find_param(param->packed, key, &p2)? 1 : 0;
}
char* dc_param_get(const dc_param_t* param, int key, const char* def)
{
char* p1 = NULL;
char* p2 = NULL;
char  bak = 0;
char* ret = NULL;
if (param==NULL || key==0) {
return def? dc_strdup(def) : NULL;
}
p1 = find_param(param->packed, key, &p2);
if (p1==NULL) {
return def? dc_strdup(def) : NULL;
}
p1 += 2;
bak = *p2;
*p2 = 0;
ret = dc_strdup(p1);
dc_rtrim(ret);
*p2 = bak;
return ret;
}
int32_t dc_param_get_int(const dc_param_t* param, int key, int32_t def)
{
if (param==NULL || key==0) {
return def;
}
char* str = dc_param_get(param, key, NULL);
if (str==NULL) {
return def;
}
int32_t ret = atol(str);
free(str);
return ret;
}
double dc_param_get_float(const dc_param_t* param, int key, double def)
{
if (param==NULL || key==0) {
return def;
}
char* str = dc_param_get(param, key, NULL);
if (str==NULL) {
return def;
}
double ret = dc_atof(str);
free(str);
return ret;
}
void dc_param_set(dc_param_t* param, int key, const char* value)
{
char* old1 = NULL;
char* old2 = NULL;
char* new1 = NULL;
if (param==NULL || key==0) {
return;
}
old1 = param->packed;
old2 = NULL;
if (old1) {
char *p1, *p2;
p1 = find_param(old1, key, &p2);
if (p1 != NULL) {
*p1 = 0;
old2 = p2;
}
else if (value==NULL) {
return;
}
}
dc_rtrim(old1);
dc_ltrim(old2);
if (old1 && old1[0]==0) { old1 = NULL; }
if (old2 && old2[0]==0) { old2 = NULL; }
if (value) {
new1 = dc_mprintf("%s%s%c=%s%s%s",
old1?  old1 : "",
old1?  "\n" : "",
key,
value,
old2?  "\n" : "",
old2?  old2 : "");
}
else {
new1 = dc_mprintf("%s%s%s",
old1?         old1 : "",
(old1&&old2)? "\n" : "",
old2?         old2 : "");
}
free(param->packed);
param->packed = new1;
}
void dc_param_set_int(dc_param_t* param, int key, int32_t value)
{
if (param==NULL || key==0) {
return;
}
char* value_str = dc_mprintf("%i", (int)value);
if (value_str==NULL) {
return;
}
dc_param_set(param, key, value_str);
free(value_str);
}
void dc_param_set_float(dc_param_t* param, int key, double value)
{
if (param==NULL || key==0) {
return;
}
char* value_str = dc_ftoa(value);
if (value_str==NULL) {
return;
}
dc_param_set(param, key, value_str);
free(value_str);
}