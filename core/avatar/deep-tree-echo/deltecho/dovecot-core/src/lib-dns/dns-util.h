#ifndef DNS_UTIL_H
#define DNS_UTIL_H 1
static inline char
dns_tolower(char c)
{
if (c >= 'A' && c <= 'Z')
c+='a'-'A';
return c;
}
int dns_compare(const char *a, const char *b) ATTR_PURE;
int dns_ncompare(const char *a, const char *b, size_t n) ATTR_PURE;
int dns_compare_labels(const char *a, const char *b) ATTR_PURE;
int dns_match_wildcard(const char *name, const char *mask) ATTR_PURE;
#endif