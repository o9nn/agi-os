#ifndef _ANALYZE_LINKAGE_H
#define _ANALYZE_LINKAGE_H
#include "api-types.h"
#include "link-includes.h"
void compute_link_names(Linkage, String_set *);
const char *intersect_strings(String_set *, const Connector *,
const Connector *);
#endif