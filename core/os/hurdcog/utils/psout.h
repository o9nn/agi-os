#ifndef __PSOUT_H__
#define __PSOUT_H__
#include <ps.h>
void psout (const struct proc_stat_list *procs,
const char *fmt_string, int posix_fmt,
const struct ps_fmt_specs *specs,
const char *sort_key_name, int sort_reverse,
int output_width, int print_heading,
int squash_bogus_fields, int squash_nominal_fields,
int top);
#endif