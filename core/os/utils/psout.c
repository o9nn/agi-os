#include <hurd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <error.h>
#include <ps.h>
void
psout (struct proc_stat_list *procs,
char *fmt_string, int posix_fmt, struct ps_fmt_specs *specs,
char *sort_key_name, int sort_reverse,
int output_width, int print_heading,
int squash_bogus_fields, int squash_nominal_fields,
int top)
{
error_t err;
struct ps_stream *output;
struct ps_fmt *fmt;
err = ps_fmt_create (fmt_string, posix_fmt, specs, &fmt);
if (err)
{
char *problem;
ps_fmt_creation_error (fmt_string, posix_fmt, specs, &problem);
error (4, 0, "%s", problem);
}
if (squash_bogus_fields)
{
ps_flags_t bogus_flags = ps_fmt_needs (fmt);
err = proc_stat_list_find_bogus_flags (procs, &bogus_flags);
if (err)
error (0, err, "Couldn't remove bogus fields");
else
ps_fmt_squash_flags (fmt, bogus_flags);
}
if (squash_nominal_fields)
{
int nominal (struct ps_fmt_field *field)
{
return !(field->flags & PS_FMT_FIELD_KEEP)
&& proc_stat_list_spec_nominal (procs, field->spec);
}
ps_fmt_squash (fmt, nominal);
}
if (sort_key_name)
{
const struct ps_fmt_spec *sort_key;
if (*sort_key_name == '-')
{
sort_reverse = 1;
sort_key_name++;
}
sort_key = ps_fmt_specs_find (specs, sort_key_name);
if (sort_key == NULL)
error (3, 0, "%s: bad sort key", sort_key_name);
err = proc_stat_list_sort (procs, sort_key, sort_reverse);
if (err)
error (0, err, "Couldn't sort processes");
}
err = ps_stream_create (stdout, &output);
if (err)
error (5, err, "Can't make output stream");
if (print_heading)
{
if (procs->num_procs > 0)
{
err = ps_fmt_write_titles (fmt, output);
if (err)
error (0, err, "Can't print titles");
ps_stream_newline (output);
}
else
error (1, 0, "No applicable processes");
}
if (output_width)
{
int deduce_term_size (int fd, char *type, int *width, int *height);
if (output_width < 0)
if (! deduce_term_size (1, getenv ("TERM"), &output_width, 0))
output_width = 80;
ps_fmt_set_output_width (fmt, output_width);
}
if (top)
{
int filter (struct proc_stat *ps)
{
return --top >= 0;
}
if (top < 0)
{
top += procs->num_procs;
proc_stat_list_filter1 (procs, filter, 0, 1);
}
else
proc_stat_list_filter1 (procs, filter, 0, 0);
}
err = proc_stat_list_fmt (procs, fmt, output);
if (err)
error (5, err, "Couldn't output process status");
}