#include <stdio.h>
#include <stdlib.h>
#include <assert-backtrace.h>
#include <string.h>
#include <ctype.h>
#include "ps.h"
#include "common.h"
static error_t
_fmt_create (char *src, int posix, struct ps_fmt_specs *fmt_specs,
struct ps_fmt **fmt, char **err_string)
{
struct ps_fmt *new_fmt;
int needs = 0;
int fields_alloced = 10;
int global_clr_flags = 0, global_inv_flags = 0;
struct ps_fmt_field *fields = NEWVEC (struct ps_fmt_field, fields_alloced);
struct ps_fmt_field *field = fields;
if (fields == NULL)
return ENOMEM;
new_fmt = NEW (struct ps_fmt);
if (new_fmt == NULL)
{
FREE (fields);
return ENOMEM;
}
new_fmt->src_len = strlen (src) + 1;
new_fmt->src = strdup (src);
if (new_fmt->src == NULL)
{
FREE (fields);
FREE (new_fmt);
return ENOMEM;
}
src = new_fmt->src;
while (*src != '\0')
{
char *start = src;
if (field - fields == fields_alloced)
{
int offs = field - fields;
fields_alloced += 10;
fields = GROWVEC (fields, struct ps_fmt_field, fields_alloced);
if (fields == NULL)
{
FREE (new_fmt->src);
FREE (new_fmt);
return ENOMEM;
}
field = fields + offs;
}
if (posix)
{
field->pfx = " ";
field->pfx_len = 1;
}
else
{
field->pfx = src;
while (*src != '\0' && *src != '%')
src++;
field->pfx_len = src - field->pfx;
}
field->spec = NULL;
field->title = NULL;
field->width = 0;
if (*src != '\0')
{
char *name;
int sign = 1;
int explicit_width = 0, explicit_precision = 0;
int quoted_name = 0;
int clr_flags = global_clr_flags, inv_flags = global_inv_flags;
if (! posix)
src++;
while (*src == '@' || *src == ':'
|| *src == '!' || *src == '?' || *src == '^')
{
if (*src == '@')
inv_flags ^= PS_FMT_FIELD_AT_MOD;
else if (*src == ':')
inv_flags ^= PS_FMT_FIELD_COLON_MOD;
else if (*src == '^')
inv_flags ^= PS_FMT_FIELD_UPCASE_TITLE;
else if (*src == '!')
{
clr_flags |= PS_FMT_FIELD_KEEP;
inv_flags |= PS_FMT_FIELD_KEEP;
}
else if (*src == '?')
{
clr_flags |= PS_FMT_FIELD_KEEP;
inv_flags &= ~PS_FMT_FIELD_KEEP;
}
src++;
}
field->width = 0;
if (*src == '-')
sign = -1, src++;
while (isdigit (*src))
{
field->width = field->width * 10 + (*src++ - '0');
explicit_width = TRUE;
}
field->precision = 0;
if (*src == '.')
while (isdigit (*++src))
{
field->precision = field->precision * 10 + (*src - '0');
explicit_precision = 1;
}
if (*src == '{')
{
src++;
quoted_name = 1;
}
else if (!isalnum (*src) && *src != '_')
{
if (src == start)
{
if (err_string)
asprintf (err_string, "%s: Unknown format spec", src);
FREE (new_fmt->src);
FREE (new_fmt);
FREE (fields);
return EINVAL;
}
global_clr_flags = clr_flags;
global_inv_flags = inv_flags;
continue;
}
name = src;
if (posix)
{
int stop = quoted_name ? '}' : ',';
while (*src != '\0' && *src != stop && *src != '=')
src++;
if (*src == '=')
{
*src++ = '\0';
field->title = src;
while (*src != '\0' && *src != stop)
src++;
}
if (*src)
*src++ = '\0';
}
else
{
while (quoted_name
? (*src != '\0' && *src != '}' && *src != ':')
: (isalnum (*src) || *src == '_'))
src++;
if (quoted_name && *src == ':')
{
*src++ = '\0';
field->title = src;
while (*src != '\0' && *src != '}')
src++;
}
bcopy (name, name - 1, src - name);
name--;
if (field->title)
field->title--;
src[-1] = '\0';
}
field->spec = ps_fmt_specs_find (fmt_specs, name);
if (! field->spec)
{
if (err_string)
asprintf (err_string, "%s: Unknown format spec", name);
FREE (new_fmt->src);
FREE (fields);
FREE (new_fmt);
return EINVAL;
}
if (! field->title)
{
if (field->spec->title)
field->title = field->spec->title;
else
field->title = field->spec->name;
}
needs |= ps_getter_needs (ps_fmt_spec_getter (field->spec));
if (! explicit_width)
field->width = field->spec->width;
if (! explicit_precision)
field->precision = field->spec->precision;
field->flags = (field->spec->flags & ~clr_flags) ^ inv_flags;
if (quoted_name && *src == '}')
src++;
if (posix)
{
if (*src == ',')
src++;
while (isspace (*src))
src++;
}
field->width *= sign;
{
int width = field->width;
int tlen = strlen (field->title);
if (width != 0 && tlen > ABS (width))
field->width = (width > 0 ? tlen : -tlen);
}
}
field++;
}
new_fmt->fields = fields;
new_fmt->num_fields = field - fields;
new_fmt->needs = needs;
new_fmt->inapp = posix ? "-" : 0;
new_fmt->error = "?";
*fmt = new_fmt;
return 0;
}
error_t
ps_fmt_create (char *src, int posix, struct ps_fmt_specs *fmt_specs,
struct ps_fmt **fmt)
{
return _fmt_create (src, posix, fmt_specs, fmt, 0);
}
void
ps_fmt_creation_error (char *src, int posix, struct ps_fmt_specs *fmt_specs,
char **error)
{
struct ps_fmt *fmt;
error_t err = _fmt_create (src, posix, fmt_specs, &fmt, error);
if (err != EINVAL)
asprintf (error, "%s", strerror (err));
if (! err)
ps_fmt_free (fmt);
}
void
ps_fmt_free (struct ps_fmt *fmt)
{
FREE (fmt->src);
FREE (fmt->fields);
FREE (fmt);
}
error_t
ps_fmt_clone (struct ps_fmt *fmt, struct ps_fmt **copy)
{
struct ps_fmt *new = NEW (struct ps_fmt);
struct ps_fmt_field *fields = NEWVEC (struct ps_fmt_field, fmt->num_fields);
char *src = malloc (fmt->src_len);
if (!new || !fields || !src)
{
free (new);
free (fields);
free (src);
return ENOMEM;
}
bcopy (fmt->src, src, fmt->src_len);
bcopy (fmt->fields, fields, fmt->num_fields * sizeof (struct ps_fmt_field));
new->fields = fields;
new->num_fields = fmt->num_fields;
new->src = src;
new->src_len = fmt->src_len;
new->inapp = fmt->inapp;
new->error = fmt->error;
*copy = new;
return 0;
}
error_t
ps_fmt_write_titles (struct ps_fmt *fmt, struct ps_stream *stream)
{
error_t err = 0;
struct ps_fmt_field *field = ps_fmt_fields (fmt);
int left = ps_fmt_num_fields (fmt);
while (left-- > 0 && !err)
{
const char *pfx = ps_fmt_field_prefix (field);
int pfx_len = ps_fmt_field_prefix_length (field);
if (pfx_len > 0)
err = ps_stream_write (stream, pfx, pfx_len);
if (ps_fmt_field_fmt_spec (field) != NULL && !err)
{
const char *title = ps_fmt_field_title (field) ?: "??";
int width = ps_fmt_field_width (field);
if (field->flags & PS_FMT_FIELD_UPCASE_TITLE)
{
int len = strlen (title), i;
char upcase_title[len + 1];
for (i = 0; i < len; i++)
upcase_title[i] = toupper (title[i]);
upcase_title[len] = '\0';
err = ps_stream_write_field (stream, upcase_title, width);
}
else
err = ps_stream_write_field (stream, title, width);
}
field++;
}
return err;
}
error_t
ps_fmt_write_proc_stat (struct ps_fmt *fmt, struct proc_stat *ps, struct ps_stream *stream)
{
error_t err = 0;
struct ps_fmt_field *field = ps_fmt_fields (fmt);
int nfields = ps_fmt_num_fields (fmt);
ps_flags_t have = ps->flags;
ps_flags_t inapp = ps->inapp;
while (nfields-- > 0 && !err)
{
const struct ps_fmt_spec *spec = ps_fmt_field_fmt_spec (field);
const char *pfx = ps_fmt_field_prefix (field);
int pfx_len = ps_fmt_field_prefix_length (field);
if (pfx_len > 0)
err = ps_stream_write (stream, pfx, pfx_len);
if (spec != NULL && !err)
{
ps_flags_t need = ps_getter_needs (ps_fmt_spec_getter (spec));
if ((need & have) == need)
err = (*spec->output_fn) (ps, field, stream);
else if (need & ~have & inapp)
err =
ps_stream_write_field (stream, fmt->inapp ?: "", field->width);
else
err =
ps_stream_write_field (stream, fmt->error ?: "", field->width);
}
field++;
}
return err;
}
void
ps_fmt_squash (struct ps_fmt *fmt, int (*fn)(struct ps_fmt_field *field))
{
int nfields = fmt->num_fields;
struct ps_fmt_field *fields = fmt->fields, *field = fields;
ps_flags_t need = 0;
while ((field - fields) < nfields)
if (field->spec != NULL && (*fn)(field))
{
const char *beg_pfx = field->pfx;
int beg_pfx_len = field->pfx_len;
nfields--;
if (nfields > 0)
bcopy (field + 1, field,
(nfields - (field - fields)) * sizeof *field);
if (field == fields)
{
if (nfields == 0)
{
nfields++;
field->pfx = beg_pfx;
field->pfx_len = beg_pfx_len;
field->spec = NULL;
}
else if (field->spec == NULL)
{
field->pfx -= beg_pfx_len;
field->pfx_len += beg_pfx_len;
bcopy (beg_pfx, (char *)field->pfx, beg_pfx_len);
}
else
{
field->pfx = beg_pfx;
field->pfx_len = beg_pfx_len;
}
}
}
else
{
if (field->spec)
need |= ps_getter_needs (field->spec->getter);
field++;
}
fmt->num_fields = nfields;
fmt->needs = need;
}
void
ps_fmt_squash_flags (struct ps_fmt *fmt, ps_flags_t flags)
{
int squashable_field (struct ps_fmt_field *field)
{
return field->spec->getter->needs & flags;
}
ps_fmt_squash (fmt, squashable_field);
}
void
ps_fmt_set_output_width (struct ps_fmt *fmt, int width)
{
struct ps_fmt_field *field = ps_fmt_fields (fmt);
int nfields = ps_fmt_num_fields (fmt);
while (--nfields > 0)
{
int fw = field->width;
width -= field->pfx_len + (fw < 0 ? -fw : fw);
field++;
}
if (nfields == 0 && field->width == 0 && width > 0)
field->width = width - field->pfx_len - 1;
}