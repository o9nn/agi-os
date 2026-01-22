#include "std.h"
#include "gdevbjcl.h"
private void
bjc_put_bytes(stream *s, const byte *data, uint count)
{
uint ignore;
sputs(s, data, count, &ignore);
}
private void
bjc_put_hi_lo(stream *s, int value)
{
spputc(s, value >> 8);
spputc(s, value & 0xff);
}
private void
bjc_put_lo_hi(stream *s, int value)
{
spputc(s, value & 0xff);
spputc(s, value >> 8);
}
private void
bjc_put_command(stream *s, int ch, int count)
{
spputc(s, 033 );
spputc(s, '(');
spputc(s, ch);
bjc_put_lo_hi(s, count);
}
void
bjc_put_LF(stream *s)
{
spputc(s, 0x0a);
}
void
bjc_put_FF(stream *s)
{
spputc(s, 0x0c);
}
void
bjc_put_CR(stream *s)
{
spputc(s, 0x0d);
}
void
bjc_put_initialize(stream *s)
{
bjc_put_bytes(s, (const byte *)"\033@", 2);
}
void
bjc_put_set_initial(stream *s)
{
bjc_put_bytes(s, (const byte *)"\033[K\002\000\000\017", 7);
}
void
bjc_put_set_compression(stream *s, bjc_raster_compression_t compression)
{
bjc_put_command(s, 'b', 1);
spputc(s, compression);
}
void
bjc_put_print_method_short(stream *s, bjc_print_color_short_t color)
{
bjc_put_command(s, 'c', 1);
spputc(s, color);
}
void
bjc_put_print_method(stream *s, bjc_print_color_t color,
bjc_print_media_t media, bjc_print_quality_t quality,
bjc_black_density_t density)
{
bjc_put_command(s, 'c', 2 + (density != 0));
spputc(s, 0x10 | color);
spputc(s, (media << 4) | quality);
if (density)
spputc(s, density << 4);
}
void
bjc_put_raster_resolution(stream *s, int x_resolution, int y_resolution)
{
if (x_resolution == y_resolution) {
bjc_put_command(s, 'd', 2);
} else {
bjc_put_command(s, 'd', 4);
bjc_put_hi_lo(s, y_resolution);
}
bjc_put_hi_lo(s, x_resolution);
}
void
bjc_put_raster_skip(stream *s, int skip)
{
bjc_put_command(s, 'e', 2);
bjc_put_hi_lo(s, skip);
}
void
bjc_put_page_margins(stream *s, int length, int lm, int rm, int top)
{
byte parms[4];
int count;
parms[0] = length, parms[1] = lm, parms[2] = rm, parms[3] = top;
count = 4;
bjc_put_command(s, 'g', count);
bjc_put_bytes(s, parms, count);
}
void
bjc_put_media_supply(stream *s, bjc_media_supply_t supply,
bjc_media_type_t type)
{
bjc_put_command(s, 'l', 2);
spputc(s, 0x10 | supply);
spputc(s, type << 4);
}
void
bjc_put_identify_cartridge(stream *s,
bjc_identify_cartridge_command_t command)
{
bjc_put_command(s, 'm', 1);
spputc(s, command);
}
void
bjc_put_cmyk_image(stream *s, bjc_cmyk_image_component_t component,
const byte *data, int count)
{
bjc_put_command(s, 'A', count + 1);
spputc(s, component);
bjc_put_bytes(s, data, count);
}
void
bjc_put_move_lines(stream *s, int lines)
{
bjc_put_command(s, 'n', 2);
bjc_put_hi_lo(s, lines);
}
void
bjc_put_move_lines_unit(stream *s, int unit)
{
bjc_put_command(s, 'o', 2);
bjc_put_hi_lo(s, unit);
}
void
bjc_put_extended_margins(stream *s, int length, int lm, int rm, int top)
{
bjc_put_command(s, 'p', 8);
bjc_put_hi_lo(s, length);
bjc_put_hi_lo(s, lm);
bjc_put_hi_lo(s, rm);
bjc_put_hi_lo(s, top);
}
void
bjc_put_image_format(stream *s, int depth, bjc_image_format_t format,
bjc_ink_system_t ink)
{
bjc_put_command(s, 't', 3);
spputc(s, depth);
spputc(s, format);
spputc(s, ink);
}
void
bjc_put_page_id(stream *s, int id)
{
bjc_put_command(s, 'q', 1);
spputc(s, id);
}
void
bjc_put_continue_image(stream *s, const byte *data, int count)
{
bjc_put_command(s, 'F', count);
bjc_put_bytes(s, data, count);
}
void
bjc_put_indexed_image(stream *s, int dot_rows, int dot_cols, int layers)
{
bjc_put_command(s, 'f', 5);
spputc(s, 'R');
spputc(s, dot_rows);
spputc(s, dot_cols);
spputc(s, layers);
}