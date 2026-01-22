#if 0
#include "png.h"
#ifndef png_jmpbuf
#  define png_jmpbuf(png_ptr) ((png_ptr)->jmpbuf)
#endif
#define PNG_BYTES_TO_CHECK 4
int check_if_png(char *file_name, FILE **fp)
{
char buf[PNG_BYTES_TO_CHECK];
if ((*fp = fopen(file_name, "rb")) == NULL)
return 0;
if (fread(buf, 1, PNG_BYTES_TO_CHECK, *fp) != PNG_BYTES_TO_CHECK)
return 0;
return(!png_sig_cmp(buf, (png_size_t)0, PNG_BYTES_TO_CHECK));
}
#ifdef open_file
void read_png(char *file_name)
{
png_structp png_ptr;
png_infop info_ptr;
unsigned int sig_read = 0;
png_uint_32 width, height;
int bit_depth, color_type, interlace_type;
FILE *fp;
if ((fp = fopen(file_name, "rb")) == NULL)
return (ERROR);
#else no_open_file
void read_png(FILE *fp, unsigned int sig_read)
{
png_structp png_ptr;
png_infop info_ptr;
png_uint_32 width, height;
int bit_depth, color_type, interlace_type;
#endif no_open_file
png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING,
png_voidp user_error_ptr, user_error_fn, user_warning_fn);
if (png_ptr == NULL)
{
fclose(fp);
return (ERROR);
}
info_ptr = png_create_info_struct(png_ptr);
if (info_ptr == NULL)
{
fclose(fp);
png_destroy_read_struct(&png_ptr, png_infopp_NULL, png_infopp_NULL);
return (ERROR);
}
if (setjmp(png_jmpbuf(png_ptr)))
{
png_destroy_read_struct(&png_ptr, &info_ptr, png_infopp_NULL);
fclose(fp);
return (ERROR);
}
#ifdef streams
png_init_io(png_ptr, fp);
#else no_streams
png_set_read_fn(png_ptr, (void *)user_io_ptr, user_read_fn);
#endif no_streams
png_set_sig_bytes(png_ptr, sig_read);
#ifdef hilevel
png_read_png(png_ptr, info_ptr, png_transforms, png_voidp_NULL);
#else
png_read_info(png_ptr, info_ptr);
png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type,
&interlace_type, int_p_NULL, int_p_NULL);
png_set_strip_16(png_ptr);
png_set_strip_alpha(png_ptr);
png_set_packing(png_ptr);
png_set_packswap(png_ptr);
if (color_type == PNG_COLOR_TYPE_PALETTE)
png_set_palette_rgb(png_ptr);
if (color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8)
png_set_gray_1_2_4_to_8(png_ptr);
if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS))
png_set_tRNS_to_alpha(png_ptr);
png_color_16 my_background, *image_background;
if (png_get_bKGD(png_ptr, info_ptr, &image_background))
png_set_background(png_ptr, image_background,
PNG_BACKGROUND_GAMMA_FILE, 1, 1.0);
else
png_set_background(png_ptr, &my_background,
PNG_BACKGROUND_GAMMA_SCREEN, 0, 1.0);
if ()
{
screen_gamma = user-defined screen_gamma;
}
else if ((gamma_str = getenv("SCREEN_GAMMA")) != NULL)
{
screen_gamma = atof(gamma_str);
}
else
{
screen_gamma = 2.2;
screen_gamma = 1.7 or 1.0;
}
int intent;
if (png_get_sRGB(png_ptr, info_ptr, &intent))
png_set_gamma(png_ptr, screen_gamma, 0.45455);
else
{
double image_gamma;
if (png_get_gAMA(png_ptr, info_ptr, &image_gamma))
png_set_gamma(png_ptr, screen_gamma, image_gamma);
else
png_set_gamma(png_ptr, screen_gamma, 0.45455);
}
if (color_type & PNG_COLOR_MASK_COLOR)
{
int num_palette;
png_colorp palette;
if ()
{
png_color std_color_cube[MAX_SCREEN_COLORS];
png_set_dither(png_ptr, std_color_cube, MAX_SCREEN_COLORS,
MAX_SCREEN_COLORS, png_uint_16p_NULL, 0);
}
else if (png_get_PLTE(png_ptr, info_ptr, &palette, &num_palette))
{
png_uint_16p histogram = NULL;
png_get_hIST(png_ptr, info_ptr, &histogram);
png_set_dither(png_ptr, palette, num_palette,
max_screen_colors, histogram, 0);
}
}
png_set_invert_mono(png_ptr);
if (png_get_valid(png_ptr, info_ptr, PNG_INFO_sBIT))
{
png_color_8p sig_bit;
png_get_sBIT(png_ptr, info_ptr, &sig_bit);
png_set_shift(png_ptr, sig_bit);
}
if (color_type & PNG_COLOR_MASK_COLOR)
png_set_bgr(png_ptr);
png_set_swap_alpha(png_ptr);
png_set_swap(png_ptr);
png_set_filler(png_ptr, 0xff, PNG_FILLER_AFTER);
number_passes = png_set_interlace_handling(png_ptr);
png_read_update_info(png_ptr, info_ptr);
png_bytep row_pointers[height];
for (row = 0; row < height; row++)
{
row_pointers[row] = png_malloc(png_ptr, png_get_rowbytes(png_ptr,
info_ptr));
}
#ifdef entire
png_read_image(png_ptr, row_pointers);
#else no_entire
for (pass = 0; pass < number_passes; pass++)
{
#ifdef single
for (y = 0; y < height; y++)
{
png_read_rows(png_ptr, &row_pointers[y], png_bytepp_NULL, 1);
}
#else no_single
for (y = 0; y < height; y += number_of_rows)
{
#ifdef sparkle
png_read_rows(png_ptr, &row_pointers[y], png_bytepp_NULL,
number_of_rows);
#else no_sparkle
png_read_rows(png_ptr, png_bytepp_NULL, &row_pointers[y],
number_of_rows);
#endif no_sparkle
}
#endif no_single
}
#endif no_entire
png_read_end(png_ptr, info_ptr);
#endif hilevel
png_destroy_read_struct(&png_ptr, &info_ptr, png_infopp_NULL);
fclose(fp);
return (OK);
}
int
initialize_png_reader(png_structp *png_ptr, png_infop *info_ptr)
{
*png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING,
png_voidp user_error_ptr, user_error_fn, user_warning_fn);
if (*png_ptr == NULL)
{
*info_ptr = NULL;
return (ERROR);
}
*info_ptr = png_create_info_struct(png_ptr);
if (*info_ptr == NULL)
{
png_destroy_read_struct(png_ptr, info_ptr, png_infopp_NULL);
return (ERROR);
}
if (setjmp(png_jmpbuf((*png_ptr))))
{
png_destroy_read_struct(png_ptr, info_ptr, png_infopp_NULL);
return (ERROR);
}
png_set_progressive_read_fn(*png_ptr, (void *)stream_data,
info_callback, row_callback, end_callback);
return (OK);
}
int
process_data(png_structp *png_ptr, png_infop *info_ptr,
png_bytep buffer, png_uint_32 length)
{
if (setjmp(png_jmpbuf((*png_ptr))))
{
png_destroy_read_struct(png_ptr, info_ptr, png_infopp_NULL);
return (ERROR);
}
png_process_data(*png_ptr, *info_ptr, buffer, length);
return (OK);
}
info_callback(png_structp png_ptr, png_infop info)
{
}
row_callback(png_structp png_ptr, png_bytep new_row,
png_uint_32 row_num, int pass)
{
if((row_num >= 0) && (row_num < height))
{
png_bytep old_row = ((png_bytep *)our_data)[row_num];
if((old_row != NULL) && (new_row != NULL))
png_progressive_combine_row(png_ptr, old_row, new_row);
}
png_progressive_combine_row(png_ptr, old_row, new_row);
}
end_callback(png_structp png_ptr, png_infop info)
{
}
void write_png(char *file_name )
{
FILE *fp;
png_structp png_ptr;
png_infop info_ptr;
png_colorp palette;
fp = fopen(file_name, "wb");
if (fp == NULL)
return (ERROR);
png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
png_voidp user_error_ptr, user_error_fn, user_warning_fn);
if (png_ptr == NULL)
{
fclose(fp);
return (ERROR);
}
info_ptr = png_create_info_struct(png_ptr);
if (info_ptr == NULL)
{
fclose(fp);
png_destroy_write_struct(&png_ptr,  png_infopp_NULL);
return (ERROR);
}
if (setjmp(png_jmpbuf(png_ptr)))
{
fclose(fp);
png_destroy_write_struct(&png_ptr, &info_ptr);
return (ERROR);
}
#ifdef streams
png_init_io(png_ptr, fp);
#else no_streams
png_set_write_fn(png_ptr, (void *)user_io_ptr, user_write_fn,
user_IO_flush_function);
#endif no_streams
#ifdef hilevel
png_write_png(png_ptr, info_ptr, png_transforms, png_voidp_NULL);
#else
png_set_IHDR(png_ptr, info_ptr, width, height, bit_depth, PNG_COLOR_TYPE_???,
PNG_INTERLACE_????, PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);
palette = (png_colorp)png_malloc(png_ptr, PNG_MAX_PALETTE_LENGTH
* png_sizeof (png_color));
png_set_PLTE(png_ptr, info_ptr, palette, PNG_MAX_PALETTE_LENGTH);
sig_bit.gray = true_bit_depth;
sig_bit.red = true_red_bit_depth;
sig_bit.green = true_green_bit_depth;
sig_bit.blue = true_blue_bit_depth;
sig_bit.alpha = true_alpha_bit_depth;
png_set_sBIT(png_ptr, info_ptr, sig_bit);
png_set_gAMA(png_ptr, info_ptr, gamma);
text_ptr[0].key = "Title";
text_ptr[0].text = "Mona Lisa";
text_ptr[0].compression = PNG_TEXT_COMPRESSION_NONE;
text_ptr[1].key = "Author";
text_ptr[1].text = "Leonardo DaVinci";
text_ptr[1].compression = PNG_TEXT_COMPRESSION_NONE;
text_ptr[2].key = "Description";
text_ptr[2].text = "<long text>";
text_ptr[2].compression = PNG_TEXT_COMPRESSION_zTXt;
#ifdef PNG_iTXt_SUPPORTED
text_ptr[0].lang = NULL;
text_ptr[1].lang = NULL;
text_ptr[2].lang = NULL;
#endif
png_set_text(png_ptr, info_ptr, text_ptr, 3);
png_write_info(png_ptr, info_ptr);
png_set_invert_mono(png_ptr);
png_set_shift(png_ptr, &sig_bit);
png_set_packing(png_ptr);
png_set_swap_alpha(png_ptr);
png_set_filler(png_ptr, 0, PNG_FILLER_BEFORE);
png_set_bgr(png_ptr);
png_set_swap(png_ptr);
png_set_packswap(png_ptr);
if (interlacing)
number_passes = png_set_interlace_handling(png_ptr);
else
number_passes = 1;
png_uint_32 k, height, width;
png_byte image[height][width*bytes_per_pixel];
png_bytep row_pointers[height];
if (height > PNG_UINT_32_MAX/png_sizeof(png_bytep))
png_error (png_ptr, "Image is too tall to process in memory");
for (k = 0; k < height; k++)
row_pointers[k] = image + k*width*bytes_per_pixel;
#ifdef entire
png_write_image(png_ptr, row_pointers);
#else no_entire
for (pass = 0; pass < number_passes; pass++)
{
png_write_rows(png_ptr, &row_pointers[first_row], number_of_rows);
for (y = 0; y < height; y++)
{
png_write_rows(png_ptr, &row_pointers[y], 1);
}
}
#endif no_entire
png_write_end(png_ptr, info_ptr);
#endif hilevel
png_free(png_ptr, palette);
palette=NULL;
png_free(png_ptr, trans);
trans=NULL;
png_destroy_write_struct(&png_ptr, &info_ptr);
fclose(fp);
return (OK);
}
#endif