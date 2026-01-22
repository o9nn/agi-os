#ifndef gdevpsu_INCLUDED
# define gdevpsu_INCLUDED
typedef struct gx_device_pswrite_common_s {
float LanguageLevel;
bool ProduceEPS;
int ProcSet_version;
long bbox_position;
} gx_device_pswrite_common_t;
#define PSWRITE_COMMON_PROCSET_VERSION 1000
#define PSWRITE_COMMON_VALUES(ll, eps, psv)\
{ll, eps, PSWRITE_COMMON_PROCSET_VERSION + (psv)}
int psw_print_lines(FILE *f, const char *const lines[]);
int psw_begin_file_header(FILE *f, const gx_device *dev,
const gs_rect *pbbox,
gx_device_pswrite_common_t *pdpc, bool ascii);
int psw_end_file_header(FILE *f);
int psw_end_file(FILE *f, const gx_device *dev,
const gx_device_pswrite_common_t *pdpc,
const gs_rect *pbbox, int page_count);
int psw_write_page_header(stream *s, const gx_device *dev,
const gx_device_pswrite_common_t *pdpc,
bool do_scale, long page_ord, int dictsize);
int psw_write_page_trailer(FILE *f, int num_copies, int flush);
#endif