#include "system.h"
#include <stdio.h>
#ifdef __FreeBSD__
#include <gnuregex.h>
#else
#include "regex.h"
#endif
#define TAB_WIDTH 8
#ifndef GDIFF_MAIN
#define EXTERN extern
#else
#define EXTERN
#endif
enum output_style {
OUTPUT_NORMAL,
OUTPUT_CONTEXT,
OUTPUT_UNIFIED,
OUTPUT_ED,
OUTPUT_FORWARD_ED,
OUTPUT_RCS,
OUTPUT_IFDEF,
OUTPUT_SDIFF
};
#define ROBUST_OUTPUT_STYLE(S) ((S) != OUTPUT_ED && (S) != OUTPUT_FORWARD_ED)
EXTERN enum output_style output_style;
EXTERN int no_diff_means_no_output;
EXTERN int context;
EXTERN int always_text_flag;
EXTERN int horizon_lines;
EXTERN int ignore_space_change_flag;
EXTERN int ignore_all_space_flag;
EXTERN int ignore_blank_lines_flag;
EXTERN int ignore_some_line_changes;
EXTERN int ignore_some_changes;
EXTERN int ignore_case_flag;
EXTERN char *file_label[2];
struct regexp_list
{
struct re_pattern_buffer buf;
struct regexp_list *next;
};
EXTERN struct regexp_list *function_regexp_list;
EXTERN struct regexp_list *ignore_regexp_list;
EXTERN int no_details_flag;
EXTERN int print_file_same_flag;
EXTERN int tab_align_flag;
EXTERN int tab_expand_flag;
EXTERN char *dir_start_file;
EXTERN int entire_new_file_flag;
EXTERN int unidirectional_new_file_flag;
EXTERN int paginate_flag;
enum line_class {
OLD,
NEW,
UNCHANGED,
CHANGED
};
EXTERN char *group_format[CHANGED + 1];
EXTERN char *line_format[UNCHANGED + 1];
EXTERN int sdiff_help_sdiff;
EXTERN int sdiff_left_only;
EXTERN int sdiff_skip_common_lines;
EXTERN unsigned sdiff_half_width;
EXTERN unsigned sdiff_column2_offset;
EXTERN char * switch_string;
EXTERN int heuristic;
EXTERN char *program_name;
struct change
{
struct change *link;
int inserted;
int deleted;
int line0;
int line1;
char ignore;
};
struct file_data {
int desc;
char const *name;
struct stat stat;
int dir_p;
char * buffer;
size_t bufsize;
size_t buffered_chars;
char const **linbuf;
int linbuf_base, buffered_lines, valid_lines, alloc_lines;
char const *prefix_end;
int prefix_lines;
char const *suffix_begin;
int *equivs;
int *undiscarded;
int *realindexes;
int nondiscarded_lines;
char *changed_flag;
int missing_newline;
int equiv_max;
};
EXTERN struct file_data files[2];
EXTERN FILE *outfile;
int diff_2_files PARAMS((struct file_data[], int));
void print_context_header PARAMS((struct file_data[], int));
void print_context_script PARAMS((struct change *, int));
int excluded_filename PARAMS((char const *));
int diff_dirs PARAMS((struct file_data const[], int (*) PARAMS((char const *, char const *, char const *, char const *, int)), int));
void print_ed_script PARAMS((struct change *));
void pr_forward_ed_script PARAMS((struct change *));
void print_ifdef_script PARAMS((struct change *));
int read_files PARAMS((struct file_data[], int));
int sip PARAMS((struct file_data *, int));
void slurp PARAMS((struct file_data *));
void print_normal_script PARAMS((struct change *));
void print_rcs_script PARAMS((struct change *));
void print_sdiff_script PARAMS((struct change *));
VOID *xmalloc PARAMS((size_t));
VOID *xrealloc PARAMS((VOID *, size_t));
char *concat PARAMS((char const *, char const *, char const *));
char *dir_file_pathname PARAMS((char const *, char const *));
int change_letter PARAMS((int, int));
int line_cmp PARAMS((char const *, char const *));
int translate_line_number PARAMS((struct file_data const *, int));
struct change *find_change PARAMS((struct change *));
struct change *find_reverse_change PARAMS((struct change *));
void analyze_hunk PARAMS((struct change *, int *, int *, int *, int *, int *, int *));
void begin_output PARAMS((void));
void debug_script PARAMS((struct change *));
void error PARAMS((char const *, char const *, char const *));
void fatal PARAMS((char const *));
void finish_output PARAMS((void));
void message PARAMS((char const *, char const *, char const *));
void message5 PARAMS((char const *, char const *, char const *, char const *, char const *));
void output_1_line PARAMS((char const *, char const *, char const *, char const *));
void perror_with_name PARAMS((char const *));
void pfatal_with_name PARAMS((char const *));
void print_1_line PARAMS((char const *, char const * const *));
void print_message_queue PARAMS((void));
void print_number_range PARAMS((int, struct file_data *, int, int));
void print_script PARAMS((struct change *, struct change * (*) PARAMS((struct change *)), void (*) PARAMS((struct change *))));
void setup_output PARAMS((char const *, char const *, int));
void translate_range PARAMS((struct file_data const *, int, int, int *, int *));
extern char const version_string[];