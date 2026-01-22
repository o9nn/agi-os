#ifndef _boot_script_h
#define _boot_script_h
#define BOOT_SCRIPT_NOMEM		1
#define BOOT_SCRIPT_SYNTAX_ERROR	2
#define BOOT_SCRIPT_INVALID_ASG		3
#define BOOT_SCRIPT_MACH_ERROR		4
#define BOOT_SCRIPT_UNDEF_SYM		5
#define BOOT_SCRIPT_EXEC_ERROR		6
#define BOOT_SCRIPT_INVALID_SYM		7
#define BOOT_SCRIPT_BAD_TYPE		8
#define VAL_NONE	0
#define VAL_STR		1
#define VAL_PORT	2
#define VAL_TASK	3
struct cmd
{
void *hook;
char *path;
task_t task;
struct arg **args;
int args_alloc;
int args_index;
struct sym **exec_funcs;
int exec_funcs_alloc;
int exec_funcs_index;
};
void *boot_script_malloc (unsigned int);
void boot_script_free (void *, unsigned int);
int boot_script_exec_cmd (void *hook,
task_t task, char *path, int argc,
char **argv, char *strings, int stringlen);
int boot_script_task_create (struct cmd *);
int boot_script_task_resume (struct cmd *);
int boot_script_prompt_task_resume (struct cmd *);
int boot_script_insert_right (struct cmd *, mach_port_t, mach_port_name_t *namep);
int boot_script_insert_task_port (struct cmd *, task_t, mach_port_name_t *namep);
void boot_script_free_task (task_t task, int aborting);
int boot_script_parse_line (void *hook, char *cmdline);
int boot_script_exec (void);
int boot_script_set_variable (const char *name, int type, long val);
int boot_script_define_function (const char *name, int ret_type,
int (*func) (const struct cmd *cmd, int *val));
char *boot_script_error_string (int err);
#endif