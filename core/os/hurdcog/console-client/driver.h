#ifndef _CONSOLE_DRIVER_H_
#define _CONSOLE_DRIVER_H_ 1
#include <errno.h>
#include <stddef.h>
#include <pthread.h>
#include "display.h"
#include "input.h"
#include "bell.h"
extern char *driver_path;
error_t driver_init (void);
error_t driver_fini (void);
struct driver_ops;
typedef struct driver_ops *driver_ops_t;
error_t driver_add (const char *const name, const char *const driver,
int argc, char *argv[], int *next, int start);
error_t driver_start (char **name);
error_t driver_remove (const char *const name);
#define driver_iterate							\
for (driver_t driver = (pthread_mutex_lock (&driver_list_lock),	\
&driver_list[0]);				\
driver < &driver_list[driver_list_len]				\
|| (pthread_mutex_unlock (&driver_list_lock), 0);		\
driver++)
struct driver_ops
{
error_t (*init) (void **handle, int no_exit,
int argc, char *argv[], int *next);
error_t (*start) (void *handle);
error_t (*fini) (void *handle, int force);
void (*save_status) (void *handle);
void (*restore_status) (void *handle);
};
struct driver
{
char *name;
char *driver;
char *filename;
driver_ops_t ops;
void *handle;
void *module;
};
typedef struct driver *driver_t;
extern pthread_mutex_t driver_list_lock;
extern driver_t driver_list;
extern size_t driver_list_len;
#define display_iterate							\
for (display_t display = (pthread_mutex_lock (&display_list_lock),	\
&display_list[0]);				\
display < &display_list[display_list_len]			\
|| (pthread_mutex_unlock (&display_list_lock), 0);		\
display++)
struct display
{
display_ops_t ops;
void *handle;
};
typedef struct display *display_t;
extern pthread_mutex_t display_list_lock;
extern display_t display_list;
extern size_t display_list_len;
#define input_iterate								\
for (input_t input = (pthread_mutex_lock (&input_list_lock), &input_list[0]);	\
input < &input_list[input_list_len]					\
|| (pthread_mutex_unlock (&input_list_lock), 0);			\
input++)
struct input
{
input_ops_t ops;
void *handle;
};
typedef struct input *input_t;
extern pthread_mutex_t input_list_lock;
extern input_t input_list;
extern size_t input_list_len;
#define bell_iterate								\
for (bell_t bell = (pthread_mutex_lock (&bell_list_lock), &bell_list[0]);	\
bell < &bell_list[bell_list_len]						\
|| (pthread_mutex_unlock (&bell_list_lock), 0);			\
bell++)
struct bell
{
bell_ops_t ops;
void *handle;
};
typedef struct bell *bell_t;
extern pthread_mutex_t bell_list_lock;
extern bell_t bell_list;
extern size_t bell_list_len;
#endif