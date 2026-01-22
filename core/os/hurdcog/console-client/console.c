#include <argp.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <wchar.h>
#include <error.h>
#include <assert-backtrace.h>
#include <pthread.h>
#if HAVE_DAEMON
#include <libdaemon/daemon.h>
#endif
#include <hurd/console.h>
#include <hurd/cons.h>
#include <version.h>
#include "driver.h"
#include "timer.h"
#include "trans.h"
const char *cons_client_name = "console";
const char *cons_client_version = HURD_VERSION;
#define DEFAULT_CONSOLE_NODE	"/dev/cons"
static pthread_mutex_t global_lock;
static vcons_t active_vcons = NULL;
static int saved_id = 0;
static cons_t saved_cons;
static char *console_node;
static int daemonize;
error_t
console_current_id (int *cur)
{
vcons_t vcons;
pthread_mutex_lock (&global_lock);
vcons = active_vcons;
if (!vcons)
{
pthread_mutex_unlock (&global_lock);
return ENODEV;
}
*cur = vcons->id;
pthread_mutex_unlock (&global_lock);
return 0;
}
error_t
console_switch (int id, int delta)
{
error_t err = 0;
vcons_t vcons;
vcons_t new_vcons;
pthread_mutex_lock (&global_lock);
vcons = active_vcons;
if (!vcons)
{
pthread_mutex_unlock (&global_lock);
return EINVAL;
}
ports_port_ref (vcons);
pthread_mutex_unlock (&global_lock);
err = cons_switch (vcons, id, delta, &new_vcons);
if (!err)
{
pthread_mutex_lock (&global_lock);
if (active_vcons != new_vcons)
{
cons_vcons_close (active_vcons);
active_vcons = new_vcons;
}
pthread_mutex_unlock (&new_vcons->lock);
ports_port_deref (vcons);
pthread_mutex_unlock (&global_lock);
}
return err;
}
error_t
console_input (char *buf, size_t size)
{
error_t err = 0;
vcons_t vcons;
pthread_mutex_lock (&global_lock);
vcons = active_vcons;
if (!vcons)
{
pthread_mutex_unlock (&global_lock);
return EINVAL;
}
ports_port_ref (vcons);
pthread_mutex_unlock (&global_lock);
if (vcons)
{
err = cons_vcons_input (vcons, buf, size);
ports_port_deref (vcons);
}
return err;
}
error_t
console_move_mouse (mouse_event_t ev)
{
error_t err;
vcons_t vcons;
pthread_mutex_lock (&global_lock);
vcons = active_vcons;
if (!vcons)
{
pthread_mutex_unlock (&global_lock);
return EINVAL;
}
ports_port_ref (vcons);
pthread_mutex_unlock (&global_lock);
if (vcons)
{
err = cons_vcons_move_mouse (vcons, ev);
ports_port_deref (vcons);
return err;
}
return 0;
}
int
console_scrollback (cons_scroll_t type, float value)
{
int nr = 0;
vcons_t vcons;
pthread_mutex_lock (&global_lock);
vcons = active_vcons;
if (!vcons)
{
pthread_mutex_unlock (&global_lock);
return EINVAL;
}
ports_port_ref (vcons);
pthread_mutex_unlock (&global_lock);
if (vcons)
{
nr = cons_vcons_scrollback (vcons, type, value);
ports_port_deref (vcons);
}
return nr;
}
void
console_switch_away (void)
{
pthread_mutex_lock (&global_lock);
driver_iterate
if (driver->ops->save_status)
driver->ops->save_status (driver->handle);
if (active_vcons)
{
saved_id = active_vcons->id;
saved_cons = active_vcons->cons;
cons_vcons_close (active_vcons);
active_vcons = NULL;
}
else
{
saved_cons = NULL;
}
pthread_mutex_unlock (&global_lock);
}
void
console_switch_back (void)
{
vcons_list_t conslist;
pthread_mutex_lock (&global_lock);
driver_iterate
if (driver->ops->restore_status)
driver->ops->restore_status (driver->handle);
if (saved_cons)
{
error_t err;
err = cons_lookup (saved_cons, saved_id, 1, &conslist);
if (err)
{
pthread_mutex_unlock (&global_lock);
return;
}
err = cons_vcons_open (saved_cons, conslist, &active_vcons);
if (err)
{
pthread_mutex_unlock (&global_lock);
return;
}
conslist->vcons = active_vcons;
saved_cons = NULL;
pthread_mutex_unlock (&active_vcons->lock);
}
pthread_mutex_unlock (&global_lock);
}
void
console_exit (void)
{
driver_fini ();
#if HAVE_DAEMON
if (daemonize)
daemon_pid_file_remove ();
#endif
exit (0);
}
void console_error (const wchar_t *const err_msg)
{
pthread_mutex_lock (&global_lock);
bell_iterate
if (bell->ops->beep)
bell->ops->beep (bell->handle);
pthread_mutex_unlock (&global_lock);
}
#if QUAERENDO_INVENIETIS
void
console_deprecated (int key)
{
pthread_mutex_lock (&global_lock);
input_iterate
if (input->ops->deprecated)
(*input->ops->deprecated) (input->handle, key);
display_iterate
if (display->ops->deprecated)
(*display->ops->deprecated) (display->handle, key);
bell_iterate
if (bell->ops->deprecated)
(*bell->ops->deprecated) (bell->handle, key);
pthread_mutex_unlock (&global_lock);
}
#endif
void
cons_vcons_add (cons_t cons, vcons_list_t vcons_entry)
{
error_t err = 0;
pthread_mutex_lock (&global_lock);
if (!active_vcons)
{
vcons_t vcons;
err = cons_vcons_open (cons, vcons_entry, &vcons);
if (!err)
{
vcons_entry->vcons = vcons;
active_vcons = vcons;
pthread_mutex_unlock (&vcons->lock);
}
}
pthread_mutex_unlock (&global_lock);
}
void
cons_vcons_update (vcons_t vcons)
{
pthread_mutex_lock (&global_lock);
if (vcons == active_vcons)
display_iterate
if (display->ops->update)
display->ops->update (display->handle);
pthread_mutex_unlock (&global_lock);
}
void
cons_vcons_set_cursor_pos (vcons_t vcons, uint32_t col, uint32_t row)
{
pthread_mutex_lock (&global_lock);
if (vcons == active_vcons)
display_iterate
if (display->ops->set_cursor_pos)
display->ops->set_cursor_pos (display->handle, col, row);
pthread_mutex_unlock (&global_lock);
}
void
cons_vcons_set_cursor_status (vcons_t vcons, uint32_t status)
{
pthread_mutex_lock (&global_lock);
if (vcons == active_vcons)
display_iterate
if (display->ops->set_cursor_status)
display->ops->set_cursor_status (display->handle, status);
pthread_mutex_unlock (&global_lock);
}
void
cons_vcons_scroll (vcons_t vcons, int delta)
{
pthread_mutex_lock (&global_lock);
if (vcons == active_vcons)
display_iterate
if (display->ops->scroll)
display->ops->scroll (display->handle, delta);
pthread_mutex_unlock (&global_lock);
}
void cons_vcons_clear (vcons_t vcons, size_t length,
uint32_t col, uint32_t row)
{
pthread_mutex_lock (&global_lock);
if (vcons == active_vcons)
display_iterate
if (display->ops->clear)
display->ops->clear (display->handle, length, col, row);
pthread_mutex_unlock (&global_lock);
}
void
cons_vcons_write (vcons_t vcons, conchar_t *str, size_t length,
uint32_t col, uint32_t row)
{
pthread_mutex_lock (&global_lock);
if (vcons == active_vcons)
display_iterate
if (display->ops->write)
display->ops->write (display->handle, str, length, col, row);
pthread_mutex_unlock (&global_lock);
}
void
cons_vcons_beep (vcons_t vcons)
{
pthread_mutex_lock (&global_lock);
if (vcons == active_vcons)
bell_iterate
if (bell->ops->beep)
bell->ops->beep (bell->handle);
pthread_mutex_unlock (&global_lock);
}
void
cons_vcons_flash (vcons_t vcons)
{
pthread_mutex_lock (&global_lock);
if (vcons == active_vcons)
display_iterate
if (display->ops->flash)
display->ops->flash (display->handle);
pthread_mutex_unlock (&global_lock);
}
void
cons_vcons_set_scroll_lock (vcons_t vcons, int onoff)
{
pthread_mutex_lock (&global_lock);
if (vcons == active_vcons)
input_iterate
if (input->ops->set_scroll_lock_status)
input->ops->set_scroll_lock_status (input->handle, onoff);
pthread_mutex_unlock (&global_lock);
}
error_t
cons_vcons_set_dimension (vcons_t vcons, uint32_t col, uint32_t row)
{
pthread_mutex_lock (&global_lock);
if (vcons == active_vcons)
display_iterate
if (display->ops->set_dimension)
display->ops->set_dimension (display->handle, col, row);
pthread_mutex_unlock (&global_lock);
return 0;
}
error_t
cons_vcons_set_mousecursor_pos (vcons_t vcons, float x, float y)
{
pthread_mutex_lock (&global_lock);
if (vcons == active_vcons)
display_iterate
if (display->ops->set_mousecursor_pos)
display->ops->set_mousecursor_pos (display->handle, x, y);
pthread_mutex_unlock (&global_lock);
return 0;
}
error_t
cons_vcons_set_mousecursor_status (vcons_t vcons, int status)
{
pthread_mutex_lock (&global_lock);
if (vcons == active_vcons)
display_iterate
if (display->ops->set_mousecursor_status)
display->ops->set_mousecursor_status (display->handle, status);
pthread_mutex_unlock (&global_lock);
return 0;
}
#define DAEMONIZE_KEY 0x80
static const struct argp_option
options[] =
{
{"driver-path", 'D', "PATH", 0, "Specify search path for driver modules" },
{"driver", 'd', "NAME", 0, "Add driver NAME to the console" },
{"console-node", 'c', "FILE", OPTION_ARG_OPTIONAL,
"Set a translator on the node FILE (default: " DEFAULT_CONSOLE_NODE ")" },
#if HAVE_DAEMON
{"daemonize", DAEMONIZE_KEY, NULL, 0, "daemonize the console client"},
#endif
{0}
};
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
static int devcount = 0;
error_t err;
switch (key)
{
case 'D':
{
char *s;
char *d;
free (driver_path);
driver_path = malloc (strlen (arg) + 2);
if (!driver_path)
{
argp_failure (state, 1, ENOMEM, "adding driver path failed");
return EINVAL;
}
s = arg;
d = driver_path;
while (*s)
{
*(d++) = (*s == ':') ? '\0' : *s;
s++;
}
*(d++) = '\0';
*d = '\0';
}
break;
case 'd':
err = driver_add (arg , arg,
state->argc, state->argv, &state->next, 0);
if (err)
{
argp_failure (state, 1, err, "loading driver `%s' failed", arg);
return EINVAL;
}
devcount++;
break;
case 'c':
console_node = arg ? arg : DEFAULT_CONSOLE_NODE;
if (!console_node)
return ENOMEM;
break;
case DAEMONIZE_KEY:
daemonize = 1;
break;
case ARGP_KEY_SUCCESS:
if (!devcount)
{
argp_error (state, "at least one --driver argument required");
return EINVAL;
}
break;
default:
return ARGP_ERR_UNKNOWN;
}
return 0;
}
static const struct argp_child startup_children[] =
{ { &cons_startup_argp }, { 0 } };
static struct argp startup_argp = {options, parse_opt, 0,
0, startup_children};
#if HAVE_DAEMON
#define daemon_error(status, errnum, format, args...)			\
do									\
{									\
if (daemonize)							\
{								\
if (errnum)							\
daemon_log (LOG_ERR, format ": %s", ##args,			\
strerror(errnum));				\
else								\
daemon_log (LOG_ERR, format, ##args);			\
if (status)							\
{								\
\
daemon_retval_send (status);				\
daemon_pid_file_remove ();				\
return 0;							\
}								\
}								\
else								\
error (status, errnum, format, ##args);				\
}									\
while (0);
#else
#define daemon_error	error
#endif
int
main (int argc, char *argv[])
{
error_t err;
char *errname;
driver_init ();
argp_parse (&startup_argp, argc, argv, ARGP_IN_ORDER, 0, 0);
#if HAVE_DAEMON
if (daemonize)
{
if (daemon_reset_sigs (-1) < 0)
error (1, errno, "Failed to reset all signal handlers");
if (daemon_unblock_sigs (-1) < 0)
error (1, errno, "Failed to unblock all signals");
daemon_pid_file_ident = daemon_log_ident = \
daemon_ident_from_argv0 (argv[0]);
pid_t pid;
if ((pid = daemon_pid_file_is_running ()) >= 0)
error (1, errno, "Daemon already running on PID file %u", pid);
if (daemon_retval_init () < 0)
error (1, errno, "Failed to create pipe.");
if ((pid = daemon_fork ()) < 0)
{
daemon_retval_done ();
error (1, errno, "Failed to fork");
}
else if (pid)
{
int ret;
if ((ret = daemon_retval_wait (20)) < 0)
error (1, errno,
"Could not receive return value from daemon process");
return ret;
}
else
{
if (daemon_close_all (-1) < 0)
daemon_error (1, errno, "Failed to close all file descriptors");
if (daemon_pid_file_create () < 0)
daemon_error (2, errno, "Could not create PID file");
}
}
#endif
err = driver_start (&errname);
if (err)
daemon_error (1, err, "Starting driver %s failed", errname);
pthread_mutex_init (&global_lock, NULL);
err = cons_init ();
if (err)
{
driver_fini ();
daemon_error (1, err, "Console library initialization failed");
}
err = timer_init ();
if (err)
{
driver_fini ();
daemon_error (1, err, "Timer thread initialization failed");
}
if (console_node)
console_setup_node (console_node);
#if HAVE_DAEMON
if (daemonize)
daemon_retval_send (0);
#endif
cons_server_loop ();
console_exit ();
}