#include <argp.h>
#include "priv.h"
const struct argp_option diskfs_common_options[] =
{
{"readonly", 'r', 0, 0, "Never write to disk or allow opens for writing"},
{"rdonly",   0,   0, OPTION_ALIAS | OPTION_HIDDEN},
{"ro",       0,   0, OPTION_ALIAS | OPTION_HIDDEN},
{"writable", 'w', 0, 0, "Use normal read/write behavior"},
{"rdwr",     0,   0, OPTION_ALIAS | OPTION_HIDDEN},
{"rw",       0,   0, OPTION_ALIAS | OPTION_HIDDEN},
{"sync",     's', "INTERVAL", OPTION_ARG_OPTIONAL,
"If INTERVAL is supplied, sync all data not actually written to disk"
" every INTERVAL seconds, otherwise operate in synchronous mode (the"
" default is to sync every " DEFAULT_SYNC_INTERVAL_STRING " seconds)"},
{"no-sync",  'n',  0, 0, "Don't automatically sync data to disk"},
{"nosync", 0, 0, OPTION_ALIAS | OPTION_HIDDEN},
{"no-suid",  'S', 0, 0, "Don't permit set-uid or set-gid execution"},
{"nosuid",   0,   0, OPTION_ALIAS | OPTION_HIDDEN},
{"suid-ok", OPT_SUID_OK, 0, 0, "Enable set-uid execution"},
{"no-exec", 'E', 0, 0, "Don't permit any execution of files on this filesystem"},
{"noexec",   0,   0, OPTION_ALIAS | OPTION_HIDDEN},
{"exec-ok", OPT_EXEC_OK, 0, 0, "Enable execution of files"},
{"no-atime", 'A', 0, 0,
"Do not update file access times on disk for reads"},
{"noatime",  0,   0, OPTION_ALIAS | OPTION_HIDDEN},
{"atime", OPT_ATIME, 0, 0, "Do update file access times for reads normally"},
{"strictatime", 0, 0, OPTION_ALIAS | OPTION_HIDDEN},
{"no-inherit-dir-group", OPT_NO_INHERIT_DIR_GROUP, 0, 0,
"Create new nodes with gid of the process"},
{"nogrpid",    0,   0, OPTION_ALIAS | OPTION_HIDDEN},
{"sysvgroups", 0,   0, OPTION_ALIAS | OPTION_HIDDEN},
{"inherit-dir-group", OPT_INHERIT_DIR_GROUP, 0, 0,
"Create new nodes with gid of parent dir (default)"},
{"grpid",    0,   0, OPTION_ALIAS | OPTION_HIDDEN},
{"bsdgroups", 0,   0, OPTION_ALIAS | OPTION_HIDDEN},
{"relatime", 'R', 0, 0,
"Only update access times once daily or if older than change time "
"or modification time."},
{0, 0}
};