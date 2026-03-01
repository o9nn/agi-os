/*
 * Echo Angel 9P Server for Inferno Kernel
 *
 * Middle agi-os layer: Registers the Echo Angel as a 9P file server
 * within the Inferno kernel's cognitive namespace, making it accessible
 * at /mnt/cog/angel/<name>/ alongside other cognitive services.
 *
 * Namespace layout:
 *   /mnt/cog/
 *   ├── atoms/          # AtomSpace (existing)
 *   ├── reasoning/      # PLN (existing)
 *   ├── attention/      # ECAN (existing)
 *   ├── angel/          # Echo Angel (NEW)
 *   │   ├── ctl
 *   │   ├── status
 *   │   ├── input
 *   │   ├── response
 *   │   ├── endocrine/
 *   │   ├── expression/
 *   │   ├── cognition/
 *   │   ├── self/
 *   │   ├── platform/
 *   │   └── stats
 *   └── learning/       # MOSES (existing)
 *
 * Copyright (C) 2026 OpenCog Community
 * Licensed under AGPL-3.0
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../../avatar/echo-angel/include/echo_angel.h"

/* ================================================================
 * 9P File Server State
 * ================================================================ */

typedef struct Angel9PServer {
    EchoAngel  angel;
    int        running;
    char       mount_path[256];
} Angel9PServer;

static Angel9PServer g_server = {.running = 0};

/* ================================================================
 * 9P Operations
 * ================================================================ */

typedef struct Angel9PFile {
    const char *name;
    int         is_dir;
    int         readable;
    int         writable;
} Angel9PFile;

static const Angel9PFile g_files[] = {
    {"angel",                1, 0, 0},
    {"angel/ctl",            0, 0, 1},
    {"angel/status",         0, 1, 0},
    {"angel/input",          0, 0, 1},
    {"angel/response",       0, 1, 0},
    {"angel/stats",          0, 1, 0},
    {"angel/endocrine",      1, 0, 0},
    {"angel/endocrine/hormones",  0, 1, 0},
    {"angel/endocrine/valence",   0, 1, 0},
    {"angel/endocrine/arousal",   0, 1, 0},
    {"angel/endocrine/stimulate", 0, 0, 1},
    {"angel/expression",     1, 0, 0},
    {"angel/expression/facs",     0, 1, 0},
    {"angel/expression/morph",    0, 1, 0},
    {"angel/cognition",      1, 0, 0},
    {"angel/cognition/4e",        0, 1, 0},
    {"angel/cognition/reservoir", 0, 1, 0},
    {"angel/cognition/step",      0, 1, 0},
    {"angel/self",           1, 0, 0},
    {"angel/self/image",     0, 1, 0},
    {"angel/self/wisdom",    0, 1, 0},
    {"angel/self/stage",     0, 1, 0},
    {"angel/platform",       1, 0, 0},
    {"angel/platform/chat",  1, 0, 0},
    {"angel/platform/stream",1, 0, 0},
    {NULL, 0, 0, 0}
};

/* ================================================================
 * Server Lifecycle
 * ================================================================ */

int
angel_9p_server_init(const char *name, const char *mount_path)
{
    if (g_server.running)
        return -1; /* Already running */

    if (echo_angel_init(&g_server.angel, name) != 0)
        return -1;

    strncpy(g_server.mount_path, mount_path,
            sizeof(g_server.mount_path) - 1);
    g_server.running = 1;

    printf("[echo-angel-9p] Server initialized: %s at %s\n",
           name, mount_path);
    return 0;
}

int
angel_9p_server_shutdown(void)
{
    if (!g_server.running)
        return -1;

    echo_angel_shutdown(&g_server.angel);
    g_server.running = 0;

    printf("[echo-angel-9p] Server shutdown\n");
    return 0;
}

/* ================================================================
 * 9P Read/Write Dispatch
 * ================================================================ */

int
angel_9p_server_read(const char *path, char *buf, size_t maxlen)
{
    if (!g_server.running || path == NULL || buf == NULL)
        return -1;

    /* Strip mount_path prefix if present */
    const char *rel_path = path;
    size_t mp_len = strlen(g_server.mount_path);
    if (strncmp(path, g_server.mount_path, mp_len) == 0)
        rel_path = path + mp_len;
    if (rel_path[0] == '/')
        rel_path++;

    /* Strip "angel/" prefix for the kernel module */
    if (strncmp(rel_path, "angel/", 6) == 0)
        rel_path += 6;

    return echo_angel_9p_read(&g_server.angel, rel_path, buf, maxlen);
}

int
angel_9p_server_write(const char *path, const char *data, size_t len)
{
    if (!g_server.running || path == NULL || data == NULL)
        return -1;

    const char *rel_path = path;
    size_t mp_len = strlen(g_server.mount_path);
    if (strncmp(path, g_server.mount_path, mp_len) == 0)
        rel_path = path + mp_len;
    if (rel_path[0] == '/')
        rel_path++;

    if (strncmp(rel_path, "angel/", 6) == 0)
        rel_path += 6;

    return echo_angel_9p_write(&g_server.angel, rel_path, data, len);
}

/* ================================================================
 * Directory Listing
 * ================================================================ */

int
angel_9p_server_readdir(const char *path, char *buf, size_t maxlen)
{
    if (!g_server.running || path == NULL || buf == NULL)
        return -1;

    const char *rel_path = path;
    if (rel_path[0] == '/') rel_path++;

    int n = 0;
    size_t prefix_len = strlen(rel_path);

    for (int i = 0; g_files[i].name != NULL && (size_t)n < maxlen - 80; i++) {
        const char *fname = g_files[i].name;

        /* Check if this file is a direct child of the requested directory */
        if (strncmp(fname, rel_path, prefix_len) == 0) {
            const char *child = fname + prefix_len;
            if (child[0] == '/') child++;
            if (child[0] == '\0') continue; /* Skip the directory itself */

            /* Only include direct children (no further slashes) */
            if (strchr(child, '/') == NULL) {
                n += snprintf(buf + n, maxlen - n, "%s%s\n",
                              child, g_files[i].is_dir ? "/" : "");
            }
        }
    }

    return n;
}

/* ================================================================
 * Stat
 * ================================================================ */

int
angel_9p_server_stat(const char *path, char *buf, size_t maxlen)
{
    if (!g_server.running || path == NULL || buf == NULL)
        return -1;

    const char *rel_path = path;
    if (rel_path[0] == '/') rel_path++;

    for (int i = 0; g_files[i].name != NULL; i++) {
        if (strcmp(g_files[i].name, rel_path) == 0) {
            return snprintf(buf, maxlen,
                "name=%s\ntype=%s\nreadable=%d\nwritable=%d\n",
                g_files[i].name,
                g_files[i].is_dir ? "dir" : "file",
                g_files[i].readable,
                g_files[i].writable);
        }
    }

    return snprintf(buf, maxlen, "error: not found '%s'\n", path);
}
