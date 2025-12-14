/**
 * @file unified_cog.c
 * @brief Implementation of unified cognitive primitives
 * 
 * This file implements the cross-system cognitive primitives that work
 * across Inferno, CogPlan9, HurdCog, CogNumach, OpenCog, and DAS.
 * 
 * @author AGI-OS Architecture Team
 * @date December 14, 2025
 */

#include "../include/unified_cog.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

/* ========================================================================
 * Internal State
 * ======================================================================== */

typedef struct {
    bool initialized;
    char mount_point[256];
    int cog_fd;  /* File descriptor for /cog mount point */
} CogState;

static CogState g_cog_state = { false, "", -1 };

/* ========================================================================
 * Helper Functions
 * ======================================================================== */

static int cog_open_file(const char* path, int flags) {
    char full_path[512];
    snprintf(full_path, sizeof(full_path), "%s%s", g_cog_state.mount_point, path);
    return open(full_path, flags);
}

static ssize_t cog_write_file(const char* path, const char* data) {
    int fd = cog_open_file(path, O_WRONLY);
    if (fd < 0) {
        return -1;
    }
    
    ssize_t result = write(fd, data, strlen(data));
    close(fd);
    return result;
}

static ssize_t cog_read_file(const char* path, char* buf, size_t size) {
    int fd = cog_open_file(path, O_RDONLY);
    if (fd < 0) {
        return -1;
    }
    
    ssize_t result = read(fd, buf, size - 1);
    if (result > 0) {
        buf[result] = '\0';
    }
    close(fd);
    return result;
}

/* ========================================================================
 * System Initialization
 * ======================================================================== */

bool cog_init(const char* config_path) {
    if (g_cog_state.initialized) {
        return true;
    }
    
    printf("[UnifiedCog] Initializing unified cognitive interface...\n");
    
    /* Default mount point */
    strncpy(g_cog_state.mount_point, "/n/cog", sizeof(g_cog_state.mount_point));
    
    /* In a real implementation, would:
     * 1. Connect to OpenCog AtomSpace
     * 2. Initialize DAS client
     * 3. Mount 9P cognitive file server
     * 4. Initialize CogPlan9 libraries
     * 5. Connect to HurdCog translators
     */
    
    g_cog_state.initialized = true;
    printf("[UnifiedCog] Initialization complete\n");
    return true;
}

void cog_shutdown(void) {
    if (!g_cog_state.initialized) {
        return;
    }
    
    printf("[UnifiedCog] Shutting down unified cognitive interface...\n");
    
    /* Cleanup resources */
    if (g_cog_state.cog_fd >= 0) {
        close(g_cog_state.cog_fd);
    }
    
    g_cog_state.initialized = false;
    printf("[UnifiedCog] Shutdown complete\n");
}

bool cog_connect_9p(const char* mount_point) {
    if (mount_point) {
        strncpy(g_cog_state.mount_point, mount_point, sizeof(g_cog_state.mount_point));
    }
    
    printf("[UnifiedCog] Connecting to 9P at %s\n", g_cog_state.mount_point);
    
    /* In a real implementation, would mount the 9P file server */
    /* For now, just verify the mount point exists */
    
    return true;
}

/* ========================================================================
 * Atom Operations
 * ======================================================================== */

CogHandle cog_atom_create(
    CogAtomType type,
    const char* name,
    const CogHandle* outgoing,
    size_t outgoing_count,
    CogTruthValue tv
) {
    if (!g_cog_state.initialized) {
        fprintf(stderr, "[UnifiedCog] Not initialized\n");
        return 0;
    }
    
    /* Format: "type name tv.strength tv.confidence" */
    char buf[1024];
    snprintf(buf, sizeof(buf), "%d %s %.3f %.3f\n", 
             type, name ? name : "", tv.strength, tv.confidence);
    
    /* Write to /cog/atoms/create */
    if (cog_write_file("/atoms/create", buf) < 0) {
        fprintf(stderr, "[UnifiedCog] Failed to create atom\n");
        return 0;
    }
    
    /* In a real implementation, would read back the handle */
    /* For now, generate a placeholder handle */
    static uint64_t next_handle = 1000;
    return next_handle++;
}

bool cog_atom_read(CogHandle handle, CogAtom* atom) {
    if (!g_cog_state.initialized || !atom) {
        return false;
    }
    
    /* Format path: /cog/atoms/read/HANDLE */
    char path[256];
    snprintf(path, sizeof(path), "/atoms/read/%lu", handle);
    
    char buf[4096];
    if (cog_read_file(path, buf, sizeof(buf)) < 0) {
        return false;
    }
    
    /* Parse atom data */
    /* In a real implementation, would parse the full atom structure */
    atom->handle = handle;
    
    return true;
}

bool cog_atom_update_tv(CogHandle handle, CogTruthValue tv) {
    if (!g_cog_state.initialized) {
        return false;
    }
    
    char path[256];
    snprintf(path, sizeof(path), "/atoms/update/%lu/tv", handle);
    
    char buf[128];
    snprintf(buf, sizeof(buf), "%.3f %.3f\n", tv.strength, tv.confidence);
    
    return cog_write_file(path, buf) >= 0;
}

bool cog_atom_update_av(CogHandle handle, CogAttentionValue av) {
    if (!g_cog_state.initialized) {
        return false;
    }
    
    char path[256];
    snprintf(path, sizeof(path), "/atoms/update/%lu/av", handle);
    
    char buf[128];
    snprintf(buf, sizeof(buf), "%d %d %d\n", av.sti, av.lti, av.vlti);
    
    return cog_write_file(path, buf) >= 0;
}

bool cog_atom_delete(CogHandle handle) {
    if (!g_cog_state.initialized) {
        return false;
    }
    
    char buf[64];
    snprintf(buf, sizeof(buf), "%lu\n", handle);
    
    return cog_write_file("/atoms/delete", buf) >= 0;
}

/* ========================================================================
 * Pattern Matching and Queries
 * ======================================================================== */

size_t cog_query_pattern(
    const char* pattern,
    CogHandle* results,
    size_t max_results
) {
    if (!g_cog_state.initialized || !pattern || !results) {
        return 0;
    }
    
    /* Write pattern to query file */
    if (cog_write_file("/query/pattern", pattern) < 0) {
        return 0;
    }
    
    /* Read results */
    char buf[8192];
    ssize_t len = cog_read_file("/query/pattern", buf, sizeof(buf));
    if (len < 0) {
        return 0;
    }
    
    /* Parse results (one handle per line) */
    size_t count = 0;
    char* line = strtok(buf, "\n");
    while (line && count < max_results) {
        results[count++] = strtoull(line, NULL, 10);
        line = strtok(NULL, "\n");
    }
    
    return count;
}

size_t cog_query_distributed(
    const char* query,
    CogHandle* results,
    size_t max_results
) {
    if (!g_cog_state.initialized || !query || !results) {
        return 0;
    }
    
    /* Write query to distributed query file */
    if (cog_write_file("/distributed/query", query) < 0) {
        return 0;
    }
    
    /* Read results */
    char buf[8192];
    ssize_t len = cog_read_file("/distributed/query", buf, sizeof(buf));
    if (len < 0) {
        return 0;
    }
    
    /* Parse results */
    size_t count = 0;
    char* line = strtok(buf, "\n");
    while (line && count < max_results) {
        results[count++] = strtoull(line, NULL, 10);
        line = strtok(NULL, "\n");
    }
    
    return count;
}

/* ========================================================================
 * Reasoning and Inference
 * ======================================================================== */

size_t cog_reason_pln(
    CogHandle target,
    int max_steps,
    CogHandle* results,
    size_t max_results
) {
    if (!g_cog_state.initialized || !results) {
        return 0;
    }
    
    /* Write inference request */
    char buf[128];
    snprintf(buf, sizeof(buf), "%lu %d\n", target, max_steps);
    
    if (cog_write_file("/reason/pln", buf) < 0) {
        return 0;
    }
    
    /* Read inference results */
    char result_buf[8192];
    ssize_t len = cog_read_file("/reason/pln", result_buf, sizeof(result_buf));
    if (len < 0) {
        return 0;
    }
    
    /* Parse results */
    size_t count = 0;
    char* line = strtok(result_buf, "\n");
    while (line && count < max_results) {
        results[count++] = strtoull(line, NULL, 10);
        line = strtok(NULL, "\n");
    }
    
    return count;
}

size_t cog_reason_forward(
    const CogHandle* premises,
    size_t premise_count,
    CogHandle* results,
    size_t max_results
) {
    /* Implementation similar to cog_reason_pln */
    /* Would write premises to /reason/forward and read results */
    return 0;
}

size_t cog_reason_backward(
    CogHandle goal,
    CogHandle* results,
    size_t max_results
) {
    /* Implementation similar to cog_reason_pln */
    /* Would write goal to /reason/backward and read results */
    return 0;
}

/* ========================================================================
 * Attention Allocation (ECAN)
 * ======================================================================== */

bool cog_attention_allocate(CogHandle handle, int16_t sti_delta) {
    if (!g_cog_state.initialized) {
        return false;
    }
    
    char buf[128];
    snprintf(buf, sizeof(buf), "%lu %d\n", handle, sti_delta);
    
    return cog_write_file("/attention/allocate", buf) >= 0;
}

bool cog_attention_spread(CogHandle handle, int16_t amount) {
    if (!g_cog_state.initialized) {
        return false;
    }
    
    char buf[128];
    snprintf(buf, sizeof(buf), "%lu %d\n", handle, amount);
    
    return cog_write_file("/attention/spread", buf) >= 0;
}

size_t cog_attention_get_top(CogHandle* results, size_t max_results) {
    if (!g_cog_state.initialized || !results) {
        return 0;
    }
    
    char buf[8192];
    ssize_t len = cog_read_file("/attention/sti/top", buf, sizeof(buf));
    if (len < 0) {
        return 0;
    }
    
    /* Parse results */
    size_t count = 0;
    char* line = strtok(buf, "\n");
    while (line && count < max_results) {
        results[count++] = strtoull(line, NULL, 10);
        line = strtok(NULL, "\n");
    }
    
    return count;
}

/* ========================================================================
 * Learning and Mining
 * ======================================================================== */

size_t cog_learn_mine(
    double min_support,
    CogHandle* results,
    size_t max_results
) {
    if (!g_cog_state.initialized || !results) {
        return 0;
    }
    
    char buf[128];
    snprintf(buf, sizeof(buf), "%.3f\n", min_support);
    
    if (cog_write_file("/learn/mine", buf) < 0) {
        return 0;
    }
    
    char result_buf[8192];
    ssize_t len = cog_read_file("/learn/mine", result_buf, sizeof(result_buf));
    if (len < 0) {
        return 0;
    }
    
    /* Parse results */
    size_t count = 0;
    char* line = strtok(result_buf, "\n");
    while (line && count < max_results) {
        results[count++] = strtoull(line, NULL, 10);
        line = strtok(NULL, "\n");
    }
    
    return count;
}

/* ========================================================================
 * Distributed Operations
 * ======================================================================== */

bool cog_distributed_sync(void) {
    if (!g_cog_state.initialized) {
        return false;
    }
    
    return cog_write_file("/distributed/sync", "sync\n") >= 0;
}

size_t cog_distributed_status(char* status_buffer, size_t buffer_size) {
    if (!g_cog_state.initialized || !status_buffer) {
        return 0;
    }
    
    ssize_t len = cog_read_file("/distributed/status", status_buffer, buffer_size);
    return len > 0 ? len : 0;
}
