/**
 * Deep Tree Echo 9P File Server Implementation
 * 
 * Implements the 9P protocol handlers for exposing Deep Tree Echo cognitive
 * state via filesystem operations.
 */

#include "deep_tree_9p.h"
#include "emotion_json.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* ============================================================================
 * Avatar Emotion Handlers
 * ============================================================================ */

/**
 * Avatar emotion read handler
 * 
 * Reads the current emotional state of an avatar and returns it as JSON.
 * 
 * Path: /mnt/deep-tree-echo/avatars/<avatar-name>/emotion
 */
int avatar_emotion_read(FileNode* node, char* buf, size_t count, off_t offset) {
    if (!node || !buf) return -1;
    
    /* Get avatar name from parent directory */
    if (!node->parent || !node->parent->name) {
        fprintf(stderr, "avatar_emotion_read: no parent directory\n");
        return -1;
    }
    const char* avatar_name = node->parent->name;
    
    /* Get server from node context */
    DeepTree9PServer* server = (DeepTree9PServer*)node->context;
    if (!server || !server->bridge) {
        fprintf(stderr, "avatar_emotion_read: no server context\n");
        return -1;
    }
    
    /* Get avatar state */
    AvatarState* avatar = vortex_bridge_get_avatar(server->bridge, avatar_name);
    if (!avatar) {
        fprintf(stderr, "avatar_emotion_read: avatar '%s' not found\n", avatar_name);
        return -1;
    }
    
    /* Build emotion structure */
    Emotion emotion;
    emotion.joy = avatar->joy;
    emotion.sadness = avatar->sadness;
    emotion.anger = avatar->anger;
    emotion.fear = avatar->fear;
    emotion.surprise = avatar->surprise;
    emotion.disgust = avatar->disgust;
    emotion.vorticity = avatar->vorticity;
    emotion_get_timestamp(emotion.timestamp, sizeof(emotion.timestamp));
    
    /* Serialize to JSON */
    char* json = emotion_to_json(&emotion);
    if (!json) {
        fprintf(stderr, "avatar_emotion_read: JSON serialization failed\n");
        return -1;
    }
    
    size_t json_len = strlen(json);
    
    /* Handle offset and count */
    if (offset >= (off_t)json_len) {
        emotion_json_free(json);
        return 0;  /* EOF */
    }
    
    size_t available = json_len - offset;
    size_t to_copy = (count < available) ? count : available;
    
    memcpy(buf, json + offset, to_copy);
    
    emotion_json_free(json);
    
    return to_copy;
}

/**
 * Avatar emotion write handler
 * 
 * Parses JSON emotion data and updates the avatar's emotional state.
 * Automatically computes vorticity and updates the morphule.
 * 
 * Path: /mnt/deep-tree-echo/avatars/<avatar-name>/emotion
 */
int avatar_emotion_write(FileNode* node, const char* buf, size_t count, off_t offset) {
    if (!node || !buf) return -1;
    
    /* JSON writes must start at offset 0 */
    if (offset != 0) {
        fprintf(stderr, "avatar_emotion_write: offset must be 0 for JSON\n");
        return -1;
    }
    
    /* Get avatar name from parent directory */
    if (!node->parent || !node->parent->name) {
        fprintf(stderr, "avatar_emotion_write: no parent directory\n");
        return -1;
    }
    const char* avatar_name = node->parent->name;
    
    /* Get server from node context */
    DeepTree9PServer* server = (DeepTree9PServer*)node->context;
    if (!server || !server->bridge) {
        fprintf(stderr, "avatar_emotion_write: no server context\n");
        return -1;
    }
    
    /* Null-terminate buffer for JSON parsing */
    char* json_buf = malloc(count + 1);
    if (!json_buf) {
        fprintf(stderr, "avatar_emotion_write: malloc failed\n");
        return -1;
    }
    memcpy(json_buf, buf, count);
    json_buf[count] = '\0';
    
    /* Parse JSON */
    Emotion emotion;
    if (emotion_from_json(json_buf, &emotion) != 0) {
        fprintf(stderr, "avatar_emotion_write: JSON parse failed\n");
        free(json_buf);
        return -1;
    }
    free(json_buf);
    
    /* Update avatar emotion via bridge */
    int result = vortex_bridge_set_emotion(server->bridge, avatar_name,
        emotion.joy, emotion.sadness, emotion.anger,
        emotion.fear, emotion.surprise, emotion.disgust);
    
    if (result != 0) {
        fprintf(stderr, "avatar_emotion_write: vortex_bridge_set_emotion failed\n");
        return -1;
    }
    
    /* Get updated vorticity */
    AvatarState* avatar = vortex_bridge_get_avatar(server->bridge, avatar_name);
    if (avatar) {
        printf("[9P] Avatar '%s' emotion updated: vorticity=%.2f\n",
               avatar_name, avatar->vorticity);
    }
    
    return count;
}

/* ============================================================================
 * Avatar Expression Handlers
 * ============================================================================ */

/**
 * Avatar expression read handler
 */
int avatar_expression_read(FileNode* node, char* buf, size_t count, off_t offset) {
    if (!node || !buf) return -1;
    
    const char* avatar_name = node->parent->name;
    DeepTree9PServer* server = (DeepTree9PServer*)node->context;
    if (!server || !server->bridge) return -1;
    
    const char* expression = vortex_bridge_get_expression(server->bridge, avatar_name);
    if (!expression) return -1;
    
    size_t expr_len = strlen(expression);
    
    if (offset >= (off_t)expr_len) return 0;  /* EOF */
    
    size_t available = expr_len - offset;
    size_t to_copy = (count < available) ? count : available;
    
    memcpy(buf, expression + offset, to_copy);
    
    return to_copy;
}

/**
 * Avatar expression write handler
 */
int avatar_expression_write(FileNode* node, const char* buf, size_t count, off_t offset) {
    if (!node || !buf || offset != 0) return -1;
    
    const char* avatar_name = node->parent->name;
    DeepTree9PServer* server = (DeepTree9PServer*)node->context;
    if (!server || !server->bridge) return -1;
    
    /* Null-terminate expression */
    char* expression = malloc(count + 1);
    if (!expression) return -1;
    memcpy(expression, buf, count);
    expression[count] = '\0';
    
    /* Remove trailing newline if present */
    if (count > 0 && expression[count - 1] == '\n') {
        expression[count - 1] = '\0';
    }
    
    int result = vortex_bridge_set_expression(server->bridge, avatar_name, expression);
    free(expression);
    
    if (result != 0) return -1;
    
    printf("[9P] Avatar '%s' expression set to '%s'\n", avatar_name, expression);
    
    return count;
}

/* ============================================================================
 * Avatar Vorticity Handler (Read-only)
 * ============================================================================ */

/**
 * Avatar vorticity read handler
 */
int avatar_vorticity_read(FileNode* node, char* buf, size_t count, off_t offset) {
    if (!node || !buf) return -1;
    
    const char* avatar_name = node->parent->name;
    DeepTree9PServer* server = (DeepTree9PServer*)node->context;
    if (!server || !server->bridge) return -1;
    
    AvatarState* avatar = vortex_bridge_get_avatar(server->bridge, avatar_name);
    if (!avatar) return -1;
    
    /* Format vorticity as string */
    char vorticity_str[32];
    snprintf(vorticity_str, sizeof(vorticity_str), "%.6f\n", avatar->vorticity);
    
    size_t str_len = strlen(vorticity_str);
    
    if (offset >= (off_t)str_len) return 0;  /* EOF */
    
    size_t available = str_len - offset;
    size_t to_copy = (count < available) ? count : available;
    
    memcpy(buf, vorticity_str + offset, to_copy);
    
    return to_copy;
}

/* ============================================================================
 * Avatar Matula Handler (Read-only)
 * ============================================================================ */

/**
 * Avatar matula read handler
 */
int avatar_matula_read(FileNode* node, char* buf, size_t count, off_t offset) {
    if (!node || !buf) return -1;
    
    const char* avatar_name = node->parent->name;
    DeepTree9PServer* server = (DeepTree9PServer*)node->context;
    if (!server || !server->bridge) return -1;
    
    AvatarState* avatar = vortex_bridge_get_avatar(server->bridge, avatar_name);
    if (!avatar) return -1;
    
    /* Format Matula number as string */
    char matula_str[32];
    snprintf(matula_str, sizeof(matula_str), "%lu\n", avatar->matula);
    
    size_t str_len = strlen(matula_str);
    
    if (offset >= (off_t)str_len) return 0;  /* EOF */
    
    size_t available = str_len - offset;
    size_t to_copy = (count < available) ? count : available;
    
    memcpy(buf, matula_str + offset, to_copy);
    
    return to_copy;
}

/* ============================================================================
 * Egregore Collective Emotion Handler
 * ============================================================================ */

/**
 * Collective emotion read handler
 * 
 * Computes the collective emotional state across all avatars in the egregore
 * and returns it as JSON with additional swarm metrics.
 * 
 * Path: /mnt/deep-tree-echo/egregore/collective_emotion
 */
int egregore_collective_emotion_read(FileNode* node, char* buf, size_t count, off_t offset) {
    if (!node || !buf) return -1;
    
    /* Get server from node context */
    DeepTree9PServer* server = (DeepTree9PServer*)node->context;
    if (!server || !server->bridge) {
        fprintf(stderr, "egregore_collective_emotion_read: no server context\n");
        return -1;
    }
    
    VortexBridge* bridge = server->bridge;
    
    /* Build collective emotion structure */
    CollectiveEmotion collective;
    memset(&collective, 0, sizeof(CollectiveEmotion));
    
    /* Get collective emotion from bridge */
    vortex_bridge_get_collective_emotion(bridge,
        &collective.emotion.joy,
        &collective.emotion.sadness,
        &collective.emotion.anger,
        &collective.emotion.fear,
        &collective.emotion.surprise,
        &collective.emotion.disgust);
    
    /* Compute collective vorticity */
    collective.emotion.vorticity = vortex_bridge_emotion_to_vorticity(
        collective.emotion.joy,
        collective.emotion.sadness,
        collective.emotion.anger,
        collective.emotion.fear,
        collective.emotion.surprise,
        collective.emotion.disgust);
    
    emotion_get_timestamp(collective.emotion.timestamp, 
                         sizeof(collective.emotion.timestamp));
    
    /* Get egregore metrics */
    collective.n_avatars = bridge->n_avatars;
    
    if (bridge->egregore) {
        collective.coherence = egregore_get_coherence(bridge->egregore);
        collective.circulation = bridge->egregore->circulation;
    } else {
        collective.coherence = 0.0;
        collective.circulation = 0.0;
    }
    
    /* Build avatar list */
    collective.n_avatar_entries = bridge->n_avatars;
    if (bridge->n_avatars > 0) {
        collective.avatars = malloc(sizeof(*collective.avatars) * bridge->n_avatars);
        if (!collective.avatars) {
            fprintf(stderr, "egregore_collective_emotion_read: malloc failed\n");
            return -1;
        }
        
        for (int i = 0; i < bridge->n_avatars; i++) {
            AvatarState* avatar = bridge->avatars[i];
            strncpy(collective.avatars[i].name, avatar->name, 
                    sizeof(collective.avatars[i].name) - 1);
            collective.avatars[i].name[sizeof(collective.avatars[i].name) - 1] = '\0';
            collective.avatars[i].vorticity = avatar->vorticity;
        }
    }
    
    /* Serialize to JSON */
    char* json = collective_emotion_to_json(&collective);
    if (collective.avatars) {
        free(collective.avatars);
    }
    
    if (!json) {
        fprintf(stderr, "egregore_collective_emotion_read: JSON serialization failed\n");
        return -1;
    }
    
    size_t json_len = strlen(json);
    
    /* Handle offset and count */
    if (offset >= (off_t)json_len) {
        emotion_json_free(json);
        return 0;  /* EOF */
    }
    
    size_t available = json_len - offset;
    size_t to_copy = (count < available) ? count : available;
    
    memcpy(buf, json + offset, to_copy);
    
    emotion_json_free(json);
    
    return to_copy;
}

/* ============================================================================
 * Egregore Coherence Handler (Read-only)
 * ============================================================================ */

/**
 * Egregore coherence read handler
 */
int egregore_coherence_read(FileNode* node, char* buf, size_t count, off_t offset) {
    if (!node || !buf) return -1;
    
    DeepTree9PServer* server = (DeepTree9PServer*)node->context;
    if (!server || !server->bridge || !server->bridge->egregore) return -1;
    
    double coherence = egregore_get_coherence(server->bridge->egregore);
    
    char coherence_str[32];
    snprintf(coherence_str, sizeof(coherence_str), "%.6f\n", coherence);
    
    size_t str_len = strlen(coherence_str);
    
    if (offset >= (off_t)str_len) return 0;  /* EOF */
    
    size_t available = str_len - offset;
    size_t to_copy = (count < available) ? count : available;
    
    memcpy(buf, coherence_str + offset, to_copy);
    
    return to_copy;
}

/* ============================================================================
 * Egregore Circulation Handler (Read-only)
 * ============================================================================ */

/**
 * Egregore circulation read handler
 */
int egregore_circulation_read(FileNode* node, char* buf, size_t count, off_t offset) {
    if (!node || !buf) return -1;
    
    DeepTree9PServer* server = (DeepTree9PServer*)node->context;
    if (!server || !server->bridge || !server->bridge->egregore) return -1;
    
    double circulation = server->bridge->egregore->circulation;
    
    char circulation_str[32];
    snprintf(circulation_str, sizeof(circulation_str), "%.6f\n", circulation);
    
    size_t str_len = strlen(circulation_str);
    
    if (offset >= (off_t)str_len) return 0;  /* EOF */
    
    size_t available = str_len - offset;
    size_t to_copy = (count < available) ? count : available;
    
    memcpy(buf, circulation_str + offset, to_copy);
    
    return to_copy;
}

/* ============================================================================
 * Egregore Synchronize Handler (Write-only)
 * ============================================================================ */

/**
 * Egregore synchronize write handler
 */
int egregore_synchronize_write(FileNode* node, const char* buf, size_t count, off_t offset) {
    if (!node || !buf || offset != 0) return -1;
    
    DeepTree9PServer* server = (DeepTree9PServer*)node->context;
    if (!server || !server->bridge) return -1;
    
    /* Trigger synchronization */
    double coherence = vortex_bridge_synchronize(server->bridge);
    
    printf("[9P] Egregore synchronized: coherence=%.2f\n", coherence);
    
    return count;
}

/* ============================================================================
 * Control File Handler
 * ============================================================================ */

/**
 * Control file write handler
 */
int ctl_write(FileNode* node, const char* buf, size_t count, off_t offset) {
    if (!node || !buf || offset != 0) return -1;
    
    DeepTree9PServer* server = (DeepTree9PServer*)node->context;
    if (!server) return -1;
    
    /* Null-terminate command */
    char* cmd = malloc(count + 1);
    if (!cmd) return -1;
    memcpy(cmd, buf, count);
    cmd[count] = '\0';
    
    /* Remove trailing newline */
    if (count > 0 && cmd[count - 1] == '\n') {
        cmd[count - 1] = '\0';
    }
    
    printf("[9P] Control command: %s\n", cmd);
    
    /* TODO: Implement control commands */
    /* Examples:
     * - "refresh" - rebuild file tree
     * - "sync" - synchronize egregore
     * - "debug on/off" - toggle debug mode
     */
    
    free(cmd);
    
    return count;
}

/* ============================================================================
 * File Node Management
 * ============================================================================ */

/**
 * Create file node
 */
FileNode* file_node_create(const char* name, FileType type) {
    if (!name) return NULL;
    
    FileNode* node = calloc(1, sizeof(FileNode));
    if (!node) return NULL;
    
    strncpy(node->name, name, sizeof(node->name) - 1);
    node->name[sizeof(node->name) - 1] = '\0';
    node->type = type;
    node->qid = 0;  /* Will be assigned by server */
    
    node->content = NULL;
    node->size = 0;
    
    node->children = NULL;
    node->n_children = 0;
    node->parent = NULL;
    
    node->context = NULL;
    
    node->on_read = NULL;
    node->on_write = NULL;
    
    return node;
}

/**
 * Free file node (recursive)
 */
void file_node_free(FileNode* node) {
    if (!node) return;
    
    /* Free children */
    if (node->children) {
        for (int i = 0; i < node->n_children; i++) {
            file_node_free(node->children[i]);
        }
        free(node->children);
    }
    
    /* Free content */
    if (node->content) {
        free(node->content);
    }
    
    free(node);
}

/**
 * Add child to directory
 */
int file_node_add_child(FileNode* parent, FileNode* child) {
    if (!parent || !child) return -1;
    if (parent->type != FILE_TYPE_DIR) return -1;
    
    /* Reallocate children array */
    FileNode** new_children = realloc(parent->children,
        sizeof(FileNode*) * (parent->n_children + 1));
    if (!new_children) return -1;
    
    parent->children = new_children;
    parent->children[parent->n_children] = child;
    parent->n_children++;
    
    child->parent = parent;
    
    return 0;
}

/**
 * Find child by name
 */
FileNode* file_node_find_child(FileNode* parent, const char* name) {
    if (!parent || !name) return NULL;
    if (parent->type != FILE_TYPE_DIR) return NULL;
    
    for (int i = 0; i < parent->n_children; i++) {
        if (strcmp(parent->children[i]->name, name) == 0) {
            return parent->children[i];
        }
    }
    
    return NULL;
}

/**
 * Set file content
 */
int file_node_set_content(FileNode* node, const char* content, size_t size) {
    if (!node || !content) return -1;
    
    /* Free old content */
    if (node->content) {
        free(node->content);
    }
    
    /* Allocate new content */
    node->content = malloc(size);
    if (!node->content) return -1;
    
    memcpy(node->content, content, size);
    node->size = size;
    
    return 0;
}

/* ============================================================================
 * File Tree Construction
 * ============================================================================ */

/**
 * Build file tree for avatar
 */
FileNode* build_avatar_tree(DeepTree9PServer* server, const char* avatar_name) {
    /* Create avatar directory */
    FileNode* avatar_dir = file_node_create(avatar_name, FILE_TYPE_DIR);
    if (!avatar_dir) return NULL;
    
    avatar_dir->context = server;
    
    /* Create emotion file */
    FileNode* emotion = file_node_create("emotion", FILE_TYPE_FILE);
    emotion->context = server;
    emotion->on_read = avatar_emotion_read;
    emotion->on_write = avatar_emotion_write;
    file_node_add_child(avatar_dir, emotion);
    
    /* Create expression file */
    FileNode* expression = file_node_create("expression", FILE_TYPE_FILE);
    expression->context = server;
    expression->on_read = avatar_expression_read;
    expression->on_write = avatar_expression_write;
    file_node_add_child(avatar_dir, expression);
    
    /* Create vorticity file (read-only) */
    FileNode* vorticity = file_node_create("vorticity", FILE_TYPE_FILE);
    vorticity->context = server;
    vorticity->on_read = avatar_vorticity_read;
    file_node_add_child(avatar_dir, vorticity);
    
    /* Create matula file (read-only) */
    FileNode* matula = file_node_create("matula", FILE_TYPE_FILE);
    matula->context = server;
    matula->on_read = avatar_matula_read;
    file_node_add_child(avatar_dir, matula);
    
    /* TODO: Add morphule/ directory */
    
    return avatar_dir;
}

/**
 * Build complete file tree
 */
int deep_tree_9p_build_tree(DeepTree9PServer* server) {
    if (!server || !server->bridge) return -1;
    
    /* Create root */
    server->root = file_node_create("/", FILE_TYPE_DIR);
    if (!server->root) return -1;
    
    server->root->context = server;
    
    /* Create avatars/ directory */
    FileNode* avatars_dir = file_node_create("avatars", FILE_TYPE_DIR);
    avatars_dir->context = server;
    file_node_add_child(server->root, avatars_dir);
    
    /* Create avatar subdirectories */
    for (int i = 0; i < server->bridge->n_avatars; i++) {
        AvatarState* avatar = server->bridge->avatars[i];
        FileNode* avatar_tree = build_avatar_tree(server, avatar->name);
        if (avatar_tree) {
            file_node_add_child(avatars_dir, avatar_tree);
        }
    }
    
    /* Create egregore/ directory */
    FileNode* egregore_dir = file_node_create("egregore", FILE_TYPE_DIR);
    egregore_dir->context = server;
    file_node_add_child(server->root, egregore_dir);
    
    /* Create collective_emotion file */
    FileNode* collective_emotion = file_node_create("collective_emotion", FILE_TYPE_FILE);
    collective_emotion->context = server;
    collective_emotion->on_read = egregore_collective_emotion_read;
    file_node_add_child(egregore_dir, collective_emotion);
    
    /* Create coherence file */
    FileNode* coherence = file_node_create("coherence", FILE_TYPE_FILE);
    coherence->context = server;
    coherence->on_read = egregore_coherence_read;
    file_node_add_child(egregore_dir, coherence);
    
    /* Create circulation file */
    FileNode* circulation = file_node_create("circulation", FILE_TYPE_FILE);
    circulation->context = server;
    circulation->on_read = egregore_circulation_read;
    file_node_add_child(egregore_dir, circulation);
    
    /* Create ctl file */
    FileNode* ctl = file_node_create("ctl", FILE_TYPE_CTL);
    ctl->context = server;
    ctl->on_write = ctl_write;
    file_node_add_child(server->root, ctl);
    
    printf("[9P] File tree built: %d avatars\n", server->bridge->n_avatars);
    
    return 0;
}

/**
 * Refresh file tree
 */
void deep_tree_9p_refresh(DeepTree9PServer* server) {
    if (!server) return;
    
    /* Free old tree */
    if (server->root) {
        file_node_free(server->root);
        server->root = NULL;
    }
    
    /* Rebuild */
    deep_tree_9p_build_tree(server);
}

/* ============================================================================
 * Server Lifecycle (Stubs)
 * ============================================================================ */

/**
 * Create 9P server
 */
DeepTree9PServer* deep_tree_9p_create(VortexBridge* bridge, const char* mount_point, uint16_t port) {
    if (!bridge || !mount_point) return NULL;
    
    DeepTree9PServer* server = calloc(1, sizeof(DeepTree9PServer));
    if (!server) return NULL;
    
    server->bridge = bridge;
    server->root = NULL;
    
    strncpy(server->mount_point, mount_point, sizeof(server->mount_point) - 1);
    server->mount_point[sizeof(server->mount_point) - 1] = '\0';
    
    server->port = (port == 0) ? 5640 : port;  /* Default 9P port */
    server->running = false;
    
    return server;
}

/**
 * Free 9P server
 */
void deep_tree_9p_free(DeepTree9PServer* server) {
    if (!server) return;
    
    if (server->root) {
        file_node_free(server->root);
    }
    
    free(server);
}

/**
 * Start 9P server (stub)
 */
int deep_tree_9p_start(DeepTree9PServer* server) {
    if (!server) return -1;
    
    /* Build file tree */
    if (deep_tree_9p_build_tree(server) != 0) {
        return -1;
    }
    
    /* TODO: Start actual 9P protocol server */
    printf("[9P] Server started on port %d\n", server->port);
    printf("[9P] Mount point: %s\n", server->mount_point);
    
    server->running = true;
    
    return 0;
}

/**
 * Stop 9P server (stub)
 */
void deep_tree_9p_stop(DeepTree9PServer* server) {
    if (!server) return;
    
    /* TODO: Stop actual 9P protocol server */
    
    server->running = false;
    
    printf("[9P] Server stopped\n");
}

/* ============================================================================
 * Utilities
 * ============================================================================ */

/**
 * Print file tree
 */
void file_tree_print(FileNode* node, int indent) {
    if (!node) return;
    
    for (int i = 0; i < indent; i++) {
        printf("  ");
    }
    
    printf("%s", node->name);
    
    if (node->type == FILE_TYPE_DIR) {
        printf("/\n");
        for (int i = 0; i < node->n_children; i++) {
            file_tree_print(node->children[i], indent + 1);
        }
    } else {
        printf("\n");
    }
}
