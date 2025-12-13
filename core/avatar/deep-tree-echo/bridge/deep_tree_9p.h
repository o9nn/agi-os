/**
 * Deep Tree Echo 9P File Server
 * 
 * Exposes Deep Tree Echo cognitive state and avatar control via 9P protocol.
 * 
 * Namespace:
 * 
 * /mnt/deep-tree-echo/
 * ├── avatars/
 * │   ├── create              # Write avatar name to create
 * │   └── <avatar-name>/
 * │       ├── emotion          # Read/write emotional state (JSON)
 * │       ├── expression       # Read/write expression name
 * │       ├── vorticity        # Read current vorticity
 * │       ├── matula           # Read Matula number
 * │       ├── morphule/        # Morphule control
 * │       │   ├── constraints  # Read constraints
 * │       │   ├── essence      # Read essence level
 * │       │   └── techniques/  # List unlocked techniques
 * │       └── thoughts/
 * │           ├── create       # Write thought content
 * │           └── <thought-id>/
 * │               ├── content  # Read thought content
 * │               ├── matula   # Read thought Matula number
 * │               └── chaos    # Read chaos value
 * ├── egregore/
 * │   ├── coherence            # Read phase coherence
 * │   ├── circulation          # Read total circulation
 * │   ├── collective_emotion   # Read collective emotional state (JSON)
 * │   └── synchronize          # Write to trigger sync
 * └── ctl                      # Control operations
 */

#ifndef _DEEP_TREE_9P_H_
#define _DEEP_TREE_9P_H_

#include <stdint.h>
#include <stdbool.h>
#include "vortex_bridge.h"

/* 9P file types */
typedef enum {
    FILE_TYPE_DIR,
    FILE_TYPE_FILE,
    FILE_TYPE_CTL,
} FileType;

/* 9P file node */
typedef struct FileNode {
    char name[256];               /* File name */
    FileType type;                /* File type */
    uint32_t qid;                 /* Unique file ID */
    
    /* For files */
    char* content;                /* File content (dynamically allocated) */
    size_t size;                  /* Content size */
    
    /* For directories */
    struct FileNode** children;   /* Child nodes */
    int n_children;               /* Number of children */
    
    /* Parent */
    struct FileNode* parent;      /* Parent directory */
    
    /* Callbacks */
    int (*on_read)(struct FileNode* node, char* buf, size_t count, off_t offset);
    int (*on_write)(struct FileNode* node, const char* buf, size_t count, off_t offset);
    
} FileNode;

/* 9P server */
typedef struct {
    VortexBridge* bridge;         /* VORTEX bridge */
    FileNode* root;               /* Root directory */
    
    /* Configuration */
    char mount_point[256];        /* Mount point path */
    uint16_t port;                /* Server port */
    bool running;                 /* Is server running? */
    
} DeepTree9PServer;

/* Server lifecycle */

/**
 * Create 9P server
 * 
 * bridge: VORTEX bridge
 * mount_point: Mount point path (e.g., "/mnt/deep-tree-echo")
 * port: Server port (0 for default)
 * 
 * Returns: Server, or NULL on error
 */
DeepTree9PServer* deep_tree_9p_create(VortexBridge* bridge, const char* mount_point, uint16_t port);

/**
 * Free 9P server
 */
void deep_tree_9p_free(DeepTree9PServer* server);

/**
 * Start 9P server
 * 
 * Returns: 0 on success, -1 on error
 */
int deep_tree_9p_start(DeepTree9PServer* server);

/**
 * Stop 9P server
 */
void deep_tree_9p_stop(DeepTree9PServer* server);

/* File tree construction */

/**
 * Build file tree
 * 
 * Constructs the complete 9P namespace from bridge state.
 * 
 * Returns: 0 on success, -1 on error
 */
int deep_tree_9p_build_tree(DeepTree9PServer* server);

/**
 * Refresh file tree
 * 
 * Updates file tree to reflect current bridge state.
 */
void deep_tree_9p_refresh(DeepTree9PServer* server);

/* File operations */

/**
 * Create file node
 */
FileNode* file_node_create(const char* name, FileType type);

/**
 * Free file node (recursive)
 */
void file_node_free(FileNode* node);

/**
 * Add child to directory
 */
int file_node_add_child(FileNode* parent, FileNode* child);

/**
 * Find child by name
 */
FileNode* file_node_find_child(FileNode* parent, const char* name);

/**
 * Set file content
 */
int file_node_set_content(FileNode* node, const char* content, size_t size);

/* Read/write handlers */

/**
 * Avatar emotion read handler
 */
int avatar_emotion_read(FileNode* node, char* buf, size_t count, off_t offset);

/**
 * Avatar emotion write handler
 */
int avatar_emotion_write(FileNode* node, const char* buf, size_t count, off_t offset);

/**
 * Avatar expression read handler
 */
int avatar_expression_read(FileNode* node, char* buf, size_t count, off_t offset);

/**
 * Avatar expression write handler
 */
int avatar_expression_write(FileNode* node, const char* buf, size_t count, off_t offset);

/**
 * Avatar vorticity read handler
 */
int avatar_vorticity_read(FileNode* node, char* buf, size_t count, off_t offset);

/**
 * Avatar matula read handler
 */
int avatar_matula_read(FileNode* node, char* buf, size_t count, off_t offset);

/**
 * Egregore coherence read handler
 */
int egregore_coherence_read(FileNode* node, char* buf, size_t count, off_t offset);

/**
 * Egregore circulation read handler
 */
int egregore_circulation_read(FileNode* node, char* buf, size_t count, off_t offset);

/**
 * Egregore collective emotion read handler
 */
int egregore_collective_emotion_read(FileNode* node, char* buf, size_t count, off_t offset);

/**
 * Egregore synchronize write handler
 */
int egregore_synchronize_write(FileNode* node, const char* buf, size_t count, off_t offset);

/**
 * Avatar create write handler
 */
int avatar_create_write(FileNode* node, const char* buf, size_t count, off_t offset);

/**
 * Thought create write handler
 */
int thought_create_write(FileNode* node, const char* buf, size_t count, off_t offset);

/* Utilities */

/**
 * Print file tree
 */
void file_tree_print(FileNode* node, int indent);

#endif /* _DEEP_TREE_9P_H_ */
