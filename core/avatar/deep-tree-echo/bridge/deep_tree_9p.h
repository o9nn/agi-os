#ifndef _DEEP_TREE_9P_H_
#define _DEEP_TREE_9P_H_
#include <stdint.h>
#include <stdbool.h>
#include "vortex_bridge.h"
typedef enum {
FILE_TYPE_DIR,
FILE_TYPE_FILE,
FILE_TYPE_CTL,
} FileType;
typedef struct FileNode {
char name[256];
FileType type;
uint32_t qid;
char* content;
size_t size;
struct FileNode** children;
int n_children;
struct FileNode* parent;
int (*on_read)(struct FileNode* node, char* buf, size_t count, off_t offset);
int (*on_write)(struct FileNode* node, const char* buf, size_t count, off_t offset);
} FileNode;
typedef struct {
VortexBridge* bridge;
FileNode* root;
char mount_point[256];
uint16_t port;
bool running;
} DeepTree9PServer;
DeepTree9PServer* deep_tree_9p_create(VortexBridge* bridge, const char* mount_point, uint16_t port);
void deep_tree_9p_free(DeepTree9PServer* server);
int deep_tree_9p_start(DeepTree9PServer* server);
void deep_tree_9p_stop(DeepTree9PServer* server);
int deep_tree_9p_build_tree(DeepTree9PServer* server);
void deep_tree_9p_refresh(DeepTree9PServer* server);
FileNode* file_node_create(const char* name, FileType type);
void file_node_free(FileNode* node);
int file_node_add_child(FileNode* parent, FileNode* child);
FileNode* file_node_find_child(FileNode* parent, const char* name);
int file_node_set_content(FileNode* node, const char* content, size_t size);
int avatar_emotion_read(FileNode* node, char* buf, size_t count, off_t offset);
int avatar_emotion_write(FileNode* node, const char* buf, size_t count, off_t offset);
int avatar_expression_read(FileNode* node, char* buf, size_t count, off_t offset);
int avatar_expression_write(FileNode* node, const char* buf, size_t count, off_t offset);
int avatar_vorticity_read(FileNode* node, char* buf, size_t count, off_t offset);
int avatar_matula_read(FileNode* node, char* buf, size_t count, off_t offset);
int egregore_coherence_read(FileNode* node, char* buf, size_t count, off_t offset);
int egregore_circulation_read(FileNode* node, char* buf, size_t count, off_t offset);
int egregore_collective_emotion_read(FileNode* node, char* buf, size_t count, off_t offset);
int egregore_synchronize_write(FileNode* node, const char* buf, size_t count, off_t offset);
int avatar_create_write(FileNode* node, const char* buf, size_t count, off_t offset);
int thought_create_write(FileNode* node, const char* buf, size_t count, off_t offset);
void file_tree_print(FileNode* node, int indent);
#endif