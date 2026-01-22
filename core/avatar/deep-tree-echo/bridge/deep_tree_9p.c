#include "deep_tree_9p.h"
#include "emotion_json.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
int avatar_emotion_read(FileNode* node, char* buf, size_t count, off_t offset) {
if (!node || !buf) return -1;
if (!node->parent || !node->parent->name) {
fprintf(stderr, "avatar_emotion_read: no parent directory\n");
return -1;
}
const char* avatar_name = node->parent->name;
DeepTree9PServer* server = (DeepTree9PServer*)node->context;
if (!server || !server->bridge) {
fprintf(stderr, "avatar_emotion_read: no server context\n");
return -1;
}
AvatarState* avatar = vortex_bridge_get_avatar(server->bridge, avatar_name);
if (!avatar) {
fprintf(stderr, "avatar_emotion_read: avatar '%s' not found\n", avatar_name);
return -1;
}
Emotion emotion;
emotion.joy = avatar->joy;
emotion.sadness = avatar->sadness;
emotion.anger = avatar->anger;
emotion.fear = avatar->fear;
emotion.surprise = avatar->surprise;
emotion.disgust = avatar->disgust;
emotion.vorticity = avatar->vorticity;
emotion_get_timestamp(emotion.timestamp, sizeof(emotion.timestamp));
char* json = emotion_to_json(&emotion);
if (!json) {
fprintf(stderr, "avatar_emotion_read: JSON serialization failed\n");
return -1;
}
size_t json_len = strlen(json);
if (offset >= (off_t)json_len) {
emotion_json_free(json);
return 0;
}
size_t available = json_len - offset;
size_t to_copy = (count < available) ? count : available;
memcpy(buf, json + offset, to_copy);
emotion_json_free(json);
return to_copy;
}
int avatar_emotion_write(FileNode* node, const char* buf, size_t count, off_t offset) {
if (!node || !buf) return -1;
if (offset != 0) {
fprintf(stderr, "avatar_emotion_write: offset must be 0 for JSON\n");
return -1;
}
if (!node->parent || !node->parent->name) {
fprintf(stderr, "avatar_emotion_write: no parent directory\n");
return -1;
}
const char* avatar_name = node->parent->name;
DeepTree9PServer* server = (DeepTree9PServer*)node->context;
if (!server || !server->bridge) {
fprintf(stderr, "avatar_emotion_write: no server context\n");
return -1;
}
char* json_buf = malloc(count + 1);
if (!json_buf) {
fprintf(stderr, "avatar_emotion_write: malloc failed\n");
return -1;
}
memcpy(json_buf, buf, count);
json_buf[count] = '\0';
Emotion emotion;
if (emotion_from_json(json_buf, &emotion) != 0) {
fprintf(stderr, "avatar_emotion_write: JSON parse failed\n");
free(json_buf);
return -1;
}
free(json_buf);
int result = vortex_bridge_set_emotion(server->bridge, avatar_name,
emotion.joy, emotion.sadness, emotion.anger,
emotion.fear, emotion.surprise, emotion.disgust);
if (result != 0) {
fprintf(stderr, "avatar_emotion_write: vortex_bridge_set_emotion failed\n");
return -1;
}
AvatarState* avatar = vortex_bridge_get_avatar(server->bridge, avatar_name);
if (avatar) {
printf("[9P] Avatar '%s' emotion updated: vorticity=%.2f\n",
avatar_name, avatar->vorticity);
}
return count;
}
int avatar_expression_read(FileNode* node, char* buf, size_t count, off_t offset) {
if (!node || !buf) return -1;
const char* avatar_name = node->parent->name;
DeepTree9PServer* server = (DeepTree9PServer*)node->context;
if (!server || !server->bridge) return -1;
const char* expression = vortex_bridge_get_expression(server->bridge, avatar_name);
if (!expression) return -1;
size_t expr_len = strlen(expression);
if (offset >= (off_t)expr_len) return 0;
size_t available = expr_len - offset;
size_t to_copy = (count < available) ? count : available;
memcpy(buf, expression + offset, to_copy);
return to_copy;
}
int avatar_expression_write(FileNode* node, const char* buf, size_t count, off_t offset) {
if (!node || !buf || offset != 0) return -1;
const char* avatar_name = node->parent->name;
DeepTree9PServer* server = (DeepTree9PServer*)node->context;
if (!server || !server->bridge) return -1;
char* expression = malloc(count + 1);
if (!expression) return -1;
memcpy(expression, buf, count);
expression[count] = '\0';
if (count > 0 && expression[count - 1] == '\n') {
expression[count - 1] = '\0';
}
int result = vortex_bridge_set_expression(server->bridge, avatar_name, expression);
free(expression);
if (result != 0) return -1;
printf("[9P] Avatar '%s' expression set to '%s'\n", avatar_name, expression);
return count;
}
int avatar_vorticity_read(FileNode* node, char* buf, size_t count, off_t offset) {
if (!node || !buf) return -1;
const char* avatar_name = node->parent->name;
DeepTree9PServer* server = (DeepTree9PServer*)node->context;
if (!server || !server->bridge) return -1;
AvatarState* avatar = vortex_bridge_get_avatar(server->bridge, avatar_name);
if (!avatar) return -1;
char vorticity_str[32];
snprintf(vorticity_str, sizeof(vorticity_str), "%.6f\n", avatar->vorticity);
size_t str_len = strlen(vorticity_str);
if (offset >= (off_t)str_len) return 0;
size_t available = str_len - offset;
size_t to_copy = (count < available) ? count : available;
memcpy(buf, vorticity_str + offset, to_copy);
return to_copy;
}
int avatar_matula_read(FileNode* node, char* buf, size_t count, off_t offset) {
if (!node || !buf) return -1;
const char* avatar_name = node->parent->name;
DeepTree9PServer* server = (DeepTree9PServer*)node->context;
if (!server || !server->bridge) return -1;
AvatarState* avatar = vortex_bridge_get_avatar(server->bridge, avatar_name);
if (!avatar) return -1;
char matula_str[32];
snprintf(matula_str, sizeof(matula_str), "%lu\n", avatar->matula);
size_t str_len = strlen(matula_str);
if (offset >= (off_t)str_len) return 0;
size_t available = str_len - offset;
size_t to_copy = (count < available) ? count : available;
memcpy(buf, matula_str + offset, to_copy);
return to_copy;
}
int egregore_collective_emotion_read(FileNode* node, char* buf, size_t count, off_t offset) {
if (!node || !buf) return -1;
DeepTree9PServer* server = (DeepTree9PServer*)node->context;
if (!server || !server->bridge) {
fprintf(stderr, "egregore_collective_emotion_read: no server context\n");
return -1;
}
VortexBridge* bridge = server->bridge;
CollectiveEmotion collective;
memset(&collective, 0, sizeof(CollectiveEmotion));
vortex_bridge_get_collective_emotion(bridge,
&collective.emotion.joy,
&collective.emotion.sadness,
&collective.emotion.anger,
&collective.emotion.fear,
&collective.emotion.surprise,
&collective.emotion.disgust);
collective.emotion.vorticity = vortex_bridge_emotion_to_vorticity(
collective.emotion.joy,
collective.emotion.sadness,
collective.emotion.anger,
collective.emotion.fear,
collective.emotion.surprise,
collective.emotion.disgust);
emotion_get_timestamp(collective.emotion.timestamp,
sizeof(collective.emotion.timestamp));
collective.n_avatars = bridge->n_avatars;
if (bridge->egregore) {
collective.coherence = egregore_get_coherence(bridge->egregore);
collective.circulation = bridge->egregore->circulation;
} else {
collective.coherence = 0.0;
collective.circulation = 0.0;
}
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
char* json = collective_emotion_to_json(&collective);
if (collective.avatars) {
free(collective.avatars);
}
if (!json) {
fprintf(stderr, "egregore_collective_emotion_read: JSON serialization failed\n");
return -1;
}
size_t json_len = strlen(json);
if (offset >= (off_t)json_len) {
emotion_json_free(json);
return 0;
}
size_t available = json_len - offset;
size_t to_copy = (count < available) ? count : available;
memcpy(buf, json + offset, to_copy);
emotion_json_free(json);
return to_copy;
}
int egregore_coherence_read(FileNode* node, char* buf, size_t count, off_t offset) {
if (!node || !buf) return -1;
DeepTree9PServer* server = (DeepTree9PServer*)node->context;
if (!server || !server->bridge || !server->bridge->egregore) return -1;
double coherence = egregore_get_coherence(server->bridge->egregore);
char coherence_str[32];
snprintf(coherence_str, sizeof(coherence_str), "%.6f\n", coherence);
size_t str_len = strlen(coherence_str);
if (offset >= (off_t)str_len) return 0;
size_t available = str_len - offset;
size_t to_copy = (count < available) ? count : available;
memcpy(buf, coherence_str + offset, to_copy);
return to_copy;
}
int egregore_circulation_read(FileNode* node, char* buf, size_t count, off_t offset) {
if (!node || !buf) return -1;
DeepTree9PServer* server = (DeepTree9PServer*)node->context;
if (!server || !server->bridge || !server->bridge->egregore) return -1;
double circulation = server->bridge->egregore->circulation;
char circulation_str[32];
snprintf(circulation_str, sizeof(circulation_str), "%.6f\n", circulation);
size_t str_len = strlen(circulation_str);
if (offset >= (off_t)str_len) return 0;
size_t available = str_len - offset;
size_t to_copy = (count < available) ? count : available;
memcpy(buf, circulation_str + offset, to_copy);
return to_copy;
}
int egregore_synchronize_write(FileNode* node, const char* buf, size_t count, off_t offset) {
if (!node || !buf || offset != 0) return -1;
DeepTree9PServer* server = (DeepTree9PServer*)node->context;
if (!server || !server->bridge) return -1;
double coherence = vortex_bridge_synchronize(server->bridge);
printf("[9P] Egregore synchronized: coherence=%.2f\n", coherence);
return count;
}
int ctl_write(FileNode* node, const char* buf, size_t count, off_t offset) {
if (!node || !buf || offset != 0) return -1;
DeepTree9PServer* server = (DeepTree9PServer*)node->context;
if (!server) return -1;
char* cmd = malloc(count + 1);
if (!cmd) return -1;
memcpy(cmd, buf, count);
cmd[count] = '\0';
if (count > 0 && cmd[count - 1] == '\n') {
cmd[count - 1] = '\0';
}
printf("[9P] Control command: %s\n", cmd);
free(cmd);
return count;
}
FileNode* file_node_create(const char* name, FileType type) {
if (!name) return NULL;
FileNode* node = calloc(1, sizeof(FileNode));
if (!node) return NULL;
strncpy(node->name, name, sizeof(node->name) - 1);
node->name[sizeof(node->name) - 1] = '\0';
node->type = type;
node->qid = 0;
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
void file_node_free(FileNode* node) {
if (!node) return;
if (node->children) {
for (int i = 0; i < node->n_children; i++) {
file_node_free(node->children[i]);
}
free(node->children);
}
if (node->content) {
free(node->content);
}
free(node);
}
int file_node_add_child(FileNode* parent, FileNode* child) {
if (!parent || !child) return -1;
if (parent->type != FILE_TYPE_DIR) return -1;
FileNode** new_children = realloc(parent->children,
sizeof(FileNode*) * (parent->n_children + 1));
if (!new_children) return -1;
parent->children = new_children;
parent->children[parent->n_children] = child;
parent->n_children++;
child->parent = parent;
return 0;
}
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
int file_node_set_content(FileNode* node, const char* content, size_t size) {
if (!node || !content) return -1;
if (node->content) {
free(node->content);
}
node->content = malloc(size);
if (!node->content) return -1;
memcpy(node->content, content, size);
node->size = size;
return 0;
}
FileNode* build_avatar_tree(DeepTree9PServer* server, const char* avatar_name) {
FileNode* avatar_dir = file_node_create(avatar_name, FILE_TYPE_DIR);
if (!avatar_dir) return NULL;
avatar_dir->context = server;
FileNode* emotion = file_node_create("emotion", FILE_TYPE_FILE);
emotion->context = server;
emotion->on_read = avatar_emotion_read;
emotion->on_write = avatar_emotion_write;
file_node_add_child(avatar_dir, emotion);
FileNode* expression = file_node_create("expression", FILE_TYPE_FILE);
expression->context = server;
expression->on_read = avatar_expression_read;
expression->on_write = avatar_expression_write;
file_node_add_child(avatar_dir, expression);
FileNode* vorticity = file_node_create("vorticity", FILE_TYPE_FILE);
vorticity->context = server;
vorticity->on_read = avatar_vorticity_read;
file_node_add_child(avatar_dir, vorticity);
FileNode* matula = file_node_create("matula", FILE_TYPE_FILE);
matula->context = server;
matula->on_read = avatar_matula_read;
file_node_add_child(avatar_dir, matula);
return avatar_dir;
}
int deep_tree_9p_build_tree(DeepTree9PServer* server) {
if (!server || !server->bridge) return -1;
server->root = file_node_create("/", FILE_TYPE_DIR);
if (!server->root) return -1;
server->root->context = server;
FileNode* avatars_dir = file_node_create("avatars", FILE_TYPE_DIR);
avatars_dir->context = server;
file_node_add_child(server->root, avatars_dir);
for (int i = 0; i < server->bridge->n_avatars; i++) {
AvatarState* avatar = server->bridge->avatars[i];
FileNode* avatar_tree = build_avatar_tree(server, avatar->name);
if (avatar_tree) {
file_node_add_child(avatars_dir, avatar_tree);
}
}
FileNode* egregore_dir = file_node_create("egregore", FILE_TYPE_DIR);
egregore_dir->context = server;
file_node_add_child(server->root, egregore_dir);
FileNode* collective_emotion = file_node_create("collective_emotion", FILE_TYPE_FILE);
collective_emotion->context = server;
collective_emotion->on_read = egregore_collective_emotion_read;
file_node_add_child(egregore_dir, collective_emotion);
FileNode* coherence = file_node_create("coherence", FILE_TYPE_FILE);
coherence->context = server;
coherence->on_read = egregore_coherence_read;
file_node_add_child(egregore_dir, coherence);
FileNode* circulation = file_node_create("circulation", FILE_TYPE_FILE);
circulation->context = server;
circulation->on_read = egregore_circulation_read;
file_node_add_child(egregore_dir, circulation);
FileNode* ctl = file_node_create("ctl", FILE_TYPE_CTL);
ctl->context = server;
ctl->on_write = ctl_write;
file_node_add_child(server->root, ctl);
printf("[9P] File tree built: %d avatars\n", server->bridge->n_avatars);
return 0;
}
void deep_tree_9p_refresh(DeepTree9PServer* server) {
if (!server) return;
if (server->root) {
file_node_free(server->root);
server->root = NULL;
}
deep_tree_9p_build_tree(server);
}
DeepTree9PServer* deep_tree_9p_create(VortexBridge* bridge, const char* mount_point, uint16_t port) {
if (!bridge || !mount_point) return NULL;
DeepTree9PServer* server = calloc(1, sizeof(DeepTree9PServer));
if (!server) return NULL;
server->bridge = bridge;
server->root = NULL;
strncpy(server->mount_point, mount_point, sizeof(server->mount_point) - 1);
server->mount_point[sizeof(server->mount_point) - 1] = '\0';
server->port = (port == 0) ? 5640 : port;
server->running = false;
return server;
}
void deep_tree_9p_free(DeepTree9PServer* server) {
if (!server) return;
if (server->root) {
file_node_free(server->root);
}
free(server);
}
int deep_tree_9p_start(DeepTree9PServer* server) {
if (!server) return -1;
if (deep_tree_9p_build_tree(server) != 0) {
return -1;
}
printf("[9P] Server started on port %d\n", server->port);
printf("[9P] Mount point: %s\n", server->mount_point);
server->running = true;
return 0;
}
void deep_tree_9p_stop(DeepTree9PServer* server) {
if (!server) return;
server->running = false;
printf("[9P] Server stopped\n");
}
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