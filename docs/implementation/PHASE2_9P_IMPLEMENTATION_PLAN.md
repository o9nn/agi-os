# Phase 2: Complete 9P Server Implementation Plan

## Overview

This document details the implementation plan for Phase 2 of the Deep Tree Echo 9P server, focusing on the emotion read/write handlers for:
- `/mnt/deep-tree-echo/avatars/<avatar-name>/emotion`
- `/mnt/deep-tree-echo/egregore/collective_emotion`

**Date**: December 13, 2025  
**Status**: Planning  
**Target Completion**: Phase 2  

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│  9P Client (shell, programs)                                │
│  $ cat /mnt/deep-tree-echo/avatars/alice/emotion            │
│  $ echo '{"joy":0.8,...}' > .../alice/emotion               │
└────────────────────┬────────────────────────────────────────┘
                     │ 9P Protocol (Tversion, Topen, Tread, Twrite)
┌────────────────────▼────────────────────────────────────────┐
│  9P Server (deep_tree_9p.c)                                 │
│  ─────────────────────────────────────────────────────────  │
│  - Protocol handling (9P2000 messages)                      │
│  - File tree navigation                                     │
│  - Read/write handler dispatch                              │
└────────────────────┬────────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────────┐
│  Emotion Handlers                                           │
│  ─────────────────────────────────────────────────────────  │
│  - avatar_emotion_read()  → JSON serialization              │
│  - avatar_emotion_write() → JSON parsing + vortex update    │
│  - collective_emotion_read() → aggregate + JSON             │
└────────────────────┬────────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────────┐
│  VORTEX Bridge                                              │
│  ─────────────────────────────────────────────────────────  │
│  - vortex_bridge_get_emotion()                              │
│  - vortex_bridge_set_emotion()                              │
│  - vortex_bridge_get_collective_emotion()                   │
└─────────────────────────────────────────────────────────────┘
```

---

## Component 1: JSON Emotion Format

### Emotion JSON Schema

```json
{
  "joy": 0.0-1.0,
  "sadness": 0.0-1.0,
  "anger": 0.0-1.0,
  "fear": 0.0-1.0,
  "surprise": 0.0-1.0,
  "disgust": 0.0-1.0,
  "vorticity": 0.0-1.0,        // computed (read-only)
  "timestamp": "ISO8601",       // optional
  "avatar": "name"              // optional (for collective)
}
```

### Example Emotion JSON

**Single Avatar**:
```json
{
  "joy": 0.85,
  "sadness": 0.15,
  "anger": 0.20,
  "fear": 0.10,
  "surprise": 0.70,
  "disgust": 0.05,
  "vorticity": 0.72,
  "timestamp": "2025-12-13T17:30:00Z"
}
```

**Collective Emotion**:
```json
{
  "joy": 0.65,
  "sadness": 0.25,
  "anger": 0.30,
  "fear": 0.20,
  "surprise": 0.45,
  "disgust": 0.15,
  "vorticity": 0.58,
  "n_avatars": 3,
  "coherence": 0.82,
  "circulation": 1.45,
  "avatars": [
    {"name": "Alice", "vorticity": 0.72},
    {"name": "Bob", "vorticity": 0.48},
    {"name": "Carol", "vorticity": 0.55}
  ]
}
```

---

## Component 2: JSON Parsing and Serialization

### Dependencies

We'll use a lightweight JSON library. Options:
1. **cJSON** (recommended) - Simple, single-file, MIT license
2. **jsmn** - Minimalist tokenizer
3. **Jansson** - Full-featured, requires linking

**Choice**: **cJSON** for simplicity and ease of integration.

### Installation

```bash
# Download cJSON
cd /home/ubuntu/agi-os/core/avatar/deep-tree-echo/bridge/
wget https://raw.githubusercontent.com/DaveGamble/cJSON/master/cJSON.c
wget https://raw.githubusercontent.com/DaveGamble/cJSON/master/cJSON.h

# Or use as submodule
git submodule add https://github.com/DaveGamble/cJSON.git external/cJSON
```

### API Design

```c
/* emotion_json.h */

#ifndef _EMOTION_JSON_H_
#define _EMOTION_JSON_H_

#include <stdint.h>
#include <stdbool.h>

/* Emotion structure */
typedef struct {
    double joy;
    double sadness;
    double anger;
    double fear;
    double surprise;
    double disgust;
    double vorticity;        /* computed */
    char timestamp[32];      /* ISO8601 */
} Emotion;

/* Collective emotion structure */
typedef struct {
    Emotion emotion;         /* average emotion */
    int n_avatars;
    double coherence;
    double circulation;
    
    /* Individual avatars */
    struct {
        char name[64];
        double vorticity;
    } *avatars;
    int n_avatar_entries;
} CollectiveEmotion;

/**
 * Parse emotion from JSON string
 * 
 * json: JSON string
 * emotion: Output emotion structure
 * 
 * Returns: 0 on success, -1 on error
 */
int emotion_from_json(const char* json, Emotion* emotion);

/**
 * Serialize emotion to JSON string
 * 
 * emotion: Input emotion structure
 * 
 * Returns: JSON string (caller must free), or NULL on error
 */
char* emotion_to_json(const Emotion* emotion);

/**
 * Serialize collective emotion to JSON string
 * 
 * collective: Input collective emotion structure
 * 
 * Returns: JSON string (caller must free), or NULL on error
 */
char* collective_emotion_to_json(const CollectiveEmotion* collective);

/**
 * Free JSON string
 */
void emotion_json_free(char* json);

/**
 * Validate emotion values (all in [0.0, 1.0])
 * 
 * Returns: true if valid, false otherwise
 */
bool emotion_validate(const Emotion* emotion);

/**
 * Get current ISO8601 timestamp
 */
void emotion_get_timestamp(char* buf, size_t size);

#endif /* _EMOTION_JSON_H_ */
```

### Implementation

```c
/* emotion_json.c */

#include "emotion_json.h"
#include "cJSON.h"
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

/* Parse emotion from JSON */
int emotion_from_json(const char* json, Emotion* emotion) {
    if (!json || !emotion) return -1;
    
    cJSON* root = cJSON_Parse(json);
    if (!root) return -1;
    
    /* Parse required fields */
    cJSON* joy = cJSON_GetObjectItem(root, "joy");
    cJSON* sadness = cJSON_GetObjectItem(root, "sadness");
    cJSON* anger = cJSON_GetObjectItem(root, "anger");
    cJSON* fear = cJSON_GetObjectItem(root, "fear");
    cJSON* surprise = cJSON_GetObjectItem(root, "surprise");
    cJSON* disgust = cJSON_GetObjectItem(root, "disgust");
    
    if (!joy || !sadness || !anger || !fear || !surprise || !disgust) {
        cJSON_Delete(root);
        return -1;
    }
    
    emotion->joy = joy->valuedouble;
    emotion->sadness = sadness->valuedouble;
    emotion->anger = anger->valuedouble;
    emotion->fear = fear->valuedouble;
    emotion->surprise = surprise->valuedouble;
    emotion->disgust = disgust->valuedouble;
    
    /* Optional fields */
    cJSON* timestamp = cJSON_GetObjectItem(root, "timestamp");
    if (timestamp && cJSON_IsString(timestamp)) {
        strncpy(emotion->timestamp, timestamp->valuestring, 
                sizeof(emotion->timestamp) - 1);
    } else {
        emotion_get_timestamp(emotion->timestamp, sizeof(emotion->timestamp));
    }
    
    /* Vorticity will be computed by caller */
    emotion->vorticity = 0.0;
    
    cJSON_Delete(root);
    
    /* Validate */
    if (!emotion_validate(emotion)) {
        return -1;
    }
    
    return 0;
}

/* Serialize emotion to JSON */
char* emotion_to_json(const Emotion* emotion) {
    if (!emotion) return NULL;
    
    cJSON* root = cJSON_CreateObject();
    if (!root) return NULL;
    
    cJSON_AddNumberToObject(root, "joy", emotion->joy);
    cJSON_AddNumberToObject(root, "sadness", emotion->sadness);
    cJSON_AddNumberToObject(root, "anger", emotion->anger);
    cJSON_AddNumberToObject(root, "fear", emotion->fear);
    cJSON_AddNumberToObject(root, "surprise", emotion->surprise);
    cJSON_AddNumberToObject(root, "disgust", emotion->disgust);
    cJSON_AddNumberToObject(root, "vorticity", emotion->vorticity);
    cJSON_AddStringToObject(root, "timestamp", emotion->timestamp);
    
    char* json = cJSON_PrintUnformatted(root);
    cJSON_Delete(root);
    
    return json;
}

/* Serialize collective emotion to JSON */
char* collective_emotion_to_json(const CollectiveEmotion* collective) {
    if (!collective) return NULL;
    
    cJSON* root = cJSON_CreateObject();
    if (!root) return NULL;
    
    /* Average emotion */
    cJSON_AddNumberToObject(root, "joy", collective->emotion.joy);
    cJSON_AddNumberToObject(root, "sadness", collective->emotion.sadness);
    cJSON_AddNumberToObject(root, "anger", collective->emotion.anger);
    cJSON_AddNumberToObject(root, "fear", collective->emotion.fear);
    cJSON_AddNumberToObject(root, "surprise", collective->emotion.surprise);
    cJSON_AddNumberToObject(root, "disgust", collective->emotion.disgust);
    cJSON_AddNumberToObject(root, "vorticity", collective->emotion.vorticity);
    
    /* Collective metrics */
    cJSON_AddNumberToObject(root, "n_avatars", collective->n_avatars);
    cJSON_AddNumberToObject(root, "coherence", collective->coherence);
    cJSON_AddNumberToObject(root, "circulation", collective->circulation);
    
    /* Individual avatars */
    cJSON* avatars = cJSON_CreateArray();
    for (int i = 0; i < collective->n_avatar_entries; i++) {
        cJSON* avatar = cJSON_CreateObject();
        cJSON_AddStringToObject(avatar, "name", collective->avatars[i].name);
        cJSON_AddNumberToObject(avatar, "vorticity", collective->avatars[i].vorticity);
        cJSON_AddItemToArray(avatars, avatar);
    }
    cJSON_AddItemToObject(root, "avatars", avatars);
    
    char* json = cJSON_PrintUnformatted(root);
    cJSON_Delete(root);
    
    return json;
}

/* Free JSON string */
void emotion_json_free(char* json) {
    if (json) {
        cJSON_free(json);
    }
}

/* Validate emotion */
bool emotion_validate(const Emotion* emotion) {
    if (!emotion) return false;
    
    /* Check all values in [0.0, 1.0] */
    if (emotion->joy < 0.0 || emotion->joy > 1.0) return false;
    if (emotion->sadness < 0.0 || emotion->sadness > 1.0) return false;
    if (emotion->anger < 0.0 || emotion->anger > 1.0) return false;
    if (emotion->fear < 0.0 || emotion->fear > 1.0) return false;
    if (emotion->surprise < 0.0 || emotion->surprise > 1.0) return false;
    if (emotion->disgust < 0.0 || emotion->disgust > 1.0) return false;
    
    /* Check for NaN */
    if (isnan(emotion->joy) || isnan(emotion->sadness) || 
        isnan(emotion->anger) || isnan(emotion->fear) ||
        isnan(emotion->surprise) || isnan(emotion->disgust)) {
        return false;
    }
    
    return true;
}

/* Get current timestamp */
void emotion_get_timestamp(char* buf, size_t size) {
    time_t now = time(NULL);
    struct tm* tm = gmtime(&now);
    strftime(buf, size, "%Y-%m-%dT%H:%M:%SZ", tm);
}
```

---

## Component 3: Avatar Emotion Read Handler

### Handler Design

```c
/**
 * Avatar emotion read handler
 * 
 * Reads the current emotional state of an avatar and returns it as JSON.
 * 
 * Path: /mnt/deep-tree-echo/avatars/<avatar-name>/emotion
 * 
 * node: File node (contains avatar name in parent)
 * buf: Output buffer
 * count: Buffer size
 * offset: Read offset
 * 
 * Returns: Number of bytes read, or -1 on error
 */
int avatar_emotion_read(FileNode* node, char* buf, size_t count, off_t offset) {
    if (!node || !buf) return -1;
    
    /* Get avatar name from parent directory */
    if (!node->parent || !node->parent->name) return -1;
    const char* avatar_name = node->parent->name;
    
    /* Get bridge from node context */
    DeepTree9PServer* server = (DeepTree9PServer*)node->context;
    if (!server || !server->bridge) return -1;
    
    /* Get avatar state */
    AvatarState* avatar = vortex_bridge_get_avatar(server->bridge, avatar_name);
    if (!avatar) return -1;
    
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
    if (!json) return -1;
    
    size_t json_len = strlen(json);
    
    /* Handle offset and count */
    if (offset >= json_len) {
        emotion_json_free(json);
        return 0;  /* EOF */
    }
    
    size_t available = json_len - offset;
    size_t to_copy = (count < available) ? count : available;
    
    memcpy(buf, json + offset, to_copy);
    
    emotion_json_free(json);
    
    return to_copy;
}
```

### Usage Example

```bash
# Read Alice's emotion
$ cat /mnt/deep-tree-echo/avatars/alice/emotion
{"joy":0.85,"sadness":0.15,"anger":0.20,"fear":0.10,"surprise":0.70,"disgust":0.05,"vorticity":0.72,"timestamp":"2025-12-13T17:30:00Z"}

# Pretty print with jq
$ cat /mnt/deep-tree-echo/avatars/alice/emotion | jq .
{
  "joy": 0.85,
  "sadness": 0.15,
  "anger": 0.20,
  "fear": 0.10,
  "surprise": 0.70,
  "disgust": 0.05,
  "vorticity": 0.72,
  "timestamp": "2025-12-13T17:30:00Z"
}
```

---

## Component 4: Avatar Emotion Write Handler

### Handler Design

```c
/**
 * Avatar emotion write handler
 * 
 * Parses JSON emotion data and updates the avatar's emotional state.
 * Automatically computes vorticity and updates the morphule.
 * 
 * Path: /mnt/deep-tree-echo/avatars/<avatar-name>/emotion
 * 
 * node: File node (contains avatar name in parent)
 * buf: Input buffer (JSON)
 * count: Buffer size
 * offset: Write offset (must be 0 for JSON)
 * 
 * Returns: Number of bytes written, or -1 on error
 */
int avatar_emotion_write(FileNode* node, const char* buf, size_t count, off_t offset) {
    if (!node || !buf) return -1;
    
    /* JSON writes must start at offset 0 */
    if (offset != 0) return -1;
    
    /* Get avatar name from parent directory */
    if (!node->parent || !node->parent->name) return -1;
    const char* avatar_name = node->parent->name;
    
    /* Get bridge from node context */
    DeepTree9PServer* server = (DeepTree9PServer*)node->context;
    if (!server || !server->bridge) return -1;
    
    /* Null-terminate buffer for JSON parsing */
    char* json_buf = malloc(count + 1);
    if (!json_buf) return -1;
    memcpy(json_buf, buf, count);
    json_buf[count] = '\0';
    
    /* Parse JSON */
    Emotion emotion;
    if (emotion_from_json(json_buf, &emotion) != 0) {
        free(json_buf);
        return -1;
    }
    free(json_buf);
    
    /* Update avatar emotion via bridge */
    int result = vortex_bridge_set_emotion(server->bridge, avatar_name,
        emotion.joy, emotion.sadness, emotion.anger,
        emotion.fear, emotion.surprise, emotion.disgust);
    
    if (result != 0) return -1;
    
    /* Log the update */
    printf("[9P] Avatar '%s' emotion updated: vorticity=%.2f\n",
           avatar_name, 
           vortex_bridge_get_avatar(server->bridge, avatar_name)->vorticity);
    
    return count;
}
```

### Usage Example

```bash
# Set Alice's emotion to happy
$ echo '{"joy":0.9,"sadness":0.1,"anger":0.1,"fear":0.05,"surprise":0.8,"disgust":0.05}' > /mnt/deep-tree-echo/avatars/alice/emotion

# Verify
$ cat /mnt/deep-tree-echo/avatars/alice/emotion | jq .joy
0.9

# Set Bob's emotion to sad
$ echo '{"joy":0.2,"sadness":0.9,"anger":0.3,"fear":0.7,"surprise":0.1,"disgust":0.4}' > /mnt/deep-tree-echo/avatars/bob/emotion

# Check vorticity
$ cat /mnt/deep-tree-echo/avatars/bob/vorticity
0.32
```

---

## Component 5: Collective Emotion Read Handler

### Handler Design

```c
/**
 * Collective emotion read handler
 * 
 * Computes the collective emotional state across all avatars in the egregore
 * and returns it as JSON with additional swarm metrics.
 * 
 * Path: /mnt/deep-tree-echo/egregore/collective_emotion
 * 
 * node: File node
 * buf: Output buffer
 * count: Buffer size
 * offset: Read offset
 * 
 * Returns: Number of bytes read, or -1 on error
 */
int egregore_collective_emotion_read(FileNode* node, char* buf, size_t count, off_t offset) {
    if (!node || !buf) return -1;
    
    /* Get bridge from node context */
    DeepTree9PServer* server = (DeepTree9PServer*)node->context;
    if (!server || !server->bridge) return -1;
    
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
    collective.avatars = malloc(sizeof(*collective.avatars) * bridge->n_avatars);
    if (!collective.avatars) return -1;
    
    for (int i = 0; i < bridge->n_avatars; i++) {
        AvatarState* avatar = bridge->avatars[i];
        strncpy(collective.avatars[i].name, avatar->name, 
                sizeof(collective.avatars[i].name) - 1);
        collective.avatars[i].vorticity = avatar->vorticity;
    }
    
    /* Serialize to JSON */
    char* json = collective_emotion_to_json(&collective);
    free(collective.avatars);
    
    if (!json) return -1;
    
    size_t json_len = strlen(json);
    
    /* Handle offset and count */
    if (offset >= json_len) {
        emotion_json_free(json);
        return 0;  /* EOF */
    }
    
    size_t available = json_len - offset;
    size_t to_copy = (count < available) ? count : available;
    
    memcpy(buf, json + offset, to_copy);
    
    emotion_json_free(json);
    
    return to_copy;
}
```

### Usage Example

```bash
# Read collective emotion
$ cat /mnt/deep-tree-echo/egregore/collective_emotion | jq .
{
  "joy": 0.65,
  "sadness": 0.25,
  "anger": 0.30,
  "fear": 0.20,
  "surprise": 0.45,
  "disgust": 0.15,
  "vorticity": 0.58,
  "n_avatars": 3,
  "coherence": 0.82,
  "circulation": 1.45,
  "avatars": [
    {"name": "Alice", "vorticity": 0.72},
    {"name": "Bob", "vorticity": 0.48},
    {"name": "Carol", "vorticity": 0.55}
  ]
}

# Monitor collective emotion in real-time
$ watch -n 1 'cat /mnt/deep-tree-echo/egregore/collective_emotion | jq .coherence'

# Extract specific metrics
$ cat /mnt/deep-tree-echo/egregore/collective_emotion | jq '.avatars[] | select(.name == "Alice") | .vorticity'
0.72
```

---

## Component 6: 9P File Server Core

### File Node Context

Each file node needs access to the server and bridge:

```c
/* Update FileNode structure */
typedef struct FileNode {
    char name[256];
    FileType type;
    uint32_t qid;
    
    char* content;
    size_t size;
    
    struct FileNode** children;
    int n_children;
    struct FileNode* parent;
    
    /* NEW: Context pointer */
    void* context;  /* Points to DeepTree9PServer */
    
    int (*on_read)(struct FileNode* node, char* buf, size_t count, off_t offset);
    int (*on_write)(struct FileNode* node, const char* buf, size_t count, off_t offset);
    
} FileNode;
```

### File Tree Construction

```c
/**
 * Build file tree for avatar
 * 
 * Creates the complete file tree for a single avatar:
 * 
 * <avatar-name>/
 * ├── emotion          # Read/write emotional state
 * ├── expression       # Read/write expression name
 * ├── vorticity        # Read current vorticity
 * ├── matula           # Read Matula number
 * └── morphule/
 *     ├── constraints
 *     ├── essence
 *     └── techniques/
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
    
    return 0;
}
```

---

## Component 7: Testing Strategy

### Unit Tests

```c
/* test_emotion_handlers.c */

#include "vortex_bridge.h"
#include "emotion_json.h"
#include "deep_tree_9p.h"
#include <assert.h>
#include <string.h>

void test_emotion_json_parse() {
    const char* json = "{\"joy\":0.8,\"sadness\":0.2,\"anger\":0.3,"
                       "\"fear\":0.1,\"surprise\":0.7,\"disgust\":0.05}";
    
    Emotion emotion;
    int result = emotion_from_json(json, &emotion);
    
    assert(result == 0);
    assert(emotion.joy == 0.8);
    assert(emotion.sadness == 0.2);
    assert(emotion.anger == 0.3);
    assert(emotion.fear == 0.1);
    assert(emotion.surprise == 0.7);
    assert(emotion.disgust == 0.05);
    
    printf("✓ test_emotion_json_parse passed\n");
}

void test_emotion_json_serialize() {
    Emotion emotion = {
        .joy = 0.85,
        .sadness = 0.15,
        .anger = 0.20,
        .fear = 0.10,
        .surprise = 0.70,
        .disgust = 0.05,
        .vorticity = 0.72
    };
    emotion_get_timestamp(emotion.timestamp, sizeof(emotion.timestamp));
    
    char* json = emotion_to_json(&emotion);
    assert(json != NULL);
    
    /* Verify JSON contains expected values */
    assert(strstr(json, "\"joy\":0.85") != NULL);
    assert(strstr(json, "\"vorticity\":0.72") != NULL);
    
    emotion_json_free(json);
    
    printf("✓ test_emotion_json_serialize passed\n");
}

void test_avatar_emotion_read_write() {
    /* Create bridge */
    VortexBridge* bridge = vortex_bridge_create();
    vortex_bridge_init(bridge);
    
    /* Register avatar */
    vortex_bridge_register_avatar(bridge, "TestAvatar");
    
    /* Create server */
    DeepTree9PServer* server = deep_tree_9p_create(bridge, "/mnt/test", 0);
    deep_tree_9p_build_tree(server);
    
    /* Find emotion file */
    FileNode* avatars = file_node_find_child(server->root, "avatars");
    assert(avatars != NULL);
    
    FileNode* avatar = file_node_find_child(avatars, "TestAvatar");
    assert(avatar != NULL);
    
    FileNode* emotion_file = file_node_find_child(avatar, "emotion");
    assert(emotion_file != NULL);
    
    /* Test write */
    const char* json_in = "{\"joy\":0.9,\"sadness\":0.1,\"anger\":0.1,"
                          "\"fear\":0.05,\"surprise\":0.8,\"disgust\":0.05}";
    int written = avatar_emotion_write(emotion_file, json_in, strlen(json_in), 0);
    assert(written == strlen(json_in));
    
    /* Test read */
    char buf[1024];
    int read_bytes = avatar_emotion_read(emotion_file, buf, sizeof(buf), 0);
    assert(read_bytes > 0);
    buf[read_bytes] = '\0';
    
    /* Verify */
    assert(strstr(buf, "\"joy\":0.9") != NULL);
    
    /* Cleanup */
    deep_tree_9p_free(server);
    vortex_bridge_free(bridge);
    
    printf("✓ test_avatar_emotion_read_write passed\n");
}

void test_collective_emotion_read() {
    /* Create bridge */
    VortexBridge* bridge = vortex_bridge_create();
    vortex_bridge_init(bridge);
    
    /* Register multiple avatars */
    vortex_bridge_register_avatar(bridge, "Alice");
    vortex_bridge_register_avatar(bridge, "Bob");
    vortex_bridge_register_avatar(bridge, "Carol");
    
    /* Set emotions */
    vortex_bridge_set_emotion(bridge, "Alice", 0.9, 0.1, 0.2, 0.1, 0.7, 0.1);
    vortex_bridge_set_emotion(bridge, "Bob", 0.3, 0.7, 0.5, 0.6, 0.2, 0.4);
    vortex_bridge_set_emotion(bridge, "Carol", 0.6, 0.4, 0.3, 0.3, 0.5, 0.2);
    
    /* Create server */
    DeepTree9PServer* server = deep_tree_9p_create(bridge, "/mnt/test", 0);
    deep_tree_9p_build_tree(server);
    
    /* Find collective_emotion file */
    FileNode* egregore = file_node_find_child(server->root, "egregore");
    assert(egregore != NULL);
    
    FileNode* collective = file_node_find_child(egregore, "collective_emotion");
    assert(collective != NULL);
    
    /* Test read */
    char buf[2048];
    int read_bytes = egregore_collective_emotion_read(collective, buf, sizeof(buf), 0);
    assert(read_bytes > 0);
    buf[read_bytes] = '\0';
    
    /* Verify */
    assert(strstr(buf, "\"n_avatars\":3") != NULL);
    assert(strstr(buf, "\"coherence\"") != NULL);
    assert(strstr(buf, "\"Alice\"") != NULL);
    assert(strstr(buf, "\"Bob\"") != NULL);
    assert(strstr(buf, "\"Carol\"") != NULL);
    
    /* Cleanup */
    deep_tree_9p_free(server);
    vortex_bridge_free(bridge);
    
    printf("✓ test_collective_emotion_read passed\n");
}

int main() {
    printf("Running emotion handler tests...\n\n");
    
    test_emotion_json_parse();
    test_emotion_json_serialize();
    test_avatar_emotion_read_write();
    test_collective_emotion_read();
    
    printf("\n✅ All tests passed!\n");
    
    return 0;
}
```

### Integration Tests

```bash
#!/bin/bash
# test_9p_emotion.sh

# Start 9P server
./deep_tree_9p_server &
SERVER_PID=$!
sleep 1

# Mount filesystem
mkdir -p /mnt/deep-tree-echo
mount -t 9p -o trans=tcp,port=5640 localhost /mnt/deep-tree-echo

# Test 1: Create avatar
echo "Alice" > /mnt/deep-tree-echo/avatars/create
[ -d /mnt/deep-tree-echo/avatars/alice ] || exit 1
echo "✓ Avatar creation works"

# Test 2: Write emotion
echo '{"joy":0.9,"sadness":0.1,"anger":0.1,"fear":0.05,"surprise":0.8,"disgust":0.05}' > /mnt/deep-tree-echo/avatars/alice/emotion
echo "✓ Emotion write works"

# Test 3: Read emotion
EMOTION=$(cat /mnt/deep-tree-echo/avatars/alice/emotion)
echo "$EMOTION" | jq . > /dev/null || exit 1
echo "✓ Emotion read works"

# Test 4: Verify vorticity
VORTICITY=$(cat /mnt/deep-tree-echo/avatars/alice/vorticity)
[ -n "$VORTICITY" ] || exit 1
echo "✓ Vorticity read works: $VORTICITY"

# Test 5: Read collective emotion
COLLECTIVE=$(cat /mnt/deep-tree-echo/egregore/collective_emotion)
echo "$COLLECTIVE" | jq . > /dev/null || exit 1
echo "✓ Collective emotion read works"

# Cleanup
umount /mnt/deep-tree-echo
kill $SERVER_PID

echo "✅ All integration tests passed!"
```

---

## Implementation Timeline

### Phase 2.1: JSON Support (Day 1)
- [ ] Download and integrate cJSON
- [ ] Implement emotion_json.h/c
- [ ] Write unit tests for JSON parsing
- [ ] Test with sample emotion data

### Phase 2.2: Avatar Emotion Handlers (Day 2)
- [ ] Implement avatar_emotion_read()
- [ ] Implement avatar_emotion_write()
- [ ] Add context pointer to FileNode
- [ ] Test with mock file nodes

### Phase 2.3: Collective Emotion Handler (Day 3)
- [ ] Implement egregore_collective_emotion_read()
- [ ] Test with multiple avatars
- [ ] Verify JSON output format

### Phase 2.4: File Tree Construction (Day 4)
- [ ] Implement build_avatar_tree()
- [ ] Implement deep_tree_9p_build_tree()
- [ ] Add dynamic avatar creation
- [ ] Test file tree navigation

### Phase 2.5: Testing (Day 5)
- [ ] Write unit tests
- [ ] Write integration tests
- [ ] Test with real 9P clients
- [ ] Performance profiling

### Phase 2.6: Documentation and Commit (Day 6)
- [ ] Update documentation
- [ ] Add usage examples
- [ ] Commit to repository
- [ ] Create release notes

---

## Performance Considerations

### Caching Strategy

```c
/* Cache JSON for read operations */
typedef struct {
    char* cached_json;
    time_t cache_time;
    uint64_t cache_version;
} EmotionCache;

/* Invalidate cache on write */
void invalidate_emotion_cache(FileNode* node) {
    EmotionCache* cache = (EmotionCache*)node->cache;
    if (cache && cache->cached_json) {
        free(cache->cached_json);
        cache->cached_json = NULL;
    }
}

/* Use cache in read */
int avatar_emotion_read_cached(FileNode* node, char* buf, size_t count, off_t offset) {
    EmotionCache* cache = (EmotionCache*)node->cache;
    
    /* Check cache */
    if (cache && cache->cached_json) {
        time_t now = time(NULL);
        if (now - cache->cache_time < 1) {  /* 1 second cache */
            /* Use cached JSON */
            size_t json_len = strlen(cache->cached_json);
            /* ... copy from cache ... */
            return to_copy;
        }
    }
    
    /* Cache miss, generate new JSON */
    /* ... */
}
```

### Memory Management

- **JSON buffers**: Allocate on-demand, free immediately after use
- **File tree**: Build once, refresh on avatar creation/deletion
- **Emotion cache**: Optional, 1-second TTL

### Concurrency

- **Read operations**: Thread-safe (read-only access to bridge)
- **Write operations**: Mutex on bridge state
- **File tree**: RW lock for navigation

---

## Error Handling

### Error Codes

```c
#define E9P_SUCCESS       0
#define E9P_INVALID_JSON  -1
#define E9P_INVALID_VALUE -2
#define E9P_AVATAR_NOT_FOUND -3
#define E9P_BRIDGE_ERROR  -4
#define E9P_MEMORY_ERROR  -5
```

### Error Responses

```bash
# Invalid JSON
$ echo 'invalid' > /mnt/deep-tree-echo/avatars/alice/emotion
bash: echo: write error: Invalid argument

# Out of range value
$ echo '{"joy":1.5,...}' > /mnt/deep-tree-echo/avatars/alice/emotion
bash: echo: write error: Invalid argument

# Avatar not found
$ cat /mnt/deep-tree-echo/avatars/nonexistent/emotion
cat: /mnt/deep-tree-echo/avatars/nonexistent/emotion: No such file or directory
```

---

## Security Considerations

### Input Validation

- **Emotion values**: Must be in [0.0, 1.0]
- **JSON size**: Limit to 4KB
- **Avatar names**: Alphanumeric + underscore only
- **Path traversal**: Prevent "../" in paths

### Access Control

- **Read**: Anyone can read emotion state
- **Write**: Require authentication (future)
- **Admin**: ctl file requires root (future)

---

## Next Steps After Phase 2

### Phase 3: Complete 9P Server
- Implement remaining handlers (expression, vorticity, matula)
- Add thought creation and management
- Implement ctl file operations

### Phase 4: Node.js N-API
- Create TypeScript bindings
- Integrate with Deep Tree Echo
- Add WebSocket bridge

### Phase 5: Production Hardening
- Performance optimization
- Security audit
- Load testing
- Documentation

---

## Summary

This implementation plan provides a complete roadmap for Phase 2 of the 9P server, focusing on emotion handlers. The key components are:

1. **JSON Support**: cJSON library for parsing and serialization
2. **Avatar Emotion**: Read/write handlers with vorticity computation
3. **Collective Emotion**: Aggregate emotion across all avatars
4. **File Tree**: Dynamic construction with context pointers
5. **Testing**: Comprehensive unit and integration tests

**Estimated Effort**: 6 days  
**Lines of Code**: ~1,500 (emotion_json.c + handlers + tests)  
**Dependencies**: cJSON (single file, MIT license)

**Status**: Ready for implementation ✅
