#ifndef _VORTEX_BRIDGE_H_
#define _VORTEX_BRIDGE_H_
#include <stdint.h>
#include <stdbool.h>
#include "../../../inferno-kernel/vortex/matula.h"
#include "../../../inferno-kernel/vortex/vorticity.h"
#include "../../../inferno-kernel/morphule/morphule.h"
#include "../../../inferno-kernel/egregore/egregore.h"
#define VORTEX_BRIDGE_VERSION_MAJOR 1
#define VORTEX_BRIDGE_VERSION_MINOR 0
#define VORTEX_BRIDGE_VERSION_PATCH 0
#define VORTEX_BRIDGE_JSON_MAX 65536
typedef struct {
char name[64];
double joy;
double sadness;
double anger;
double fear;
double surprise;
double disgust;
char expression[32];
double chaos_coefficient;
double resonance_threshold;
int tree_depth;
uint64_t matula;
Morphule* morphule;
double vorticity;
} AvatarState;
typedef struct {
char content[256];
uint64_t matula;
double activation;
double chaos_value;
int depth;
} ThoughtNode;
typedef struct VortexBridge {
VortexConfig* vortex;
Egregore* egregore;
int n_avatars;
AvatarState** avatars;
bool enable_chaos;
bool enable_resonance;
bool enable_coordination;
void (*on_thought)(struct VortexBridge* bridge, ThoughtNode* thought);
void (*on_expression_change)(struct VortexBridge* bridge, AvatarState* avatar);
void (*on_emotion_change)(struct VortexBridge* bridge, AvatarState* avatar);
} VortexBridge;
VortexBridge* vortex_bridge_create(void);
void vortex_bridge_free(VortexBridge* bridge);
int vortex_bridge_init(VortexBridge* bridge);
AvatarState* vortex_bridge_register_avatar(VortexBridge* bridge, const char* name);
int vortex_bridge_unregister_avatar(VortexBridge* bridge, const char* name);
AvatarState* vortex_bridge_get_avatar(VortexBridge* bridge, const char* name);
int vortex_bridge_set_emotion(VortexBridge* bridge, const char* avatar,
double joy, double sadness, double anger,
double fear, double surprise, double disgust);
int vortex_bridge_get_emotion(VortexBridge* bridge, const char* avatar,
double* joy, double* sadness, double* anger,
double* fear, double* surprise, double* disgust);
int vortex_bridge_set_expression(VortexBridge* bridge, const char* avatar, const char* expression);
const char* vortex_bridge_get_expression(VortexBridge* bridge, const char* avatar);
ThoughtNode* vortex_bridge_process_thought(VortexBridge* bridge, const char* avatar,
const char* content, double chaos_value, int depth);
void vortex_bridge_free_thought(ThoughtNode* thought);
double vortex_bridge_link_thoughts(VortexBridge* bridge, ThoughtNode* thought1, ThoughtNode* thought2);
double vortex_bridge_synchronize(VortexBridge* bridge);
int vortex_bridge_get_collective_emotion(VortexBridge* bridge,
double* joy, double* sadness, double* anger,
double* fear, double* surprise, double* disgust);
char* vortex_bridge_avatar_to_json(AvatarState* avatar);
AvatarState* vortex_bridge_avatar_from_json(const char* json);
char* vortex_bridge_thought_to_json(ThoughtNode* thought);
ThoughtNode* vortex_bridge_thought_from_json(const char* json);
void* vortex_bridge_napi_init(void* env, void* exports);
double vortex_bridge_emotion_to_vorticity(double joy, double sadness, double anger,
double fear, double surprise, double disgust);
void vortex_bridge_vorticity_to_emotion(double vorticity,
double* joy, double* sadness, double* anger,
double* fear, double* surprise, double* disgust);
double vortex_bridge_thought_similarity(ThoughtNode* t1, ThoughtNode* t2);
void vortex_bridge_print(VortexBridge* bridge);
#endif