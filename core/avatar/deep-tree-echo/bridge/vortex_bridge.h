/**
 * VORTEX-MORPHULE-EGREGORE ↔ Deep Tree Echo Integration Bridge
 * 
 * This bridge connects the C-based VORTEX kernel primitives with the
 * TypeScript-based Deep Tree Echo cognitive avatar system.
 * 
 * Architecture:
 * 
 *   ┌─────────────────────────────────────────────────────────┐
 *   │  Deep Tree Echo (TypeScript)                            │
 *   │  - Chaotic attractors                                   │
 *   │  - Reservoir networks                                   │
 *   │  - Avatar rendering (Live2D + Three.js)                 │
 *   └────────────────────┬────────────────────────────────────┘
 *                        │
 *                        │ Node.js N-API / FFI
 *                        │
 *   ┌────────────────────▼────────────────────────────────────┐
 *   │  Integration Bridge (C)                                 │
 *   │  - Vortex state serialization                           │
 *   │  - Morphule expression mapping                          │
 *   │  - Egregore avatar coordination                         │
 *   └────────────────────┬────────────────────────────────────┘
 *                        │
 *   ┌────────────────────▼────────────────────────────────────┐
 *   │  VORTEX-MORPHULE-EGREGORE (C)                           │
 *   │  - Matula numbers                                       │
 *   │  - Vorticity primitives                                 │
 *   │  - Stigmergic coordination                              │
 *   └─────────────────────────────────────────────────────────┘
 */

#ifndef _VORTEX_BRIDGE_H_
#define _VORTEX_BRIDGE_H_

#include <stdint.h>
#include <stdbool.h>
#include "../../../inferno-kernel/vortex/matula.h"
#include "../../../inferno-kernel/vortex/vorticity.h"
#include "../../../inferno-kernel/morphule/morphule.h"
#include "../../../inferno-kernel/egregore/egregore.h"

/* Bridge version */
#define VORTEX_BRIDGE_VERSION_MAJOR 1
#define VORTEX_BRIDGE_VERSION_MINOR 0
#define VORTEX_BRIDGE_VERSION_PATCH 0

/* Maximum JSON buffer size */
#define VORTEX_BRIDGE_JSON_MAX 65536

/* Avatar state */
typedef struct {
    char name[64];                /* Avatar name */
    
    /* Emotional state (from Deep Tree Echo) */
    double joy;                   /* 0.0 to 1.0 */
    double sadness;               /* 0.0 to 1.0 */
    double anger;                 /* 0.0 to 1.0 */
    double fear;                  /* 0.0 to 1.0 */
    double surprise;              /* 0.0 to 1.0 */
    double disgust;               /* 0.0 to 1.0 */
    
    /* Expression (from Deep Tree Echo) */
    char expression[32];          /* "happy", "sad", "angry", etc. */
    
    /* Cognitive state (from Deep Tree Echo) */
    double chaos_coefficient;     /* Chaotic attractor strength */
    double resonance_threshold;   /* Thought linking threshold */
    int tree_depth;               /* Thought tree depth */
    
    /* VORTEX mapping */
    uint64_t matula;              /* Avatar's structural identity */
    Morphule* morphule;           /* Associated morphule */
    double vorticity;             /* Current vorticity */
    
} AvatarState;

/* Cognitive thought node */
typedef struct {
    char content[256];            /* Thought content */
    uint64_t matula;              /* Structural representation */
    double activation;            /* Activation level */
    double chaos_value;           /* Chaotic attractor value */
    int depth;                    /* Tree depth */
} ThoughtNode;

/* Bridge context */
typedef struct VortexBridge {
    /* VORTEX components */
    VortexConfig* vortex;         /* Shared vortex */
    Egregore* egregore;           /* Avatar egregore */
    
    /* Avatar states */
    int n_avatars;
    AvatarState** avatars;
    
    /* Configuration */
    bool enable_chaos;            /* Enable chaotic dynamics */
    bool enable_resonance;        /* Enable thought resonance */
    bool enable_coordination;     /* Enable egregore coordination */
    
    /* Callbacks */
    void (*on_thought)(struct VortexBridge* bridge, ThoughtNode* thought);
    void (*on_expression_change)(struct VortexBridge* bridge, AvatarState* avatar);
    void (*on_emotion_change)(struct VortexBridge* bridge, AvatarState* avatar);
    
} VortexBridge;

/* Bridge lifecycle */

/**
 * Create bridge
 * 
 * Initializes the integration bridge between VORTEX and Deep Tree Echo.
 * 
 * Returns: Bridge context, or NULL on error
 */
VortexBridge* vortex_bridge_create(void);

/**
 * Free bridge
 */
void vortex_bridge_free(VortexBridge* bridge);

/**
 * Initialize bridge
 * 
 * Sets up vortex, egregore, and avatar coordination.
 * 
 * Returns: 0 on success, -1 on error
 */
int vortex_bridge_init(VortexBridge* bridge);

/* Avatar management */

/**
 * Register avatar
 * 
 * Registers a Deep Tree Echo avatar with the VORTEX system.
 * Creates a morphule for the avatar and adds it to the egregore.
 * 
 * name: Avatar name
 * 
 * Returns: Avatar state, or NULL on error
 */
AvatarState* vortex_bridge_register_avatar(VortexBridge* bridge, const char* name);

/**
 * Unregister avatar
 * 
 * Removes avatar from the system.
 * 
 * Returns: 0 on success, -1 on error
 */
int vortex_bridge_unregister_avatar(VortexBridge* bridge, const char* name);

/**
 * Get avatar by name
 */
AvatarState* vortex_bridge_get_avatar(VortexBridge* bridge, const char* name);

/* Emotional state mapping */

/**
 * Set avatar emotion
 * 
 * Maps Deep Tree Echo emotional state to morphule quirk and vorticity.
 * 
 * avatar: Avatar name
 * joy, sadness, anger, fear, surprise, disgust: Emotion values (0.0 to 1.0)
 * 
 * Returns: 0 on success, -1 on error
 */
int vortex_bridge_set_emotion(VortexBridge* bridge, const char* avatar,
                               double joy, double sadness, double anger,
                               double fear, double surprise, double disgust);

/**
 * Get avatar emotion
 * 
 * Retrieves current emotional state.
 * 
 * Returns: 0 on success, -1 on error
 */
int vortex_bridge_get_emotion(VortexBridge* bridge, const char* avatar,
                               double* joy, double* sadness, double* anger,
                               double* fear, double* surprise, double* disgust);

/* Expression mapping */

/**
 * Set avatar expression
 * 
 * Maps expression to morphule state.
 * 
 * avatar: Avatar name
 * expression: Expression name ("happy", "sad", "angry", etc.)
 * 
 * Returns: 0 on success, -1 on error
 */
int vortex_bridge_set_expression(VortexBridge* bridge, const char* avatar, const char* expression);

/**
 * Get avatar expression
 * 
 * Returns: Expression string, or NULL on error
 */
const char* vortex_bridge_get_expression(VortexBridge* bridge, const char* avatar);

/* Cognitive state mapping */

/**
 * Process thought
 * 
 * Converts Deep Tree Echo thought to VORTEX representation.
 * 
 * avatar: Avatar name
 * content: Thought content
 * chaos_value: Chaotic attractor value
 * depth: Tree depth
 * 
 * Returns: ThoughtNode with Matula number, or NULL on error
 */
ThoughtNode* vortex_bridge_process_thought(VortexBridge* bridge, const char* avatar,
                                            const char* content, double chaos_value, int depth);

/**
 * Free thought node
 */
void vortex_bridge_free_thought(ThoughtNode* thought);

/**
 * Link thoughts
 * 
 * Creates resonance link between thoughts if similarity exceeds threshold.
 * 
 * thought1, thought2: Thoughts to link
 * 
 * Returns: Resonance strength (0.0 to 1.0), or -1.0 on error
 */
double vortex_bridge_link_thoughts(VortexBridge* bridge, ThoughtNode* thought1, ThoughtNode* thought2);

/* Egregore coordination */

/**
 * Synchronize avatars
 * 
 * Triggers egregore coordination to synchronize avatar states.
 * 
 * Returns: Coherence value (0.0 to 1.0)
 */
double vortex_bridge_synchronize(VortexBridge* bridge);

/**
 * Get collective emotion
 * 
 * Computes collective emotional state across all avatars.
 * 
 * Returns: 0 on success, -1 on error
 */
int vortex_bridge_get_collective_emotion(VortexBridge* bridge,
                                          double* joy, double* sadness, double* anger,
                                          double* fear, double* surprise, double* disgust);

/* JSON serialization (for Node.js FFI) */

/**
 * Serialize avatar state to JSON
 * 
 * Returns: JSON string (caller must free), or NULL on error
 */
char* vortex_bridge_avatar_to_json(AvatarState* avatar);

/**
 * Deserialize avatar state from JSON
 * 
 * Returns: Avatar state, or NULL on error
 */
AvatarState* vortex_bridge_avatar_from_json(const char* json);

/**
 * Serialize thought to JSON
 */
char* vortex_bridge_thought_to_json(ThoughtNode* thought);

/**
 * Deserialize thought from JSON
 */
ThoughtNode* vortex_bridge_thought_from_json(const char* json);

/* Node.js N-API exports */

/**
 * Initialize Node.js module
 * 
 * Called by Node.js when loading the native addon.
 */
void* vortex_bridge_napi_init(void* env, void* exports);

/* Utilities */

/**
 * Map emotion vector to vorticity
 * 
 * Converts 6D emotion vector to scalar vorticity value.
 * 
 * Returns: Vorticity (0.0 to 1.0)
 */
double vortex_bridge_emotion_to_vorticity(double joy, double sadness, double anger,
                                           double fear, double surprise, double disgust);

/**
 * Map vorticity to emotion vector
 * 
 * Inverse mapping from vorticity to emotion space.
 */
void vortex_bridge_vorticity_to_emotion(double vorticity,
                                         double* joy, double* sadness, double* anger,
                                         double* fear, double* surprise, double* disgust);

/**
 * Compute thought similarity
 * 
 * Compares two thoughts using Matula number distance.
 * 
 * Returns: Similarity (0.0 to 1.0)
 */
double vortex_bridge_thought_similarity(ThoughtNode* t1, ThoughtNode* t2);

/**
 * Print bridge state
 */
void vortex_bridge_print(VortexBridge* bridge);

#endif /* _VORTEX_BRIDGE_H_ */
