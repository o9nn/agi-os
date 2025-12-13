/**
 * VORTEX-MORPHULE-EGREGORE ↔ Deep Tree Echo Integration Bridge Implementation
 */

#include "vortex_bridge.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

/* Helper: Allocate and zero memory */
static void* zalloc(size_t size) {
    void* ptr = malloc(size);
    if (ptr) {
        memset(ptr, 0, size);
    }
    return ptr;
}

/* Create bridge */
VortexBridge* vortex_bridge_create(void) {
    VortexBridge* bridge = zalloc(sizeof(VortexBridge));
    if (!bridge) return NULL;
    
    bridge->vortex = NULL;
    bridge->egregore = NULL;
    bridge->n_avatars = 0;
    bridge->avatars = NULL;
    
    bridge->enable_chaos = true;
    bridge->enable_resonance = true;
    bridge->enable_coordination = true;
    
    bridge->on_thought = NULL;
    bridge->on_expression_change = NULL;
    bridge->on_emotion_change = NULL;
    
    return bridge;
}

/* Free bridge */
void vortex_bridge_free(VortexBridge* bridge) {
    if (!bridge) return;
    
    /* Free avatars */
    if (bridge->avatars) {
        for (int i = 0; i < bridge->n_avatars; i++) {
            if (bridge->avatars[i]) {
                /* Don't free morphule (owned by egregore) */
                free(bridge->avatars[i]);
            }
        }
        free(bridge->avatars);
    }
    
    /* Don't free vortex or egregore (owned externally) */
    
    free(bridge);
}

/* Initialize bridge */
int vortex_bridge_init(VortexBridge* bridge) {
    if (!bridge) return -1;
    
    /* Create shared vortex for avatar coordination */
    TreeNode* tree = tree_create_empty();
    if (!tree) return -1;
    
    bridge->vortex = vortex_from_tree(tree);
    if (!bridge->vortex) {
        tree_free(tree);
        return -1;
    }
    
    /* Create egregore for avatar swarm */
    bridge->egregore = egregore_create("avatar-swarm",
                                       "Collective avatar intelligence",
                                       bridge->vortex);
    if (!bridge->egregore) {
        vortex_free(bridge->vortex);
        return -1;
    }
    
    /* Enable swarm features */
    egregore_enable_phase_locking(bridge->egregore);
    egregore_enable_knowledge_sharing(bridge->egregore);
    bridge->egregore->swarm_transform = true;
    bridge->egregore->swarm_techniques = true;
    
    return 0;
}

/* Register avatar */
AvatarState* vortex_bridge_register_avatar(VortexBridge* bridge, const char* name) {
    if (!bridge || !name) return NULL;
    
    /* Create avatar state */
    AvatarState* avatar = zalloc(sizeof(AvatarState));
    if (!avatar) return NULL;
    
    strncpy(avatar->name, name, sizeof(avatar->name) - 1);
    
    /* Initialize emotional state (neutral) */
    avatar->joy = 0.5;
    avatar->sadness = 0.5;
    avatar->anger = 0.5;
    avatar->fear = 0.5;
    avatar->surprise = 0.5;
    avatar->disgust = 0.5;
    
    /* Initialize expression */
    strncpy(avatar->expression, "neutral", sizeof(avatar->expression) - 1);
    
    /* Initialize cognitive state */
    avatar->chaos_coefficient = 0.618;  /* Golden ratio */
    avatar->resonance_threshold = 0.7;
    avatar->tree_depth = 7;
    
    /* Create morphule for avatar */
    avatar->morphule = morphule_create_transform(name);
    if (!avatar->morphule) {
        free(avatar);
        return NULL;
    }
    
    /* Compute initial Matula number from name */
    /* Simple hash for now - in production, use proper tree structure */
    avatar->matula = 1;
    for (const char* p = name; *p; p++) {
        avatar->matula = avatar->matula * 31 + *p;
    }
    
    /* Initial vorticity from emotional state */
    avatar->vorticity = vortex_bridge_emotion_to_vorticity(
        avatar->joy, avatar->sadness, avatar->anger,
        avatar->fear, avatar->surprise, avatar->disgust);
    
    /* Add morphule to egregore */
    if (bridge->egregore) {
        egregore_summon(bridge->egregore, avatar->morphule);
    }
    
    /* Add to avatar list */
    AvatarState** new_avatars = realloc(bridge->avatars,
        sizeof(AvatarState*) * (bridge->n_avatars + 1));
    if (!new_avatars) {
        morphule_free(avatar->morphule);
        free(avatar);
        return NULL;
    }
    
    bridge->avatars = new_avatars;
    bridge->avatars[bridge->n_avatars] = avatar;
    bridge->n_avatars++;
    
    printf("Avatar '%s' registered (Matula: %lu)\n", name, avatar->matula);
    
    return avatar;
}

/* Unregister avatar */
int vortex_bridge_unregister_avatar(VortexBridge* bridge, const char* name) {
    if (!bridge || !name) return -1;
    
    /* Find avatar */
    int idx = -1;
    for (int i = 0; i < bridge->n_avatars; i++) {
        if (strcmp(bridge->avatars[i]->name, name) == 0) {
            idx = i;
            break;
        }
    }
    
    if (idx == -1) return -1;
    
    AvatarState* avatar = bridge->avatars[idx];
    
    /* Remove from egregore */
    if (bridge->egregore && avatar->morphule) {
        egregore_banish(bridge->egregore, avatar->morphule);
    }
    
    /* Free morphule */
    if (avatar->morphule) {
        morphule_free(avatar->morphule);
    }
    
    /* Remove from list */
    for (int i = idx; i < bridge->n_avatars - 1; i++) {
        bridge->avatars[i] = bridge->avatars[i + 1];
    }
    bridge->n_avatars--;
    
    free(avatar);
    
    return 0;
}

/* Get avatar by name */
AvatarState* vortex_bridge_get_avatar(VortexBridge* bridge, const char* name) {
    if (!bridge || !name) return NULL;
    
    for (int i = 0; i < bridge->n_avatars; i++) {
        if (strcmp(bridge->avatars[i]->name, name) == 0) {
            return bridge->avatars[i];
        }
    }
    
    return NULL;
}

/* Set avatar emotion */
int vortex_bridge_set_emotion(VortexBridge* bridge, const char* avatar_name,
                               double joy, double sadness, double anger,
                               double fear, double surprise, double disgust) {
    if (!bridge || !avatar_name) return -1;
    
    AvatarState* avatar = vortex_bridge_get_avatar(bridge, avatar_name);
    if (!avatar) return -1;
    
    /* Update emotional state */
    avatar->joy = joy;
    avatar->sadness = sadness;
    avatar->anger = anger;
    avatar->fear = fear;
    avatar->surprise = surprise;
    avatar->disgust = disgust;
    
    /* Map to vorticity */
    avatar->vorticity = vortex_bridge_emotion_to_vorticity(
        joy, sadness, anger, fear, surprise, disgust);
    
    /* Update morphule quirk */
    if (avatar->morphule) {
        morphule_set_quirk(avatar->morphule, avatar->vorticity);
    }
    
    /* Callback */
    if (bridge->on_emotion_change) {
        bridge->on_emotion_change(bridge, avatar);
    }
    
    return 0;
}

/* Get avatar emotion */
int vortex_bridge_get_emotion(VortexBridge* bridge, const char* avatar_name,
                               double* joy, double* sadness, double* anger,
                               double* fear, double* surprise, double* disgust) {
    if (!bridge || !avatar_name) return -1;
    
    AvatarState* avatar = vortex_bridge_get_avatar(bridge, avatar_name);
    if (!avatar) return -1;
    
    if (joy) *joy = avatar->joy;
    if (sadness) *sadness = avatar->sadness;
    if (anger) *anger = avatar->anger;
    if (fear) *fear = avatar->fear;
    if (surprise) *surprise = avatar->surprise;
    if (disgust) *disgust = avatar->disgust;
    
    return 0;
}

/* Set avatar expression */
int vortex_bridge_set_expression(VortexBridge* bridge, const char* avatar_name, const char* expression) {
    if (!bridge || !avatar_name || !expression) return -1;
    
    AvatarState* avatar = vortex_bridge_get_avatar(bridge, avatar_name);
    if (!avatar) return -1;
    
    strncpy(avatar->expression, expression, sizeof(avatar->expression) - 1);
    
    /* Callback */
    if (bridge->on_expression_change) {
        bridge->on_expression_change(bridge, avatar);
    }
    
    return 0;
}

/* Get avatar expression */
const char* vortex_bridge_get_expression(VortexBridge* bridge, const char* avatar_name) {
    if (!bridge || !avatar_name) return NULL;
    
    AvatarState* avatar = vortex_bridge_get_avatar(bridge, avatar_name);
    if (!avatar) return NULL;
    
    return avatar->expression;
}

/* Process thought */
ThoughtNode* vortex_bridge_process_thought(VortexBridge* bridge, const char* avatar_name,
                                            const char* content, double chaos_value, int depth) {
    if (!bridge || !avatar_name || !content) return NULL;
    
    AvatarState* avatar = vortex_bridge_get_avatar(bridge, avatar_name);
    if (!avatar) return NULL;
    
    /* Create thought node */
    ThoughtNode* thought = zalloc(sizeof(ThoughtNode));
    if (!thought) return NULL;
    
    strncpy(thought->content, content, sizeof(thought->content) - 1);
    thought->chaos_value = chaos_value;
    thought->depth = depth;
    thought->activation = 1.0;
    
    /* Compute Matula number from content structure */
    /* Simple hash for now - in production, parse content to tree */
    thought->matula = avatar->matula;
    for (const char* p = content; *p; p++) {
        thought->matula = thought->matula * 31 + *p;
    }
    
    /* Callback */
    if (bridge->on_thought) {
        bridge->on_thought(bridge, thought);
    }
    
    return thought;
}

/* Free thought node */
void vortex_bridge_free_thought(ThoughtNode* thought) {
    if (thought) {
        free(thought);
    }
}

/* Link thoughts */
double vortex_bridge_link_thoughts(VortexBridge* bridge, ThoughtNode* t1, ThoughtNode* t2) {
    if (!bridge || !t1 || !t2) return -1.0;
    
    /* Compute similarity using Matula distance */
    double similarity = vortex_bridge_thought_similarity(t1, t2);
    
    /* Check resonance threshold */
    if (bridge->enable_resonance && similarity > 0.7) {
        /* Create resonance link */
        printf("Thought resonance: %.2f (M1=%lu, M2=%lu)\n",
               similarity, t1->matula, t2->matula);
    }
    
    return similarity;
}

/* Synchronize avatars */
double vortex_bridge_synchronize(VortexBridge* bridge) {
    if (!bridge || !bridge->egregore) return 0.0;
    
    /* Update egregore state */
    egregore_update(bridge->egregore);
    
    /* Get coherence */
    return egregore_get_coherence(bridge->egregore);
}

/* Get collective emotion */
int vortex_bridge_get_collective_emotion(VortexBridge* bridge,
                                          double* joy, double* sadness, double* anger,
                                          double* fear, double* surprise, double* disgust) {
    if (!bridge) return -1;
    
    if (bridge->n_avatars == 0) {
        if (joy) *joy = 0.0;
        if (sadness) *sadness = 0.0;
        if (anger) *anger = 0.0;
        if (fear) *fear = 0.0;
        if (surprise) *surprise = 0.0;
        if (disgust) *disgust = 0.0;
        return 0;
    }
    
    /* Average emotions across all avatars */
    double sum_joy = 0.0, sum_sadness = 0.0, sum_anger = 0.0;
    double sum_fear = 0.0, sum_surprise = 0.0, sum_disgust = 0.0;
    
    for (int i = 0; i < bridge->n_avatars; i++) {
        AvatarState* avatar = bridge->avatars[i];
        sum_joy += avatar->joy;
        sum_sadness += avatar->sadness;
        sum_anger += avatar->anger;
        sum_fear += avatar->fear;
        sum_surprise += avatar->surprise;
        sum_disgust += avatar->disgust;
    }
    
    if (joy) *joy = sum_joy / bridge->n_avatars;
    if (sadness) *sadness = sum_sadness / bridge->n_avatars;
    if (anger) *anger = sum_anger / bridge->n_avatars;
    if (fear) *fear = sum_fear / bridge->n_avatars;
    if (surprise) *surprise = sum_surprise / bridge->n_avatars;
    if (disgust) *disgust = sum_disgust / bridge->n_avatars;
    
    return 0;
}

/* Map emotion vector to vorticity */
double vortex_bridge_emotion_to_vorticity(double joy, double sadness, double anger,
                                           double fear, double surprise, double disgust) {
    /* Compute magnitude of emotion vector */
    double sum = joy + sadness + anger + fear + surprise + disgust;
    if (sum < 0.01) return 0.0;
    
    /* Weighted combination emphasizing positive emotions */
    double vorticity = (joy * 1.5 + surprise * 1.2 + anger * 0.8 + 
                       fear * 0.5 + sadness * 0.3 + disgust * 0.2) / sum;
    
    /* Normalize to [0, 1] */
    if (vorticity < 0.0) vorticity = 0.0;
    if (vorticity > 1.0) vorticity = 1.0;
    
    return vorticity;
}

/* Map vorticity to emotion vector */
void vortex_bridge_vorticity_to_emotion(double vorticity,
                                         double* joy, double* sadness, double* anger,
                                         double* fear, double* surprise, double* disgust) {
    /* Inverse mapping (approximate) */
    if (vorticity > 0.8) {
        /* High vorticity = joy + surprise */
        if (joy) *joy = vorticity;
        if (surprise) *surprise = vorticity * 0.8;
        if (sadness) *sadness = 1.0 - vorticity;
        if (anger) *anger = 0.5;
        if (fear) *fear = 1.0 - vorticity;
        if (disgust) *disgust = 0.3;
    } else if (vorticity > 0.5) {
        /* Medium vorticity = balanced */
        if (joy) *joy = vorticity;
        if (sadness) *sadness = 1.0 - vorticity;
        if (anger) *anger = 0.5;
        if (fear) *fear = 0.5;
        if (surprise) *surprise = vorticity * 0.5;
        if (disgust) *disgust = 0.5;
    } else {
        /* Low vorticity = sadness + fear */
        if (joy) *joy = vorticity;
        if (sadness) *sadness = 1.0 - vorticity;
        if (anger) *anger = 0.5;
        if (fear) *fear = 1.0 - vorticity;
        if (surprise) *surprise = vorticity * 0.3;
        if (disgust) *disgust = 0.5;
    }
}

/* Compute thought similarity */
double vortex_bridge_thought_similarity(ThoughtNode* t1, ThoughtNode* t2) {
    if (!t1 || !t2) return 0.0;
    
    /* Use Matula number distance as similarity metric */
    uint64_t m1 = t1->matula;
    uint64_t m2 = t2->matula;
    
    /* Simple XOR distance */
    uint64_t distance = m1 ^ m2;
    
    /* Count bits (Hamming distance) */
    int bits = 0;
    while (distance) {
        bits += distance & 1;
        distance >>= 1;
    }
    
    /* Convert to similarity (0.0 to 1.0) */
    double similarity = 1.0 - (bits / 64.0);
    
    return similarity;
}

/* Print bridge state */
void vortex_bridge_print(VortexBridge* bridge) {
    if (!bridge) {
        printf("NULL bridge\n");
        return;
    }
    
    printf("VORTEX Bridge State:\n");
    printf("  Avatars: %d\n", bridge->n_avatars);
    
    for (int i = 0; i < bridge->n_avatars; i++) {
        AvatarState* avatar = bridge->avatars[i];
        printf("    %d: %s\n", i, avatar->name);
        printf("       Matula: %lu\n", avatar->matula);
        printf("       Expression: %s\n", avatar->expression);
        printf("       Vorticity: %.2f\n", avatar->vorticity);
        printf("       Emotions: joy=%.2f sad=%.2f anger=%.2f\n",
               avatar->joy, avatar->sadness, avatar->anger);
    }
    
    if (bridge->egregore) {
        printf("  Egregore:\n");
        printf("    Coherence: %.2f\n", bridge->egregore->identity.coherence);
        printf("    Circulation: %.2f\n", bridge->egregore->circulation);
    }
    
    printf("  Configuration:\n");
    printf("    Chaos: %s\n", bridge->enable_chaos ? "enabled" : "disabled");
    printf("    Resonance: %s\n", bridge->enable_resonance ? "enabled" : "disabled");
    printf("    Coordination: %s\n", bridge->enable_coordination ? "enabled" : "disabled");
}

/* Stub implementations for JSON serialization */

char* vortex_bridge_avatar_to_json(AvatarState* avatar) {
    /* TODO: Implement JSON serialization */
    (void)avatar;
    return NULL;
}

AvatarState* vortex_bridge_avatar_from_json(const char* json) {
    /* TODO: Implement JSON deserialization */
    (void)json;
    return NULL;
}

char* vortex_bridge_thought_to_json(ThoughtNode* thought) {
    /* TODO: Implement JSON serialization */
    (void)thought;
    return NULL;
}

ThoughtNode* vortex_bridge_thought_from_json(const char* json) {
    /* TODO: Implement JSON deserialization */
    (void)json;
    return NULL;
}

/* Stub for Node.js N-API */
void* vortex_bridge_napi_init(void* env, void* exports) {
    /* TODO: Implement N-API bindings */
    (void)env; (void)exports;
    return NULL;
}
