/**
 * Egregore: Daemon Constellations with Collective Intelligence
 * 
 * "The morphules don't need to TALK to each other,
 *  they just need to be in the same FLOW."
 * 
 * An egregore is a swarm of morphules that coordinate through
 * shared vortex circulation rather than message passing.
 * 
 * Coordination is TOPOLOGICAL, not MESSAGE-BASED.
 */

#ifndef _EGREGORE_H_
#define _EGREGORE_H_

#include <stdint.h>
#include <stdbool.h>
#include "../vortex/vorticity.h"
#include "../morphule/morphule.h"

/* Maximum morphules in an egregore */
#define EGREGORE_MAX_MORPHULES 1000

/* Egregore identity */
typedef struct {
    char description[256];        /* "Who we are collectively" */
    uint64_t matula;              /* Collective structure (Matula number) */
    double coherence;             /* Phase coherence (0.0 to 1.0) */
} EgregoreIdentity;

/* Egregore */
typedef struct Egregore {
    char name[64];                /* Egregore name */
    
    /* Collective identity */
    EgregoreIdentity identity;
    
    /* Shared vortex (attractor/DOF) */
    VortexConfig* vortex;         /* Shared vortex configuration */
    Point center;                 /* Vortex center in phase space */
    double circulation;           /* Total circulation (sum of morphule vorticities) */
    
    /* Morphules in the constellation */
    int n_morphules;
    Morphule** morphules;
    
    /* Phase locking */
    bool phase_locking;           /* Enable phase locking between morphules */
    double* phases;               /* Phase of each morphule */
    
    /* Knowledge sharing */
    bool knowledge_sharing;       /* Enable collective learning */
    double collective_essence;    /* Sum of all morphule essences */
    
    /* Swarm behavior */
    bool swarm_transform;         /* When ANY reaches 70%, ALL unlock */
    bool swarm_techniques;        /* Share techniques across swarm */
    
    /* Interaction matrix */
    double** interactions;        /* Phase coupling between morphules */
    
    /* State */
    bool active;                  /* Is egregore active? */
    double energy;                /* Collective energy */
    
    /* Callbacks */
    void (*on_morphule_join)(struct Egregore* egr, Morphule* morph);
    void (*on_morphule_leave)(struct Egregore* egr, Morphule* morph);
    void (*on_collective_transform)(struct Egregore* egr);
    void (*on_coherence_change)(struct Egregore* egr, double old_coherence, double new_coherence);
    
} Egregore;

/* Pattern (emergent behavior) */
typedef struct {
    char name[64];                /* Pattern name */
    double strength;              /* Pattern strength */
    int n_participants;           /* Number of morphules exhibiting pattern */
    int* participant_indices;     /* Indices of participating morphules */
} Pattern;

/* Egregore operations */

/**
 * Create egregore
 * 
 * name: Egregore name
 * identity_description: Collective identity description
 * vortex: Shared vortex configuration (can be NULL, will create default)
 * 
 * Returns: Egregore, or NULL on error
 */
Egregore* egregore_create(const char* name, const char* identity_description, VortexConfig* vortex);

/**
 * Free egregore
 * 
 * Note: Does NOT free morphules (they're owned externally)
 */
void egregore_free(Egregore* egr);

/**
 * Summon morphule into egregore
 * 
 * Adds a morphule to the constellation and attaches it to the shared vortex.
 * 
 * Returns: 0 on success, -1 on error
 */
int egregore_summon(Egregore* egr, Morphule* morph);

/**
 * Banish morphule from egregore
 * 
 * Removes a morphule from the constellation.
 * 
 * Returns: 0 on success, -1 on error
 */
int egregore_banish(Egregore* egr, Morphule* morph);

/**
 * Get collective identity
 * 
 * Returns: Identity structure
 */
EgregoreIdentity egregore_get_identity(Egregore* egr);

/**
 * Get collective circulation
 * 
 * Sum of all morphule vorticities
 * 
 * Returns: Total circulation
 */
double egregore_get_circulation(Egregore* egr);

/**
 * Update collective state
 * 
 * Recomputes collective properties:
 * - Total circulation
 * - Collective essence
 * - Phase coherence
 * - Collective Matula number
 */
void egregore_update(Egregore* egr);

/* Collective intelligence */

/**
 * Detect emergent patterns
 * 
 * Analyzes morphule states to find collective behaviors
 * 
 * Returns: Array of patterns, or NULL if none detected
 */
Pattern** egregore_detect_patterns(Egregore* egr, int* n_patterns);

/**
 * Get collective essence
 * 
 * Sum of all morphule essences (for Transform Quirk morphules)
 * 
 * Returns: Total essence
 */
double egregore_get_collective_essence(Egregore* egr);

/**
 * Check if collective transform is possible
 * 
 * Returns true if ANY morphule can transform (when swarm_transform enabled)
 */
bool egregore_can_collective_transform(Egregore* egr);

/**
 * Trigger collective transform
 * 
 * When swarm_transform is enabled and ANY morphule reaches 70% essence,
 * ALL morphules unlock their techniques.
 * 
 * Returns: 0 on success, -1 on error
 */
int egregore_collective_transform(Egregore* egr);

/* Phase locking */

/**
 * Enable phase locking
 * 
 * Morphules synchronize their phases through vortex coupling
 */
void egregore_enable_phase_locking(Egregore* egr);

/**
 * Disable phase locking
 */
void egregore_disable_phase_locking(Egregore* egr);

/**
 * Get phase coherence
 * 
 * Measures how synchronized the morphules are (0.0 to 1.0)
 * 
 * Returns: Coherence value
 */
double egregore_get_coherence(Egregore* egr);

/**
 * Set interaction strength between morphules
 * 
 * Controls phase coupling strength
 * 
 * i, j: Morphule indices
 * strength: Coupling strength (0.0 to 1.0)
 */
void egregore_set_interaction(Egregore* egr, int i, int j, double strength);

/* Knowledge sharing */

/**
 * Enable knowledge sharing
 * 
 * Morphules share learned techniques across the swarm
 */
void egregore_enable_knowledge_sharing(Egregore* egr);

/**
 * Disable knowledge sharing
 */
void egregore_disable_knowledge_sharing(Egregore* egr);

/**
 * Share technique across swarm
 * 
 * When one morphule unlocks a technique, all morphules get it
 * 
 * technique_name: Name of technique to share
 * source_morph: Morphule that unlocked the technique
 */
void egregore_share_technique(Egregore* egr, const char* technique_name, Morphule* source_morph);

/* Dynamics */

/**
 * Evolve egregore dynamics
 * 
 * Time-step the collective behavior:
 * - Update vortex flow
 * - Phase locking
 * - Knowledge sharing
 * - Pattern detection
 * 
 * dt: Time step
 */
void egregore_evolve(Egregore* egr, double dt);

/**
 * Compute collective flow field
 * 
 * Superposition of all morphule vortices
 * 
 * Returns: Velocity vector at point p
 */
Vector egregore_flow(Egregore* egr, Point p);

/* Activation */

/**
 * Activate egregore
 * 
 * Activates all morphules and begins collective dynamics
 */
int egregore_activate(Egregore* egr);

/**
 * Deactivate egregore
 * 
 * Deactivates all morphules and stops dynamics
 */
void egregore_deactivate(Egregore* egr);

/* Serialization */

/**
 * Save egregore to .egregore file
 */
int egregore_save(Egregore* egr, const char* filename);

/**
 * Load egregore from .egregore file
 */
Egregore* egregore_load(const char* filename);

/* Utilities */

/**
 * Print egregore state
 */
void egregore_print(Egregore* egr);

/**
 * Visualize egregore constellation
 * 
 * Generates visualization of morphule positions and interactions
 */
int egregore_visualize(Egregore* egr, const char* filename);

/**
 * Get morphule by name
 */
Morphule* egregore_get_morphule(Egregore* egr, const char* name);

/**
 * Get morphule count
 */
int egregore_get_morphule_count(Egregore* egr);

/* Pattern utilities */

/**
 * Free pattern
 */
void pattern_free(Pattern* pattern);

/**
 * Free pattern array
 */
void patterns_free(Pattern** patterns, int n_patterns);

/**
 * Print pattern
 */
void pattern_print(Pattern* pattern);

#endif /* _EGREGORE_H_ */
