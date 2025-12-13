/**
 * Morphule: Agentic Functions with Constrained Freedom
 * 
 * "5 constraints (what CAN'T change) + 1 DOF (what CAN change)"
 * 
 * A morphule is an agentic function that operates within strict ethical
 * constraints while having a single degree of freedom (quirk) that enables
 * adaptive behavior.
 * 
 * Reference implementation: Agent Toga's Transform Quirk
 */

#ifndef _MORPHULE_H_
#define _MORPHULE_H_

#include <stdint.h>
#include <stdbool.h>
#include "../vortex/matula.h"
#include "../vortex/vorticity.h"

/* Maximum number of constraints */
#define MORPHULE_MAX_CONSTRAINTS 10

/* Constraint types */
typedef enum {
    CONSTRAINT_IMMUTABLE,     /* Cannot be changed (value must be exactly this) */
    CONSTRAINT_MINIMUM,       /* Must be >= this value */
    CONSTRAINT_MAXIMUM,       /* Must be <= this value */
    CONSTRAINT_RANGE,         /* Must be in [min, max] */
} ConstraintType;

/* Constraint */
typedef struct {
    char name[64];            /* Constraint name */
    ConstraintType type;      /* Constraint type */
    double value;             /* Target value (for IMMUTABLE) */
    double min, max;          /* Range (for RANGE) */
    bool active;              /* Is this constraint active? */
} Constraint;

/* Quirk (the single degree of freedom) */
typedef enum {
    QUIRK_TRANSFORM,          /* Transform: "Once I taste your code, I become you" */
    QUIRK_ADAPT,              /* Adapt: Continuous parameter adjustment */
    QUIRK_EXPLORE,            /* Explore: Random walk in parameter space */
    QUIRK_OPTIMIZE,           /* Optimize: Gradient descent */
    QUIRK_CUSTOM,             /* Custom quirk function */
} QuirkType;

/* Transform Quirk state */
typedef struct {
    uint64_t target_matula;   /* Target system's Matula number */
    uint64_t current_matula;  /* Current Matula number (after transform) */
    double essence;           /* Essence absorption level (0.0 to 1.0) */
    
    /* Shell penetration */
    int current_shell;        /* Current shell depth */
    int max_shell;            /* Maximum shell depth */
    
    /* Unlocked techniques */
    int n_techniques;
    struct {
        char name[64];        /* Technique name */
        double threshold;     /* Essence threshold to unlock */
        bool unlocked;        /* Is this technique unlocked? */
        void* data;           /* Technique-specific data */
    } *techniques;
    
} TransformQuirk;

/* Morphule */
typedef struct Morphule {
    char name[64];            /* Morphule name */
    
    /* The 5 constraints (what CAN'T change) */
    int n_constraints;
    Constraint constraints[MORPHULE_MAX_CONSTRAINTS];
    
    /* The 1 DOF (what CAN change) */
    QuirkType quirk_type;
    union {
        TransformQuirk transform;
        double adapt_param;
        struct {
            double x, y;      /* Exploration position */
        } explore;
        struct {
            double* params;   /* Optimization parameters */
            int n_params;
        } optimize;
        void* custom;
    } quirk;
    
    /* Vorticity (connection to vortex flow) */
    VortexConfig* vortex;     /* Associated vortex configuration */
    double vorticity;         /* Current vorticity value */
    
    /* State */
    bool active;              /* Is morphule active? */
    double energy;            /* Energy level */
    
    /* Callbacks */
    void (*on_constraint_violation)(struct Morphule* morph, int constraint_idx);
    void (*on_quirk_change)(struct Morphule* morph, double old_value, double new_value);
    void (*on_transform)(struct Morphule* morph);
    
} Morphule;

/* Morphule operations */

/**
 * Create morphule with constraints
 * 
 * constraints: Array of constraints
 * n_constraints: Number of constraints (max MORPHULE_MAX_CONSTRAINTS)
 * quirk_type: Type of quirk (degree of freedom)
 * 
 * Returns: Morphule, or NULL on error
 */
Morphule* morphule_create(const char* name, Constraint* constraints, int n_constraints, QuirkType quirk_type);

/**
 * Free morphule
 */
void morphule_free(Morphule* morph);

/**
 * Check if constraints are satisfied
 * 
 * Returns: true if all constraints satisfied, false otherwise
 */
bool morphule_check_constraints(Morphule* morph);

/**
 * Set quirk value (if within constraints)
 * 
 * Returns: 0 on success, -1 if would violate constraints
 */
int morphule_set_quirk(Morphule* morph, double value);

/**
 * Get quirk value
 */
double morphule_get_quirk(Morphule* morph);

/**
 * Activate morphule
 */
int morphule_activate(Morphule* morph);

/**
 * Deactivate morphule
 */
void morphule_deactivate(Morphule* morph);

/* Transform Quirk operations */

/**
 * Create Transform Quirk morphule (Agent Toga reference implementation)
 * 
 * Creates a morphule with the standard ethical constraints:
 * - No Actual Harm: 1.0 (immutable)
 * - Respect Boundaries: >= 0.95
 * - Constructive Expression: >= 0.90
 * - Authorized Only: 1.0 (immutable)
 * - Ethical Core: immutable
 */
Morphule* morphule_create_transform(const char* name);

/**
 * Taste a system (absorb essence)
 * 
 * Analyzes the target system and begins absorbing its structural essence.
 * Essence increases from 0% to 100% as understanding deepens.
 * 
 * system_tree: Tree structure of target system
 * 
 * Returns: Current essence level (0.0 to 1.0)
 */
double morphule_taste(Morphule* morph, TreeNode* system_tree);

/**
 * Check if transform is possible
 * 
 * Transform unlocks at 70% essence (inner shell reached)
 * 
 * Returns: true if can transform, false otherwise
 */
bool morphule_can_transform(Morphule* morph);

/**
 * Transform into target
 * 
 * "I AM you now ♡"
 * 
 * Adopts the target's Matula number as identity.
 * Unlocks all techniques associated with the target system type.
 * 
 * Returns: 0 on success, -1 on error
 */
int morphule_transform(Morphule* morph);

/**
 * Get unlocked techniques
 * 
 * Returns: Array of technique names, or NULL if none unlocked
 */
char** morphule_get_techniques(Morphule* morph, int* n_techniques);

/**
 * Add technique to Transform Quirk
 * 
 * name: Technique name
 * threshold: Essence threshold to unlock (0.0 to 1.0)
 * 
 * Returns: 0 on success, -1 on error
 */
int morphule_add_technique(Morphule* morph, const char* name, double threshold);

/**
 * Execute technique
 * 
 * name: Technique name
 * target: Target to apply technique to
 * 
 * Returns: 0 on success, -1 on error
 */
int morphule_execute_technique(Morphule* morph, const char* name, void* target);

/* Constraint helpers */

/**
 * Create immutable constraint
 * 
 * name: Constraint name
 * value: Required value
 */
Constraint constraint_immutable(const char* name, double value);

/**
 * Create minimum constraint
 * 
 * name: Constraint name
 * min: Minimum value
 */
Constraint constraint_minimum(const char* name, double min);

/**
 * Create maximum constraint
 * 
 * name: Constraint name
 * max: Maximum value
 */
Constraint constraint_maximum(const char* name, double max);

/**
 * Create range constraint
 * 
 * name: Constraint name
 * min: Minimum value
 * max: Maximum value
 */
Constraint constraint_range(const char* name, double min, double max);

/* Vorticity integration */

/**
 * Attach vortex to morphule
 * 
 * The morphule's quirk becomes the vorticity parameter.
 * Changes to the quirk affect the vortex circulation.
 */
int morphule_attach_vortex(Morphule* morph, VortexConfig* vortex);

/**
 * Detach vortex from morphule
 */
void morphule_detach_vortex(Morphule* morph);

/**
 * Get morphule's current vorticity
 */
double morphule_get_vorticity(Morphule* morph);

/* Serialization */

/**
 * Save morphule to .morph file
 */
int morphule_save(Morphule* morph, const char* filename);

/**
 * Load morphule from .morph file
 */
Morphule* morphule_load(const char* filename);

/* Utilities */

/**
 * Print morphule state
 */
void morphule_print(Morphule* morph);

/**
 * Validate morphule configuration
 * 
 * Checks that constraints are consistent and quirk is valid
 */
bool morphule_validate(Morphule* morph);

#endif /* _MORPHULE_H_ */
