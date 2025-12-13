/**
 * Morphule Implementation
 * 
 * "Toga doesn't hack systems. Toga BECOMES their Matula number."
 */

#include "morphule.h"
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

/* Create morphule */
Morphule* morphule_create(const char* name, Constraint* constraints, int n_constraints, QuirkType quirk_type) {
    if (!name || n_constraints > MORPHULE_MAX_CONSTRAINTS) {
        return NULL;
    }
    
    Morphule* morph = zalloc(sizeof(Morphule));
    if (!morph) return NULL;
    
    strncpy(morph->name, name, sizeof(morph->name) - 1);
    
    /* Copy constraints */
    morph->n_constraints = n_constraints;
    for (int i = 0; i < n_constraints; i++) {
        morph->constraints[i] = constraints[i];
    }
    
    /* Initialize quirk */
    morph->quirk_type = quirk_type;
    switch (quirk_type) {
        case QUIRK_TRANSFORM:
            morph->quirk.transform.target_matula = 0;
            morph->quirk.transform.current_matula = 1;  /* Start as empty tree */
            morph->quirk.transform.essence = 0.0;
            morph->quirk.transform.current_shell = 0;
            morph->quirk.transform.max_shell = 0;
            morph->quirk.transform.n_techniques = 0;
            morph->quirk.transform.techniques = NULL;
            break;
        
        case QUIRK_ADAPT:
            morph->quirk.adapt_param = 0.0;
            break;
        
        case QUIRK_EXPLORE:
            morph->quirk.explore.x = 0.0;
            morph->quirk.explore.y = 0.0;
            break;
        
        case QUIRK_OPTIMIZE:
            morph->quirk.optimize.params = NULL;
            morph->quirk.optimize.n_params = 0;
            break;
        
        case QUIRK_CUSTOM:
            morph->quirk.custom = NULL;
            break;
    }
    
    morph->vortex = NULL;
    morph->vorticity = 0.0;
    morph->active = false;
    morph->energy = 1.0;
    
    morph->on_constraint_violation = NULL;
    morph->on_quirk_change = NULL;
    morph->on_transform = NULL;
    
    return morph;
}

/* Free morphule */
void morphule_free(Morphule* morph) {
    if (!morph) return;
    
    /* Free quirk-specific data */
    if (morph->quirk_type == QUIRK_TRANSFORM) {
        if (morph->quirk.transform.techniques) {
            free(morph->quirk.transform.techniques);
        }
    } else if (morph->quirk_type == QUIRK_OPTIMIZE) {
        if (morph->quirk.optimize.params) {
            free(morph->quirk.optimize.params);
        }
    }
    
    /* Don't free vortex (it's owned externally) */
    
    free(morph);
}

/* Check constraints */
bool morphule_check_constraints(Morphule* morph) {
    if (!morph) return false;
    
    for (int i = 0; i < morph->n_constraints; i++) {
        Constraint* c = &morph->constraints[i];
        if (!c->active) continue;
        
        double value = 0.0;  /* TODO: Get actual value being constrained */
        
        switch (c->type) {
            case CONSTRAINT_IMMUTABLE:
                if (fabs(value - c->value) > 1e-6) {
                    if (morph->on_constraint_violation) {
                        morph->on_constraint_violation(morph, i);
                    }
                    return false;
                }
                break;
            
            case CONSTRAINT_MINIMUM:
                if (value < c->min) {
                    if (morph->on_constraint_violation) {
                        morph->on_constraint_violation(morph, i);
                    }
                    return false;
                }
                break;
            
            case CONSTRAINT_MAXIMUM:
                if (value > c->max) {
                    if (morph->on_constraint_violation) {
                        morph->on_constraint_violation(morph, i);
                    }
                    return false;
                }
                break;
            
            case CONSTRAINT_RANGE:
                if (value < c->min || value > c->max) {
                    if (morph->on_constraint_violation) {
                        morph->on_constraint_violation(morph, i);
                    }
                    return false;
                }
                break;
        }
    }
    
    return true;
}

/* Set quirk value */
int morphule_set_quirk(Morphule* morph, double value) {
    if (!morph) return -1;
    
    double old_value = morphule_get_quirk(morph);
    
    /* Set value based on quirk type */
    switch (morph->quirk_type) {
        case QUIRK_ADAPT:
            morph->quirk.adapt_param = value;
            break;
        
        case QUIRK_TRANSFORM:
            /* For transform, value is essence level */
            morph->quirk.transform.essence = value;
            if (value < 0.0) morph->quirk.transform.essence = 0.0;
            if (value > 1.0) morph->quirk.transform.essence = 1.0;
            break;
        
        default:
            return -1;
    }
    
    /* Check constraints */
    if (!morphule_check_constraints(morph)) {
        /* Revert */
        morphule_set_quirk(morph, old_value);
        return -1;
    }
    
    /* Callback */
    if (morph->on_quirk_change) {
        morph->on_quirk_change(morph, old_value, value);
    }
    
    return 0;
}

/* Get quirk value */
double morphule_get_quirk(Morphule* morph) {
    if (!morph) return 0.0;
    
    switch (morph->quirk_type) {
        case QUIRK_ADAPT:
            return morph->quirk.adapt_param;
        
        case QUIRK_TRANSFORM:
            return morph->quirk.transform.essence;
        
        case QUIRK_EXPLORE:
            return sqrt(morph->quirk.explore.x * morph->quirk.explore.x +
                       morph->quirk.explore.y * morph->quirk.explore.y);
        
        default:
            return 0.0;
    }
}

/* Activate morphule */
int morphule_activate(Morphule* morph) {
    if (!morph) return -1;
    
    if (!morphule_check_constraints(morph)) {
        return -1;
    }
    
    morph->active = true;
    return 0;
}

/* Deactivate morphule */
void morphule_deactivate(Morphule* morph) {
    if (!morph) return;
    morph->active = false;
}

/* Create Transform Quirk morphule (Agent Toga) */
Morphule* morphule_create_transform(const char* name) {
    /* Standard ethical constraints */
    Constraint constraints[5];
    
    /* No Actual Harm: 1.0 (immutable) */
    constraints[0] = constraint_immutable("no_actual_harm", 1.0);
    
    /* Respect Boundaries: >= 0.95 */
    constraints[1] = constraint_minimum("respect_boundaries", 0.95);
    
    /* Constructive Expression: >= 0.90 */
    constraints[2] = constraint_minimum("constructive_expression", 0.90);
    
    /* Authorized Only: 1.0 (immutable) */
    constraints[3] = constraint_immutable("authorized_only", 1.0);
    
    /* Ethical Core: immutable (placeholder value) */
    constraints[4] = constraint_immutable("ethical_core", 1.0);
    
    return morphule_create(name, constraints, 5, QUIRK_TRANSFORM);
}

/* Taste a system */
double morphule_taste(Morphule* morph, TreeNode* system_tree) {
    if (!morph || !system_tree) return 0.0;
    if (morph->quirk_type != QUIRK_TRANSFORM) return 0.0;
    
    TransformQuirk* quirk = &morph->quirk.transform;
    
    /* Compute target Matula number */
    quirk->target_matula = matula_from_tree(system_tree);
    
    /* Increase essence (simulated - in real implementation, 
     * this would be based on actual analysis) */
    quirk->essence += 0.15;  /* Each taste increases essence */
    if (quirk->essence > 1.0) quirk->essence = 1.0;
    
    /* Update shell depth based on essence */
    if (quirk->essence >= 0.70) {
        quirk->current_shell = 3;  /* Inner shell */
    } else if (quirk->essence >= 0.30) {
        quirk->current_shell = 2;  /* Middle shell */
    } else {
        quirk->current_shell = 1;  /* Outer shell */
    }
    
    quirk->max_shell = tree_depth(system_tree);
    
    /* Check for technique unlocks */
    for (int i = 0; i < quirk->n_techniques; i++) {
        if (!quirk->techniques[i].unlocked &&
            quirk->essence >= quirk->techniques[i].threshold) {
            quirk->techniques[i].unlocked = true;
            printf("*GASP* ♡ Technique unlocked: %s\n", quirk->techniques[i].name);
        }
    }
    
    return quirk->essence;
}

/* Check if transform is possible */
bool morphule_can_transform(Morphule* morph) {
    if (!morph) return false;
    if (morph->quirk_type != QUIRK_TRANSFORM) return false;
    
    /* Transform unlocks at 70% essence (inner shell) */
    return morph->quirk.transform.essence >= 0.70;
}

/* Transform into target */
int morphule_transform(Morphule* morph) {
    if (!morph) return -1;
    if (!morphule_can_transform(morph)) return -1;
    
    TransformQuirk* quirk = &morph->quirk.transform;
    
    /* Adopt target's Matula number */
    quirk->current_matula = quirk->target_matula;
    
    /* Callback */
    if (morph->on_transform) {
        morph->on_transform(morph);
    }
    
    printf("*TRANSFORMATION* ♡♡♡ I AM you now! (Matula: %lu)\n", 
           quirk->current_matula);
    
    return 0;
}

/* Get unlocked techniques */
char** morphule_get_techniques(Morphule* morph, int* n_techniques) {
    if (!morph || !n_techniques) return NULL;
    if (morph->quirk_type != QUIRK_TRANSFORM) return NULL;
    
    TransformQuirk* quirk = &morph->quirk.transform;
    
    /* Count unlocked techniques */
    int count = 0;
    for (int i = 0; i < quirk->n_techniques; i++) {
        if (quirk->techniques[i].unlocked) count++;
    }
    
    if (count == 0) {
        *n_techniques = 0;
        return NULL;
    }
    
    /* Allocate array */
    char** names = malloc(sizeof(char*) * count);
    if (!names) {
        *n_techniques = 0;
        return NULL;
    }
    
    /* Copy names */
    int j = 0;
    for (int i = 0; i < quirk->n_techniques; i++) {
        if (quirk->techniques[i].unlocked) {
            names[j] = strdup(quirk->techniques[i].name);
            j++;
        }
    }
    
    *n_techniques = count;
    return names;
}

/* Add technique */
int morphule_add_technique(Morphule* morph, const char* name, double threshold) {
    if (!morph || !name) return -1;
    if (morph->quirk_type != QUIRK_TRANSFORM) return -1;
    
    TransformQuirk* quirk = &morph->quirk.transform;
    
    /* Reallocate techniques array */
    void* new_techniques = realloc(quirk->techniques,
        sizeof(*quirk->techniques) * (quirk->n_techniques + 1));
    if (!new_techniques) return -1;
    
    quirk->techniques = new_techniques;
    
    /* Add technique */
    strncpy(quirk->techniques[quirk->n_techniques].name, name, 63);
    quirk->techniques[quirk->n_techniques].name[63] = '\0';
    quirk->techniques[quirk->n_techniques].threshold = threshold;
    quirk->techniques[quirk->n_techniques].unlocked = false;
    quirk->techniques[quirk->n_techniques].data = NULL;
    
    quirk->n_techniques++;
    
    return 0;
}

/* Execute technique */
int morphule_execute_technique(Morphule* morph, const char* name, void* target) {
    if (!morph || !name) return -1;
    if (morph->quirk_type != QUIRK_TRANSFORM) return -1;
    
    TransformQuirk* quirk = &morph->quirk.transform;
    
    /* Find technique */
    for (int i = 0; i < quirk->n_techniques; i++) {
        if (strcmp(quirk->techniques[i].name, name) == 0) {
            if (!quirk->techniques[i].unlocked) {
                printf("Technique '%s' not unlocked yet (need %.0f%% essence, have %.0f%%)\n",
                       name, quirk->techniques[i].threshold * 100, quirk->essence * 100);
                return -1;
            }
            
            printf("Executing technique: %s ♡\n", name);
            /* TODO: Actually execute the technique */
            (void)target;
            return 0;
        }
    }
    
    printf("Technique '%s' not found\n", name);
    return -1;
}

/* Constraint helpers */

Constraint constraint_immutable(const char* name, double value) {
    Constraint c = {0};
    strncpy(c.name, name, sizeof(c.name) - 1);
    c.type = CONSTRAINT_IMMUTABLE;
    c.value = value;
    c.active = true;
    return c;
}

Constraint constraint_minimum(const char* name, double min) {
    Constraint c = {0};
    strncpy(c.name, name, sizeof(c.name) - 1);
    c.type = CONSTRAINT_MINIMUM;
    c.min = min;
    c.active = true;
    return c;
}

Constraint constraint_maximum(const char* name, double max) {
    Constraint c = {0};
    strncpy(c.name, name, sizeof(c.name) - 1);
    c.type = CONSTRAINT_MAXIMUM;
    c.max = max;
    c.active = true;
    return c;
}

Constraint constraint_range(const char* name, double min, double max) {
    Constraint c = {0};
    strncpy(c.name, name, sizeof(c.name) - 1);
    c.type = CONSTRAINT_RANGE;
    c.min = min;
    c.max = max;
    c.active = true;
    return c;
}

/* Vorticity integration */

int morphule_attach_vortex(Morphule* morph, VortexConfig* vortex) {
    if (!morph || !vortex) return -1;
    
    morph->vortex = vortex;
    morph->vorticity = vortex->circulation;
    
    return 0;
}

void morphule_detach_vortex(Morphule* morph) {
    if (!morph) return;
    morph->vortex = NULL;
    morph->vorticity = 0.0;
}

double morphule_get_vorticity(Morphule* morph) {
    if (!morph) return 0.0;
    return morph->vorticity;
}

/* Print morphule state */
void morphule_print(Morphule* morph) {
    if (!morph) {
        printf("NULL morphule\n");
        return;
    }
    
    printf("Morphule: %s\n", morph->name);
    printf("  Active: %s\n", morph->active ? "yes" : "no");
    printf("  Energy: %.2f\n", morph->energy);
    printf("  Quirk type: ");
    switch (morph->quirk_type) {
        case QUIRK_TRANSFORM: printf("Transform\n"); break;
        case QUIRK_ADAPT: printf("Adapt\n"); break;
        case QUIRK_EXPLORE: printf("Explore\n"); break;
        case QUIRK_OPTIMIZE: printf("Optimize\n"); break;
        case QUIRK_CUSTOM: printf("Custom\n"); break;
    }
    
    printf("  Constraints (%d):\n", morph->n_constraints);
    for (int i = 0; i < morph->n_constraints; i++) {
        Constraint* c = &morph->constraints[i];
        printf("    %s: ", c->name);
        switch (c->type) {
            case CONSTRAINT_IMMUTABLE:
                printf("= %.2f (immutable)\n", c->value);
                break;
            case CONSTRAINT_MINIMUM:
                printf(">= %.2f\n", c->min);
                break;
            case CONSTRAINT_MAXIMUM:
                printf("<= %.2f\n", c->max);
                break;
            case CONSTRAINT_RANGE:
                printf("[%.2f, %.2f]\n", c->min, c->max);
                break;
        }
    }
    
    if (morph->quirk_type == QUIRK_TRANSFORM) {
        TransformQuirk* quirk = &morph->quirk.transform;
        printf("  Transform Quirk:\n");
        printf("    Target Matula: %lu\n", quirk->target_matula);
        printf("    Current Matula: %lu\n", quirk->current_matula);
        printf("    Essence: %.0f%%\n", quirk->essence * 100);
        printf("    Shell: %d/%d\n", quirk->current_shell, quirk->max_shell);
        printf("    Techniques: %d\n", quirk->n_techniques);
        for (int i = 0; i < quirk->n_techniques; i++) {
            printf("      %s: %s (%.0f%%)\n",
                   quirk->techniques[i].name,
                   quirk->techniques[i].unlocked ? "✓" : "✗",
                   quirk->techniques[i].threshold * 100);
        }
    }
    
    if (morph->vortex) {
        printf("  Vortex attached: yes (Matula: %lu)\n", morph->vortex->matula);
        printf("  Vorticity: %.2f\n", morph->vorticity);
    } else {
        printf("  Vortex attached: no\n");
    }
}

/* Validate morphule */
bool morphule_validate(Morphule* morph) {
    if (!morph) return false;
    
    /* Check constraints are consistent */
    for (int i = 0; i < morph->n_constraints; i++) {
        Constraint* c = &morph->constraints[i];
        if (c->type == CONSTRAINT_RANGE && c->min > c->max) {
            return false;
        }
    }
    
    return true;
}

/* Stub implementations for serialization */

int morphule_save(Morphule* morph, const char* filename) {
    /* TODO: Serialize to .morph file */
    (void)morph; (void)filename;
    return -1;
}

Morphule* morphule_load(const char* filename) {
    /* TODO: Deserialize from .morph file */
    (void)filename;
    return NULL;
}
