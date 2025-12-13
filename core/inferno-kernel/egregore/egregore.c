/**
 * Egregore Implementation
 * 
 * "This is literally how bird flocks and fish schools work.
 *  Stigmergic coordination through shared medium."
 */

#include "egregore.h"
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

/* Create egregore */
Egregore* egregore_create(const char* name, const char* identity_description, VortexConfig* vortex) {
    if (!name) return NULL;
    
    Egregore* egr = zalloc(sizeof(Egregore));
    if (!egr) return NULL;
    
    strncpy(egr->name, name, sizeof(egr->name) - 1);
    
    /* Initialize identity */
    if (identity_description) {
        strncpy(egr->identity.description, identity_description, 
                sizeof(egr->identity.description) - 1);
    }
    egr->identity.matula = 1;  /* Start as empty tree */
    egr->identity.coherence = 0.0;
    
    /* Initialize vortex */
    if (vortex) {
        egr->vortex = vortex;
    } else {
        /* Create default vortex */
        TreeNode* tree = tree_create_empty();
        if (tree) {
            egr->vortex = vortex_from_tree(tree);
        }
    }
    
    egr->center.x = 0.0;
    egr->center.y = 0.0;
    egr->circulation = 0.0;
    
    /* Initialize morphule list */
    egr->n_morphules = 0;
    egr->morphules = NULL;
    
    /* Initialize phase locking */
    egr->phase_locking = false;
    egr->phases = NULL;
    
    /* Initialize knowledge sharing */
    egr->knowledge_sharing = false;
    egr->collective_essence = 0.0;
    
    /* Initialize swarm behavior */
    egr->swarm_transform = false;
    egr->swarm_techniques = false;
    
    /* Initialize interaction matrix */
    egr->interactions = NULL;
    
    /* Initialize state */
    egr->active = false;
    egr->energy = 1.0;
    
    /* Initialize callbacks */
    egr->on_morphule_join = NULL;
    egr->on_morphule_leave = NULL;
    egr->on_collective_transform = NULL;
    egr->on_coherence_change = NULL;
    
    return egr;
}

/* Free egregore */
void egregore_free(Egregore* egr) {
    if (!egr) return;
    
    /* Free morphule list (but not morphules themselves) */
    if (egr->morphules) {
        free(egr->morphules);
    }
    
    /* Free phases */
    if (egr->phases) {
        free(egr->phases);
    }
    
    /* Free interaction matrix */
    if (egr->interactions) {
        for (int i = 0; i < egr->n_morphules; i++) {
            free(egr->interactions[i]);
        }
        free(egr->interactions);
    }
    
    /* Don't free vortex (owned externally) */
    
    free(egr);
}

/* Summon morphule */
int egregore_summon(Egregore* egr, Morphule* morph) {
    if (!egr || !morph) return -1;
    if (egr->n_morphules >= EGREGORE_MAX_MORPHULES) return -1;
    
    /* Reallocate morphule array */
    Morphule** new_morphules = realloc(egr->morphules,
        sizeof(Morphule*) * (egr->n_morphules + 1));
    if (!new_morphules) return -1;
    
    egr->morphules = new_morphules;
    egr->morphules[egr->n_morphules] = morph;
    
    /* Reallocate phases array */
    if (egr->phase_locking) {
        double* new_phases = realloc(egr->phases,
            sizeof(double) * (egr->n_morphules + 1));
        if (!new_phases) return -1;
        
        egr->phases = new_phases;
        egr->phases[egr->n_morphules] = 0.0;  /* Start at phase 0 */
    }
    
    /* Reallocate interaction matrix */
    if (egr->interactions) {
        /* Add row */
        double** new_interactions = realloc(egr->interactions,
            sizeof(double*) * (egr->n_morphules + 1));
        if (!new_interactions) return -1;
        
        egr->interactions = new_interactions;
        egr->interactions[egr->n_morphules] = zalloc(sizeof(double) * (egr->n_morphules + 1));
        if (!egr->interactions[egr->n_morphules]) return -1;
        
        /* Add column to existing rows */
        for (int i = 0; i < egr->n_morphules; i++) {
            double* new_row = realloc(egr->interactions[i],
                sizeof(double) * (egr->n_morphules + 1));
            if (!new_row) return -1;
            
            egr->interactions[i] = new_row;
            egr->interactions[i][egr->n_morphules] = 0.0;
        }
    }
    
    egr->n_morphules++;
    
    /* Attach vortex to morphule */
    if (egr->vortex) {
        morphule_attach_vortex(morph, egr->vortex);
    }
    
    /* Update collective state */
    egregore_update(egr);
    
    /* Callback */
    if (egr->on_morphule_join) {
        egr->on_morphule_join(egr, morph);
    }
    
    return 0;
}

/* Banish morphule */
int egregore_banish(Egregore* egr, Morphule* morph) {
    if (!egr || !morph) return -1;
    
    /* Find morphule */
    int idx = -1;
    for (int i = 0; i < egr->n_morphules; i++) {
        if (egr->morphules[i] == morph) {
            idx = i;
            break;
        }
    }
    
    if (idx == -1) return -1;  /* Not found */
    
    /* Detach vortex */
    morphule_detach_vortex(morph);
    
    /* Remove from array */
    for (int i = idx; i < egr->n_morphules - 1; i++) {
        egr->morphules[i] = egr->morphules[i + 1];
        if (egr->phases) {
            egr->phases[i] = egr->phases[i + 1];
        }
    }
    
    egr->n_morphules--;
    
    /* TODO: Shrink arrays */
    
    /* Update collective state */
    egregore_update(egr);
    
    /* Callback */
    if (egr->on_morphule_leave) {
        egr->on_morphule_leave(egr, morph);
    }
    
    return 0;
}

/* Get collective identity */
EgregoreIdentity egregore_get_identity(Egregore* egr) {
    if (!egr) {
        EgregoreIdentity empty = {0};
        return empty;
    }
    return egr->identity;
}

/* Get collective circulation */
double egregore_get_circulation(Egregore* egr) {
    if (!egr) return 0.0;
    
    double total = 0.0;
    for (int i = 0; i < egr->n_morphules; i++) {
        total += morphule_get_vorticity(egr->morphules[i]);
    }
    
    return total;
}

/* Update collective state */
void egregore_update(Egregore* egr) {
    if (!egr) return;
    
    /* Update circulation */
    egr->circulation = egregore_get_circulation(egr);
    
    /* Update collective essence */
    egr->collective_essence = egregore_get_collective_essence(egr);
    
    /* Update coherence */
    double old_coherence = egr->identity.coherence;
    egr->identity.coherence = egregore_get_coherence(egr);
    
    if (fabs(egr->identity.coherence - old_coherence) > 0.01) {
        if (egr->on_coherence_change) {
            egr->on_coherence_change(egr, old_coherence, egr->identity.coherence);
        }
    }
    
    /* Update collective Matula number */
    /* TODO: Compute collective structure from morphule configurations */
    
    /* Check for collective transform */
    if (egr->swarm_transform && egregore_can_collective_transform(egr)) {
        egregore_collective_transform(egr);
    }
}

/* Get collective essence */
double egregore_get_collective_essence(Egregore* egr) {
    if (!egr) return 0.0;
    
    double total = 0.0;
    for (int i = 0; i < egr->n_morphules; i++) {
        Morphule* morph = egr->morphules[i];
        if (morph->quirk_type == QUIRK_TRANSFORM) {
            total += morph->quirk.transform.essence;
        }
    }
    
    return total;
}

/* Check if collective transform is possible */
bool egregore_can_collective_transform(Egregore* egr) {
    if (!egr || !egr->swarm_transform) return false;
    
    /* Check if ANY morphule can transform */
    for (int i = 0; i < egr->n_morphules; i++) {
        if (morphule_can_transform(egr->morphules[i])) {
            return true;
        }
    }
    
    return false;
}

/* Trigger collective transform */
int egregore_collective_transform(Egregore* egr) {
    if (!egr || !egregore_can_collective_transform(egr)) return -1;
    
    printf("=== COLLECTIVE TRANSFORM ===\n");
    printf("Egregore '%s' is transforming!\n", egr->name);
    
    /* Transform all morphules */
    for (int i = 0; i < egr->n_morphules; i++) {
        Morphule* morph = egr->morphules[i];
        if (morph->quirk_type == QUIRK_TRANSFORM) {
            /* Unlock all techniques for all morphules */
            TransformQuirk* quirk = &morph->quirk.transform;
            for (int j = 0; j < quirk->n_techniques; j++) {
                quirk->techniques[j].unlocked = true;
            }
            printf("  %s: All techniques unlocked! ♡\n", morph->name);
        }
    }
    
    /* Callback */
    if (egr->on_collective_transform) {
        egr->on_collective_transform(egr);
    }
    
    return 0;
}

/* Enable phase locking */
void egregore_enable_phase_locking(Egregore* egr) {
    if (!egr) return;
    
    egr->phase_locking = true;
    
    /* Allocate phases array if needed */
    if (!egr->phases && egr->n_morphules > 0) {
        egr->phases = zalloc(sizeof(double) * egr->n_morphules);
    }
}

/* Disable phase locking */
void egregore_disable_phase_locking(Egregore* egr) {
    if (!egr) return;
    egr->phase_locking = false;
}

/* Get phase coherence */
double egregore_get_coherence(Egregore* egr) {
    if (!egr || !egr->phase_locking || !egr->phases || egr->n_morphules < 2) {
        return 0.0;
    }
    
    /* Compute order parameter (Kuramoto model) */
    double sum_cos = 0.0;
    double sum_sin = 0.0;
    
    for (int i = 0; i < egr->n_morphules; i++) {
        sum_cos += cos(egr->phases[i]);
        sum_sin += sin(egr->phases[i]);
    }
    
    double r = sqrt(sum_cos * sum_cos + sum_sin * sum_sin) / egr->n_morphules;
    return r;  /* 0.0 = incoherent, 1.0 = fully synchronized */
}

/* Set interaction strength */
void egregore_set_interaction(Egregore* egr, int i, int j, double strength) {
    if (!egr || i < 0 || i >= egr->n_morphules || j < 0 || j >= egr->n_morphules) {
        return;
    }
    
    /* Allocate interaction matrix if needed */
    if (!egr->interactions) {
        egr->interactions = zalloc(sizeof(double*) * egr->n_morphules);
        for (int k = 0; k < egr->n_morphules; k++) {
            egr->interactions[k] = zalloc(sizeof(double) * egr->n_morphules);
        }
    }
    
    egr->interactions[i][j] = strength;
    egr->interactions[j][i] = strength;  /* Symmetric */
}

/* Enable knowledge sharing */
void egregore_enable_knowledge_sharing(Egregore* egr) {
    if (!egr) return;
    egr->knowledge_sharing = true;
}

/* Disable knowledge sharing */
void egregore_disable_knowledge_sharing(Egregore* egr) {
    if (!egr) return;
    egr->knowledge_sharing = false;
}

/* Share technique */
void egregore_share_technique(Egregore* egr, const char* technique_name, Morphule* source_morph) {
    if (!egr || !technique_name || !egr->knowledge_sharing) return;
    
    printf("Sharing technique '%s' across swarm...\n", technique_name);
    
    /* Add technique to all morphules */
    for (int i = 0; i < egr->n_morphules; i++) {
        Morphule* morph = egr->morphules[i];
        if (morph != source_morph && morph->quirk_type == QUIRK_TRANSFORM) {
            /* Find technique threshold from source */
            TransformQuirk* source_quirk = &source_morph->quirk.transform;
            double threshold = 0.0;
            for (int j = 0; j < source_quirk->n_techniques; j++) {
                if (strcmp(source_quirk->techniques[j].name, technique_name) == 0) {
                    threshold = source_quirk->techniques[j].threshold;
                    break;
                }
            }
            
            /* Add to target morphule */
            morphule_add_technique(morph, technique_name, threshold);
        }
    }
}

/* Evolve egregore dynamics */
void egregore_evolve(Egregore* egr, double dt) {
    if (!egr || !egr->active) return;
    
    /* Phase locking (Kuramoto model) */
    if (egr->phase_locking && egr->phases && egr->interactions) {
        double* phase_derivatives = zalloc(sizeof(double) * egr->n_morphules);
        
        for (int i = 0; i < egr->n_morphules; i++) {
            double sum = 0.0;
            for (int j = 0; j < egr->n_morphules; j++) {
                if (i != j) {
                    double coupling = egr->interactions[i][j];
                    sum += coupling * sin(egr->phases[j] - egr->phases[i]);
                }
            }
            phase_derivatives[i] = sum;
        }
        
        /* Update phases */
        for (int i = 0; i < egr->n_morphules; i++) {
            egr->phases[i] += phase_derivatives[i] * dt;
            /* Wrap to [0, 2π] */
            while (egr->phases[i] < 0.0) egr->phases[i] += 2.0 * M_PI;
            while (egr->phases[i] >= 2.0 * M_PI) egr->phases[i] -= 2.0 * M_PI;
        }
        
        free(phase_derivatives);
    }
    
    /* Update collective state */
    egregore_update(egr);
}

/* Compute collective flow field */
Vector egregore_flow(Egregore* egr, Point p) {
    Vector v = {0.0, 0.0};
    
    if (!egr || !egr->vortex) return v;
    
    /* Use shared vortex flow */
    v = vortex_flow(egr->vortex, p);
    
    /* Scale by circulation */
    v.x *= egr->circulation;
    v.y *= egr->circulation;
    
    return v;
}

/* Activate egregore */
int egregore_activate(Egregore* egr) {
    if (!egr) return -1;
    
    /* Activate all morphules */
    for (int i = 0; i < egr->n_morphules; i++) {
        morphule_activate(egr->morphules[i]);
    }
    
    egr->active = true;
    return 0;
}

/* Deactivate egregore */
void egregore_deactivate(Egregore* egr) {
    if (!egr) return;
    
    /* Deactivate all morphules */
    for (int i = 0; i < egr->n_morphules; i++) {
        morphule_deactivate(egr->morphules[i]);
    }
    
    egr->active = false;
}

/* Detect patterns */
Pattern** egregore_detect_patterns(Egregore* egr, int* n_patterns) {
    if (!egr || !n_patterns) return NULL;
    
    /* TODO: Implement pattern detection
     * - Analyze morphule states
     * - Find correlations
     * - Identify emergent behaviors
     */
    
    *n_patterns = 0;
    return NULL;
}

/* Print egregore state */
void egregore_print(Egregore* egr) {
    if (!egr) {
        printf("NULL egregore\n");
        return;
    }
    
    printf("Egregore: %s\n", egr->name);
    printf("  Active: %s\n", egr->active ? "yes" : "no");
    printf("  Identity: %s\n", egr->identity.description);
    printf("  Matula: %lu\n", egr->identity.matula);
    printf("  Coherence: %.2f\n", egr->identity.coherence);
    printf("  Circulation: %.2f\n", egr->circulation);
    printf("  Collective Essence: %.2f\n", egr->collective_essence);
    printf("  Morphules: %d\n", egr->n_morphules);
    
    for (int i = 0; i < egr->n_morphules; i++) {
        printf("    %d: %s", i, egr->morphules[i]->name);
        if (egr->phases) {
            printf(" (phase: %.2f)", egr->phases[i]);
        }
        printf("\n");
    }
    
    printf("  Phase Locking: %s\n", egr->phase_locking ? "enabled" : "disabled");
    printf("  Knowledge Sharing: %s\n", egr->knowledge_sharing ? "enabled" : "disabled");
    printf("  Swarm Transform: %s\n", egr->swarm_transform ? "enabled" : "disabled");
}

/* Get morphule by name */
Morphule* egregore_get_morphule(Egregore* egr, const char* name) {
    if (!egr || !name) return NULL;
    
    for (int i = 0; i < egr->n_morphules; i++) {
        if (strcmp(egr->morphules[i]->name, name) == 0) {
            return egr->morphules[i];
        }
    }
    
    return NULL;
}

/* Get morphule count */
int egregore_get_morphule_count(Egregore* egr) {
    if (!egr) return 0;
    return egr->n_morphules;
}

/* Pattern utilities */

void pattern_free(Pattern* pattern) {
    if (!pattern) return;
    if (pattern->participant_indices) {
        free(pattern->participant_indices);
    }
    free(pattern);
}

void patterns_free(Pattern** patterns, int n_patterns) {
    if (!patterns) return;
    for (int i = 0; i < n_patterns; i++) {
        pattern_free(patterns[i]);
    }
    free(patterns);
}

void pattern_print(Pattern* pattern) {
    if (!pattern) {
        printf("NULL pattern\n");
        return;
    }
    
    printf("Pattern: %s\n", pattern->name);
    printf("  Strength: %.2f\n", pattern->strength);
    printf("  Participants: %d\n", pattern->n_participants);
}

/* Stub implementations for serialization */

int egregore_save(Egregore* egr, const char* filename) {
    /* TODO: Serialize to .egregore file */
    (void)egr; (void)filename;
    return -1;
}

Egregore* egregore_load(const char* filename) {
    /* TODO: Deserialize from .egregore file */
    (void)filename;
    return NULL;
}

int egregore_visualize(Egregore* egr, const char* filename) {
    /* TODO: Generate visualization */
    (void)egr; (void)filename;
    return -1;
}
