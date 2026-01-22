#ifndef _EGREGORE_H_
#define _EGREGORE_H_
#include <stdint.h>
#include <stdbool.h>
#include "../vortex/vorticity.h"
#include "../morphule/morphule.h"
#define EGREGORE_MAX_MORPHULES 1000
typedef struct {
char description[256];
uint64_t matula;
double coherence;
} EgregoreIdentity;
typedef struct Egregore {
char name[64];
EgregoreIdentity identity;
VortexConfig* vortex;
Point center;
double circulation;
int n_morphules;
Morphule** morphules;
bool phase_locking;
double* phases;
bool knowledge_sharing;
double collective_essence;
bool swarm_transform;
bool swarm_techniques;
double** interactions;
bool active;
double energy;
void (*on_morphule_join)(struct Egregore* egr, Morphule* morph);
void (*on_morphule_leave)(struct Egregore* egr, Morphule* morph);
void (*on_collective_transform)(struct Egregore* egr);
void (*on_coherence_change)(struct Egregore* egr, double old_coherence, double new_coherence);
} Egregore;
typedef struct {
char name[64];
double strength;
int n_participants;
int* participant_indices;
} Pattern;
Egregore* egregore_create(const char* name, const char* identity_description, VortexConfig* vortex);
void egregore_free(Egregore* egr);
int egregore_summon(Egregore* egr, Morphule* morph);
int egregore_banish(Egregore* egr, Morphule* morph);
EgregoreIdentity egregore_get_identity(Egregore* egr);
double egregore_get_circulation(Egregore* egr);
void egregore_update(Egregore* egr);
Pattern** egregore_detect_patterns(Egregore* egr, int* n_patterns);
double egregore_get_collective_essence(Egregore* egr);
bool egregore_can_collective_transform(Egregore* egr);
int egregore_collective_transform(Egregore* egr);
void egregore_enable_phase_locking(Egregore* egr);
void egregore_disable_phase_locking(Egregore* egr);
double egregore_get_coherence(Egregore* egr);
void egregore_set_interaction(Egregore* egr, int i, int j, double strength);
void egregore_enable_knowledge_sharing(Egregore* egr);
void egregore_disable_knowledge_sharing(Egregore* egr);
void egregore_share_technique(Egregore* egr, const char* technique_name, Morphule* source_morph);
void egregore_evolve(Egregore* egr, double dt);
Vector egregore_flow(Egregore* egr, Point p);
int egregore_activate(Egregore* egr);
void egregore_deactivate(Egregore* egr);
int egregore_save(Egregore* egr, const char* filename);
Egregore* egregore_load(const char* filename);
void egregore_print(Egregore* egr);
int egregore_visualize(Egregore* egr, const char* filename);
Morphule* egregore_get_morphule(Egregore* egr, const char* name);
int egregore_get_morphule_count(Egregore* egr);
void pattern_free(Pattern* pattern);
void patterns_free(Pattern** patterns, int n_patterns);
void pattern_print(Pattern* pattern);
#endif