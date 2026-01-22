#ifndef _MORPHULE_H_
#define _MORPHULE_H_
#include <stdint.h>
#include <stdbool.h>
#include "../vortex/matula.h"
#include "../vortex/vorticity.h"
#define MORPHULE_MAX_CONSTRAINTS 10
typedef enum {
CONSTRAINT_IMMUTABLE,
CONSTRAINT_MINIMUM,
CONSTRAINT_MAXIMUM,
CONSTRAINT_RANGE,
} ConstraintType;
typedef struct {
char name[64];
ConstraintType type;
double value;
double min, max;
bool active;
} Constraint;
typedef enum {
QUIRK_TRANSFORM,
QUIRK_ADAPT,
QUIRK_EXPLORE,
QUIRK_OPTIMIZE,
QUIRK_CUSTOM,
} QuirkType;
typedef struct {
uint64_t target_matula;
uint64_t current_matula;
double essence;
int current_shell;
int max_shell;
int n_techniques;
struct {
char name[64];
double threshold;
bool unlocked;
void* data;
} *techniques;
} TransformQuirk;
typedef struct Morphule {
char name[64];
int n_constraints;
Constraint constraints[MORPHULE_MAX_CONSTRAINTS];
QuirkType quirk_type;
union {
TransformQuirk transform;
double adapt_param;
struct {
double x, y;
} explore;
struct {
double* params;
int n_params;
} optimize;
void* custom;
} quirk;
VortexConfig* vortex;
double vorticity;
bool active;
double energy;
void (*on_constraint_violation)(struct Morphule* morph, int constraint_idx);
void (*on_quirk_change)(struct Morphule* morph, double old_value, double new_value);
void (*on_transform)(struct Morphule* morph);
} Morphule;
Morphule* morphule_create(const char* name, Constraint* constraints, int n_constraints, QuirkType quirk_type);
void morphule_free(Morphule* morph);
bool morphule_check_constraints(Morphule* morph);
int morphule_set_quirk(Morphule* morph, double value);
double morphule_get_quirk(Morphule* morph);
int morphule_activate(Morphule* morph);
void morphule_deactivate(Morphule* morph);
Morphule* morphule_create_transform(const char* name);
double morphule_taste(Morphule* morph, TreeNode* system_tree);
bool morphule_can_transform(Morphule* morph);
int morphule_transform(Morphule* morph);
char** morphule_get_techniques(Morphule* morph, int* n_techniques);
int morphule_add_technique(Morphule* morph, const char* name, double threshold);
int morphule_execute_technique(Morphule* morph, const char* name, void* target);
Constraint constraint_immutable(const char* name, double value);
Constraint constraint_minimum(const char* name, double min);
Constraint constraint_maximum(const char* name, double max);
Constraint constraint_range(const char* name, double min, double max);
int morphule_attach_vortex(Morphule* morph, VortexConfig* vortex);
void morphule_detach_vortex(Morphule* morph);
double morphule_get_vorticity(Morphule* morph);
int morphule_save(Morphule* morph, const char* filename);
Morphule* morphule_load(const char* filename);
void morphule_print(Morphule* morph);
bool morphule_validate(Morphule* morph);
#endif