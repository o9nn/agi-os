#ifndef _VORTICITY_H_
#define _VORTICITY_H_
#include <stdint.h>
#include <stdbool.h>
#include "matula.h"
#define A000081_MAX_ORDER 20
typedef struct {
int order;
int index;
uint64_t matula;
TreeNode* tree;
double circulation;
double phase;
int n_singularities;
struct {
double x, y;
double strength;
double rotation;
} *singularities;
int n_membranes;
struct {
int from, to;
double flow;
} *membranes;
} VortexConfig;
typedef struct {
double x, y;
} Vector;
typedef struct {
double x, y;
} Point;
int a000081(int n);
VortexConfig** vortex_enumerate(int order, int* count);
VortexConfig* vortex_get(int order, int index);
void vortex_free(VortexConfig* vortex);
VortexConfig* vortex_from_tree(TreeNode* tree);
VortexConfig* vortex_from_matula(uint64_t matula);
Vector vortex_flow(VortexConfig* vortex, Point p);
VortexConfig* vortex_compose(VortexConfig* v1, VortexConfig* v2);
bool vortex_equivalent(VortexConfig* v1, VortexConfig* v2);
Vector** flow_field_grid(VortexConfig* vortex,
double x_min, double x_max, int nx,
double y_min, double y_max, int ny);
Point** flow_streamlines(VortexConfig* vortex,
Point* seeds, int n_seeds,
double dt, int n_steps,
int* n_points);
double** flow_vorticity_field(VortexConfig* vortex,
double x_min, double x_max, int nx,
double y_min, double y_max, int ny);
int gradient_order(VortexConfig* vortex);
VortexConfig** gradient_enumerate(int order, int* count);
typedef struct {
int width, height;
VortexConfig** vortices;
int n_vortices;
double** interactions;
} MembraneReservoir;
MembraneReservoir* membrane_create(int width, int height);
int membrane_add_vortex(MembraneReservoir* membrane, VortexConfig* vortex, Point position);
Vector membrane_flow(MembraneReservoir* membrane, Point p);
void membrane_evolve(MembraneReservoir* membrane, double dt);
void membrane_free(MembraneReservoir* membrane);
bool vortex_is_turing_complete(int order);
VortexConfig* vortex_from_circuit(const char* circuit_spec);
void vortex_print(VortexConfig* vortex);
int vortex_save(VortexConfig* vortex, const char* filename);
VortexConfig* vortex_load(const char* filename);
int vortex_visualize(VortexConfig* vortex, const char* filename);
#endif