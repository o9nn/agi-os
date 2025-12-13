/**
 * Vorticity: A000081 Enumeration and Flow Composition
 * 
 * "Butcher trees ARE vortex configurations"
 * 
 * A000081: Number of unlabeled rooted trees with n nodes
 *          = Number of elementary differentials of order n
 *          = Number of topologically distinct vortex compositions
 * 
 * This implements the vorticity alphabet - the primitive operations
 * from which all computation emerges.
 */

#ifndef _VORTICITY_H_
#define _VORTICITY_H_

#include <stdint.h>
#include <stdbool.h>
#include "matula.h"

/* A000081 sequence (first 20 terms) */
#define A000081_MAX_ORDER 20

/* Vortex configuration */
typedef struct {
    int order;                    /* Differential order (tree depth) */
    int index;                    /* Index within order (0 to A000081(n)-1) */
    uint64_t matula;              /* Matula number encoding */
    TreeNode* tree;               /* Tree structure */
    
    /* Flow properties */
    double circulation;           /* Total circulation */
    double phase;                 /* Phase offset */
    
    /* Singularities (whirlpool centers) */
    int n_singularities;
    struct {
        double x, y;              /* Position */
        double strength;          /* Vortex strength */
        double rotation;          /* Clockwise or counter-clockwise */
    } *singularities;
    
    /* Membranes (connections between singularities) */
    int n_membranes;
    struct {
        int from, to;             /* Singularity indices */
        double flow;              /* Flow rate */
    } *membranes;
    
} VortexConfig;

/* Flow vector field */
typedef struct {
    double x, y;                  /* Vector components */
} Vector;

/* Point in 2D space */
typedef struct {
    double x, y;
} Point;

/* A000081 enumeration */

/**
 * Get A000081(n) - number of rooted trees with n nodes
 * 
 * Sequence: 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, ...
 * 
 * Returns: Count, or 0 if n out of range
 */
int a000081(int n);

/**
 * Get all vortex configurations for order n
 * 
 * Returns: Array of vortex configs, or NULL on error
 */
VortexConfig** vortex_enumerate(int order, int* count);

/**
 * Get specific vortex configuration
 * 
 * order: Differential order (tree depth)
 * index: Index within order (0 to A000081(n)-1)
 * 
 * Returns: Vortex config, or NULL on error
 */
VortexConfig* vortex_get(int order, int index);

/**
 * Free vortex configuration
 */
void vortex_free(VortexConfig* vortex);

/* Vortex operations */

/**
 * Create vortex from tree
 * 
 * Converts rooted tree into vortex configuration:
 * - Vertices → whirlpool centers (singularities)
 * - Edges → flow connections (membranes)
 * - Tree structure → circulation pattern
 */
VortexConfig* vortex_from_tree(TreeNode* tree);

/**
 * Create vortex from Matula number
 */
VortexConfig* vortex_from_matula(uint64_t matula);

/**
 * Compute flow field at point
 * 
 * Returns velocity vector at given point in space
 */
Vector vortex_flow(VortexConfig* vortex, Point p);

/**
 * Compose two vortices
 * 
 * Creates new vortex by combining two existing ones.
 * Composition follows tree multiplication:
 * matula(composed) = 2^matula(v1) × 3^matula(v2)
 */
VortexConfig* vortex_compose(VortexConfig* v1, VortexConfig* v2);

/**
 * Check if two vortices are equivalent
 * 
 * Two vortices are equivalent if they have the same Matula number
 * (i.e., same topological structure)
 */
bool vortex_equivalent(VortexConfig* v1, VortexConfig* v2);

/* Flow field operations */

/**
 * Evaluate flow field on grid
 * 
 * Computes velocity vectors at each point on a regular grid
 * 
 * Returns: 2D array of vectors, or NULL on error
 */
Vector** flow_field_grid(VortexConfig* vortex, 
                         double x_min, double x_max, int nx,
                         double y_min, double y_max, int ny);

/**
 * Compute streamlines
 * 
 * Traces paths that are tangent to the flow field
 * 
 * Returns: Array of point arrays, or NULL on error
 */
Point** flow_streamlines(VortexConfig* vortex, 
                         Point* seeds, int n_seeds,
                         double dt, int n_steps,
                         int* n_points);

/**
 * Compute vorticity field
 * 
 * Vorticity = curl of velocity field
 * 
 * Returns: 2D array of scalars, or NULL on error
 */
double** flow_vorticity_field(VortexConfig* vortex,
                              double x_min, double x_max, int nx,
                              double y_min, double y_max, int ny);

/* Gradient enumeration */

/**
 * Get gradient order
 * 
 * The "differential order" = depth of nesting
 * 
 * Returns: Order (tree height)
 */
int gradient_order(VortexConfig* vortex);

/**
 * Enumerate all gradients of order n
 * 
 * Returns all topologically distinct ways to compose
 * differential operators of order n
 */
VortexConfig** gradient_enumerate(int order, int* count);

/* Membrane reservoir operations */

/**
 * Create membrane reservoir
 * 
 * A membrane reservoir is a 2D surface where vortices
 * can interact without crossing (planarity constraint)
 */
typedef struct {
    int width, height;            /* Grid dimensions */
    VortexConfig** vortices;      /* Vortices on membrane */
    int n_vortices;
    
    /* Interaction matrix */
    double** interactions;        /* Phase coupling between vortices */
    
} MembraneReservoir;

/**
 * Create membrane reservoir
 */
MembraneReservoir* membrane_create(int width, int height);

/**
 * Add vortex to membrane
 */
int membrane_add_vortex(MembraneReservoir* membrane, VortexConfig* vortex, Point position);

/**
 * Compute collective flow field
 * 
 * Superposition of all vortex flows on the membrane
 */
Vector membrane_flow(MembraneReservoir* membrane, Point p);

/**
 * Evolve membrane dynamics
 * 
 * Time-step the vortex interactions
 */
void membrane_evolve(MembraneReservoir* membrane, double dt);

/**
 * Free membrane reservoir
 */
void membrane_free(MembraneReservoir* membrane);

/* Turing completeness */

/**
 * Check if vortex order is Turing complete
 * 
 * Returns true if order is high enough to simulate
 * arbitrary boolean circuits
 */
bool vortex_is_turing_complete(int order);

/**
 * Compile boolean circuit to vortex configuration
 * 
 * Maps logic gates to vortex compositions
 */
VortexConfig* vortex_from_circuit(const char* circuit_spec);

/* Utilities */

/**
 * Print vortex configuration
 */
void vortex_print(VortexConfig* vortex);

/**
 * Serialize vortex to .vort file
 */
int vortex_save(VortexConfig* vortex, const char* filename);

/**
 * Deserialize vortex from .vort file
 */
VortexConfig* vortex_load(const char* filename);

/**
 * Visualize vortex flow field
 * 
 * Generates ASCII art or image file
 */
int vortex_visualize(VortexConfig* vortex, const char* filename);

#endif /* _VORTICITY_H_ */
