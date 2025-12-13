/**
 * Vorticity Implementation
 * 
 * "The vorticity alphabet - primitive operations from which computation emerges"
 */

#include "vorticity.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

/* A000081 sequence: Number of rooted trees with n nodes
 * https://oeis.org/A000081
 * 
 * 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, 4766, 12486, 32973, ...
 */
static const int A000081_SEQUENCE[] = {
    0,      /* n=0: undefined */
    1,      /* n=1: 1 tree (single node) */
    1,      /* n=2: 1 tree */
    2,      /* n=3: 2 trees */
    4,      /* n=4: 4 trees */
    9,      /* n=5: 9 trees */
    20,     /* n=6: 20 trees */
    48,     /* n=7: 48 trees */
    115,    /* n=8: 115 trees */
    286,    /* n=9: 286 trees */
    719,    /* n=10: 719 trees */
    1842,   /* n=11: 1842 trees */
    4766,   /* n=12: 4766 trees */
    12486,  /* n=13: 12486 trees */
    32973,  /* n=14: 32973 trees */
    87811,  /* n=15: 87811 trees */
    235381, /* n=16: 235381 trees */
    634847, /* n=17: 634847 trees */
    1721159,/* n=18: 1721159 trees */
    4688676,/* n=19: 4688676 trees */
    12826228/* n=20: 12826228 trees */
};

/* Helper: Allocate and zero memory */
static void* zalloc(size_t size) {
    void* ptr = malloc(size);
    if (ptr) {
        memset(ptr, 0, size);
    }
    return ptr;
}

/* Get A000081(n) */
int a000081(int n) {
    if (n < 0 || n > A000081_MAX_ORDER) {
        return 0;
    }
    return A000081_SEQUENCE[n];
}

/* Create vortex configuration */
static VortexConfig* vortex_create(void) {
    VortexConfig* vortex = zalloc(sizeof(VortexConfig));
    if (!vortex) return NULL;
    
    vortex->circulation = 1.0;
    vortex->phase = 0.0;
    
    return vortex;
}

/* Free vortex configuration */
void vortex_free(VortexConfig* vortex) {
    if (!vortex) return;
    
    if (vortex->tree) {
        tree_free(vortex->tree);
    }
    
    if (vortex->singularities) {
        free(vortex->singularities);
    }
    
    if (vortex->membranes) {
        free(vortex->membranes);
    }
    
    free(vortex);
}

/* Create vortex from tree */
VortexConfig* vortex_from_tree(TreeNode* tree) {
    if (!tree) return NULL;
    
    VortexConfig* vortex = vortex_create();
    if (!vortex) return NULL;
    
    vortex->tree = tree;
    vortex->matula = tree->matula;
    vortex->order = tree_depth(tree) + 1;
    vortex->index = 0;  /* TODO: Compute index from tree structure */
    
    /* Convert tree structure to singularities and membranes */
    int n_nodes = tree_node_count(tree);
    vortex->n_singularities = n_nodes;
    vortex->singularities = zalloc(sizeof(*vortex->singularities) * n_nodes);
    if (!vortex->singularities) {
        vortex_free(vortex);
        return NULL;
    }
    
    /* Place singularities in a circular pattern for now */
    /* TODO: Use force-directed layout for better visualization */
    for (int i = 0; i < n_nodes; i++) {
        double angle = 2.0 * M_PI * i / n_nodes;
        vortex->singularities[i].x = cos(angle);
        vortex->singularities[i].y = sin(angle);
        vortex->singularities[i].strength = 1.0;
        vortex->singularities[i].rotation = 1.0;  /* Counter-clockwise */
    }
    
    /* Create membranes from tree edges */
    /* TODO: Walk tree and create membrane connections */
    vortex->n_membranes = (n_nodes > 1) ? (n_nodes - 1) : 0;
    if (vortex->n_membranes > 0) {
        vortex->membranes = zalloc(sizeof(*vortex->membranes) * vortex->n_membranes);
        if (!vortex->membranes) {
            vortex_free(vortex);
            return NULL;
        }
        
        /* Simple linear chain for now */
        for (int i = 0; i < vortex->n_membranes; i++) {
            vortex->membranes[i].from = i;
            vortex->membranes[i].to = i + 1;
            vortex->membranes[i].flow = 1.0;
        }
    }
    
    return vortex;
}

/* Create vortex from Matula number */
VortexConfig* vortex_from_matula(uint64_t matula) {
    TreeNode* tree = tree_from_matula(matula);
    if (!tree) return NULL;
    
    return vortex_from_tree(tree);
}

/* Get specific vortex configuration */
VortexConfig* vortex_get(int order, int index) {
    int count = a000081(order);
    if (index < 0 || index >= count) {
        return NULL;
    }
    
    /* TODO: Generate the specific tree for this order and index
     * For now, just create a simple tree
     */
    
    TreeNode* tree = tree_create_empty();
    if (!tree) return NULL;
    
    /* Add children based on order */
    for (int i = 1; i < order; i++) {
        TreeNode* child = tree_create_empty();
        if (!child) {
            tree_free(tree);
            return NULL;
        }
        tree_add_child(tree, child);
    }
    
    return vortex_from_tree(tree);
}

/* Enumerate all vortices for order n */
VortexConfig** vortex_enumerate(int order, int* count) {
    int n = a000081(order);
    if (n == 0) {
        if (count) *count = 0;
        return NULL;
    }
    
    VortexConfig** vortices = malloc(sizeof(VortexConfig*) * n);
    if (!vortices) {
        if (count) *count = 0;
        return NULL;
    }
    
    /* Generate all vortices for this order */
    for (int i = 0; i < n; i++) {
        vortices[i] = vortex_get(order, i);
        if (!vortices[i]) {
            /* Cleanup on error */
            for (int j = 0; j < i; j++) {
                vortex_free(vortices[j]);
            }
            free(vortices);
            if (count) *count = 0;
            return NULL;
        }
    }
    
    if (count) *count = n;
    return vortices;
}

/* Compute flow field at point */
Vector vortex_flow(VortexConfig* vortex, Point p) {
    Vector v = {0.0, 0.0};
    
    if (!vortex || !vortex->singularities) {
        return v;
    }
    
    /* Superposition of all singularity flows */
    for (int i = 0; i < vortex->n_singularities; i++) {
        double dx = p.x - vortex->singularities[i].x;
        double dy = p.y - vortex->singularities[i].y;
        double r2 = dx*dx + dy*dy;
        
        if (r2 < 1e-10) continue;  /* Avoid singularity */
        
        double strength = vortex->singularities[i].strength;
        double rotation = vortex->singularities[i].rotation;
        
        /* Vortex flow: v = (strength / r^2) * (-y, x) * rotation */
        double factor = strength * rotation / r2;
        v.x += -dy * factor;
        v.y += dx * factor;
    }
    
    /* Scale by circulation */
    v.x *= vortex->circulation;
    v.y *= vortex->circulation;
    
    return v;
}

/* Compose two vortices */
VortexConfig* vortex_compose(VortexConfig* v1, VortexConfig* v2) {
    if (!v1 || !v2) return NULL;
    
    /* Compose Matula numbers */
    uint64_t composed_matula = matula_compose(v1->matula, v2->matula);
    
    return vortex_from_matula(composed_matula);
}

/* Check equivalence */
bool vortex_equivalent(VortexConfig* v1, VortexConfig* v2) {
    if (!v1 || !v2) return false;
    return matula_equal(v1->matula, v2->matula);
}

/* Get gradient order */
int gradient_order(VortexConfig* vortex) {
    if (!vortex || !vortex->tree) return 0;
    return tree_depth(vortex->tree);
}

/* Enumerate gradients */
VortexConfig** gradient_enumerate(int order, int* count) {
    /* Gradients of order n = all rooted trees with depth n */
    return vortex_enumerate(order, count);
}

/* Check Turing completeness */
bool vortex_is_turing_complete(int order) {
    /* Empirically, order >= 4 is sufficient for Turing completeness
     * (can encode AND, OR, NOT gates)
     */
    return order >= 4;
}

/* Print vortex configuration */
void vortex_print(VortexConfig* vortex) {
    if (!vortex) {
        printf("NULL vortex\n");
        return;
    }
    
    printf("Vortex Configuration:\n");
    printf("  Order: %d\n", vortex->order);
    printf("  Index: %d\n", vortex->index);
    printf("  Matula: %lu\n", vortex->matula);
    printf("  Circulation: %.2f\n", vortex->circulation);
    printf("  Phase: %.2f\n", vortex->phase);
    printf("  Singularities: %d\n", vortex->n_singularities);
    printf("  Membranes: %d\n", vortex->n_membranes);
    
    if (vortex->tree) {
        printf("  Tree structure:\n");
        tree_print(vortex->tree, 2);
    }
}

/* Membrane reservoir operations */

MembraneReservoir* membrane_create(int width, int height) {
    MembraneReservoir* membrane = zalloc(sizeof(MembraneReservoir));
    if (!membrane) return NULL;
    
    membrane->width = width;
    membrane->height = height;
    membrane->n_vortices = 0;
    membrane->vortices = NULL;
    membrane->interactions = NULL;
    
    return membrane;
}

int membrane_add_vortex(MembraneReservoir* membrane, VortexConfig* vortex, Point position) {
    if (!membrane || !vortex) return -1;
    
    /* Reallocate vortex array */
    VortexConfig** new_vortices = realloc(membrane->vortices,
        sizeof(VortexConfig*) * (membrane->n_vortices + 1));
    if (!new_vortices) return -1;
    
    membrane->vortices = new_vortices;
    membrane->vortices[membrane->n_vortices] = vortex;
    membrane->n_vortices++;
    
    /* TODO: Update interaction matrix */
    
    return 0;
}

Vector membrane_flow(MembraneReservoir* membrane, Point p) {
    Vector v = {0.0, 0.0};
    
    if (!membrane || !membrane->vortices) {
        return v;
    }
    
    /* Superposition of all vortex flows */
    for (int i = 0; i < membrane->n_vortices; i++) {
        Vector vi = vortex_flow(membrane->vortices[i], p);
        v.x += vi.x;
        v.y += vi.y;
    }
    
    return v;
}

void membrane_evolve(MembraneReservoir* membrane, double dt) {
    if (!membrane) return;
    
    /* TODO: Implement vortex dynamics
     * - Vortices move in the collective flow field
     * - Phase coupling between nearby vortices
     * - Conservation of circulation
     */
    (void)dt;
}

void membrane_free(MembraneReservoir* membrane) {
    if (!membrane) return;
    
    if (membrane->vortices) {
        for (int i = 0; i < membrane->n_vortices; i++) {
            vortex_free(membrane->vortices[i]);
        }
        free(membrane->vortices);
    }
    
    if (membrane->interactions) {
        for (int i = 0; i < membrane->n_vortices; i++) {
            free(membrane->interactions[i]);
        }
        free(membrane->interactions);
    }
    
    free(membrane);
}

/* Stub implementations for advanced features */

Vector** flow_field_grid(VortexConfig* vortex, 
                         double x_min, double x_max, int nx,
                         double y_min, double y_max, int ny) {
    /* TODO: Implement grid evaluation */
    (void)vortex; (void)x_min; (void)x_max; (void)nx;
    (void)y_min; (void)y_max; (void)ny;
    return NULL;
}

Point** flow_streamlines(VortexConfig* vortex, 
                         Point* seeds, int n_seeds,
                         double dt, int n_steps,
                         int* n_points) {
    /* TODO: Implement streamline tracing */
    (void)vortex; (void)seeds; (void)n_seeds;
    (void)dt; (void)n_steps; (void)n_points;
    return NULL;
}

double** flow_vorticity_field(VortexConfig* vortex,
                              double x_min, double x_max, int nx,
                              double y_min, double y_max, int ny) {
    /* TODO: Implement vorticity field computation */
    (void)vortex; (void)x_min; (void)x_max; (void)nx;
    (void)y_min; (void)y_max; (void)ny;
    return NULL;
}

VortexConfig* vortex_from_circuit(const char* circuit_spec) {
    /* TODO: Compile boolean circuit to vortex */
    (void)circuit_spec;
    return NULL;
}

int vortex_save(VortexConfig* vortex, const char* filename) {
    /* TODO: Serialize to .vort file */
    (void)vortex; (void)filename;
    return -1;
}

VortexConfig* vortex_load(const char* filename) {
    /* TODO: Deserialize from .vort file */
    (void)filename;
    return NULL;
}

int vortex_visualize(VortexConfig* vortex, const char* filename) {
    /* TODO: Generate visualization */
    (void)vortex; (void)filename;
    return -1;
}
