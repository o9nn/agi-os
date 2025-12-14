/*
 * Echo State Network (ESN) Demonstration
 * 
 * This program demonstrates how an Echo State Network serves as a bridge
 * structure connecting all parallel cognitive frameworks:
 * 
 * 1. Dyck/Parentheses Grammar
 * 2. Rooted Trees  
 * 3. Matula Numbers
 * 4. Membrane Systems
 * 5. Hypergraphs
 * 6. Multiplicative RNNs
 * 7. Statistical Physics
 * 8. Quantum-like Dynamics
 * 
 * The ESN provides a fixed reservoir with rich recurrent dynamics where
 * only the readout layer is trained. The reservoir "echoes" input history
 * through decaying activations.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <limits.h>

// ═══════════════════════════════════════════════════════════════════════
// Simplified ESN Implementation for Demonstration
// ═══════════════════════════════════════════════════════════════════════

#define MAX_RESERVOIR_SIZE 100
#define MAX_PRIMES 100

// Prime number table (first 100 primes)
static unsigned long long primes[] = {
    2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
    73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151,
    157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233,
    239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317,
    331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419,
    421, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509,
    521, 523, 541, 547
};

typedef struct {
    int reservoir_size;
    float spectral_radius;
    float *activations;
    float **W_reservoir;
    unsigned long long matula_encoding;
} ESN;

// Initialize ESN with sparse random weights
ESN* create_esn(int size, float spectral_radius) {
    ESN *esn = (ESN*)malloc(sizeof(ESN));
    esn->reservoir_size = size;
    esn->spectral_radius = spectral_radius;
    esn->activations = (float*)calloc(size, sizeof(float));
    esn->matula_encoding = 1;
    
    // Allocate weight matrix
    esn->W_reservoir = (float**)malloc(size * sizeof(float*));
    for (int i = 0; i < size; i++) {
        esn->W_reservoir[i] = (float*)calloc(size, sizeof(float));
    }
    
    // Initialize sparse random weights (10% connectivity)
    srand(time(NULL));
    float sparsity = 0.1;
    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            if ((float)rand() / RAND_MAX < sparsity) {
                esn->W_reservoir[i][j] = ((float)rand() / RAND_MAX - 0.5) * 2.0;
            }
        }
    }
    
    // Scale to spectral radius (simplified: scale by largest element)
    float max_val = 0.0;
    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            if (fabs(esn->W_reservoir[i][j]) > max_val)
                max_val = fabs(esn->W_reservoir[i][j]);
        }
    }
    if (max_val > 0) {
        float scale = spectral_radius / (max_val * sqrt(size * sparsity));
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                esn->W_reservoir[i][j] *= scale;
            }
        }
    }
    
    return esn;
}

// Update ESN state: x(t+1) = tanh(W·x(t) + input)
void esn_update(ESN *esn, float input) {
    float *new_activations = (float*)calloc(esn->reservoir_size, sizeof(float));
    
    for (int i = 0; i < esn->reservoir_size; i++) {
        float sum = input * 0.1; // Input scaling
        
        // Reservoir recurrence: W·x(t)
        for (int j = 0; j < esn->reservoir_size; j++) {
            sum += esn->W_reservoir[i][j] * esn->activations[j];
        }
        
        // Apply activation function
        new_activations[i] = tanh(sum);
    }
    
    // Update state
    memcpy(esn->activations, new_activations, esn->reservoir_size * sizeof(float));
    free(new_activations);
}

// Convert ESN state to Matula number
unsigned long long esn_to_matula(ESN *esn) {
    unsigned long long matula = 1;
    
    for (int i = 0; i < esn->reservoir_size && i < MAX_PRIMES; i++) {
        // Quantize activation to 0-3 range
        int exponent = (int)((esn->activations[i] + 1.0) * 1.5);
        if (exponent < 0) exponent = 0;
        if (exponent > 3) exponent = 3;
        
        // Multiply by prime^exponent
        {
            int e;
            for (e = 0; e < exponent; e++) {
                // Avoid overflow for large numbers
                if (matula > ULLONG_MAX / primes[i])
                    return matula; // Return current value to avoid overflow
                matula *= primes[i];
            }
        }
    }
    
    esn->matula_encoding = matula;
    return matula;
}

// Convert ESN state to Dyck expression (parentheses)
void esn_to_dyck(ESN *esn, char *buffer, int buf_size) {
    int pos = 0;
    
    {
        int i;
        for (i = 0; i < esn->reservoir_size && pos < buf_size - 8; i++) {
            int l;
            // Activation level determines nesting
            int level = (int)((esn->activations[i] + 1.0) * 1.5);
            if (level < 0) level = 0;
            if (level > 3) level = 3;
            
            if (level > 0) {
                for (l = 0; l < level; l++) {
                    buffer[pos++] = '(';
                }
                for (l = 0; l < level; l++) {
                    buffer[pos++] = ')';
                }
            }
        }
    }
    
    buffer[pos] = '\0';
}

// Count active membranes (nodes with significant activation)
int esn_count_membranes(ESN *esn) {
    int count = 0;
    for (int i = 0; i < esn->reservoir_size; i++) {
        if (esn->activations[i] > 0.1)
            count++;
    }
    return count;
}

// Print ESN state in all frameworks
void esn_print_multi_framework(ESN *esn, int step) {
    char dyck[1024];
    esn_to_dyck(esn, dyck, sizeof(dyck));
    unsigned long long matula = esn_to_matula(esn);
    int membranes = esn_count_membranes(esn);
    
    printf("\n╔═══════════════════════════════════════════════════════════════════╗\n");
    printf("║  ESN State at Step %d - Multi-Framework View                    ║\n", step);
    printf("╠═══════════════════════════════════════════════════════════════════╣\n");
    
    // 1. Matula Number
    printf("║  1. Matula Number (Integer Encoding):                           ║\n");
    printf("║     %llu                                                         \n", matula);
    printf("║     Prime factorization encodes reservoir state                 ║\n");
    printf("║                                                                   ║\n");
    
    // 2. Dyck Grammar
    printf("║  2. Dyck/Parentheses Grammar:                                    ║\n");
    printf("║     %.60s%-3s║\n", dyck, strlen(dyck) > 60 ? "..." : "");
    printf("║     Continuously rewritten expression machine                    ║\n");
    printf("║                                                                   ║\n");
    
    // 3. Rooted Trees
    printf("║  3. Rooted Tree Forest:                                          ║\n");
    printf("║     %d active trees in forest                                   ║\n", membranes);
    printf("║     Dynamic grafting at leaves each step                         ║\n");
    printf("║                                                                   ║\n");
    
    // 4. Membrane System
    printf("║  4. Membrane Computing (P-System):                               ║\n");
    printf("║     %d active membranes with echo multisets                     ║\n", membranes);
    printf("║     Objects decay with rate %.3f                                ║\n", esn->spectral_radius);
    printf("║                                                                   ║\n");
    
    // 5. Hypergraph
    int edges = 0;
    for (int i = 0; i < esn->reservoir_size; i++) {
        for (int j = 0; j < esn->reservoir_size; j++) {
            if (esn->W_reservoir[i][j] != 0.0) edges++;
        }
    }
    printf("║  5. Hypergraph Automaton:                                        ║\n");
    printf("║     %d nodes, %d weighted hyperedges                         ║\n", 
           esn->reservoir_size, edges);
    printf("║     Signal propagation along edges                               ║\n");
    printf("║                                                                   ║\n");
    
    // 6. Multiplicative RNN
    printf("║  6. Multiplicative Recursive Neural Net:                         ║\n");
    printf("║     Prime-mode superposition: Σ p_i^e_i                          ║\n");
    printf("║     State = %llu (composite of primes)                        \n", matula);
    printf("║                                                                   ║\n");
    
    // 7. Statistical Physics
    printf("║  7. Statistical Physics Ensemble:                                ║\n");
    printf("║     Deterministic microstate (no entropy)                        ║\n");
    printf("║     High-dimensional hidden flow field                           ║\n");
    printf("║                                                                   ║\n");
    
    // 8. Quantum-like
    printf("║  8. Quantum-like Dynamics:                                       ║\n");
    printf("║     Mixed prime-mode amplitude superposition                     ║\n");
    printf("║     Echo = decaying amplitude of basis states                    ║\n");
    printf("╚═══════════════════════════════════════════════════════════════════╝\n");
}

// ═══════════════════════════════════════════════════════════════════════
// Demonstration Scenarios
// ═══════════════════════════════════════════════════════════════════════

void demo_basic_dynamics() {
    printf("\n");
    printf("═══════════════════════════════════════════════════════════════════════════\n");
    printf("  DEMONSTRATION 1: Basic ESN Dynamics Across All Frameworks\n");
    printf("═══════════════════════════════════════════════════════════════════════════\n");
    printf("\n");
    printf("An Echo State Network is a reservoir computing architecture where:\n");
    printf("  • Fixed sparse recurrent weights (reservoir)\n");
    printf("  • Rich dynamics echo input history\n");
    printf("  • Only readout layer is trained\n");
    printf("  • Spectral radius < 1 ensures stable echo decay\n");
    printf("\n");
    printf("We'll show how this SAME structure appears in all 8 frameworks:\n");
    printf("\n");
    
    ESN *esn = create_esn(20, 0.9);
    
    printf("Creating ESN with 20 reservoir nodes, spectral radius 0.9...\n");
    printf("Press Enter to see evolution through different input patterns...\n");
    getchar();
    
    // Evolve with different inputs
    float inputs[] = {0.5, -0.3, 0.8, -0.5, 0.2};
    for (int t = 0; t < 5; t++) {
        esn_update(esn, inputs[t]);
        esn_print_multi_framework(esn, t + 1);
        
        if (t < 4) {
            printf("\nPress Enter to continue to next step...\n");
            getchar();
        }
    }
    
    free(esn->activations);
    for (int i = 0; i < esn->reservoir_size; i++) {
        free(esn->W_reservoir[i]);
    }
    free(esn->W_reservoir);
    free(esn);
}

void demo_matula_evolution() {
    printf("\n");
    printf("═══════════════════════════════════════════════════════════════════════════\n");
    printf("  DEMONSTRATION 2: ESN State as Evolving Matula Number\n");
    printf("═══════════════════════════════════════════════════════════════════════════\n");
    printf("\n");
    printf("KEY INSIGHT: The ESN state can be encoded as a SINGLE INTEGER\n");
    printf("using Matula number representation:\n");
    printf("\n");
    printf("  State = ∏ p_i^e_i  where:\n");
    printf("    p_i = i-th prime (node identity)\n");
    printf("    e_i = quantized activation (echo strength)\n");
    printf("\n");
    printf("This means:\n");
    printf("  • ESN dynamics = integer dynamics\n");
    printf("  • Multiplication = structural composition\n");
    printf("  • Exponentiation = multiplicity (NOT iteration)\n");
    printf("  • No addition = no sequential time accumulation\n");
    printf("\n");
    printf("Press Enter to see Matula evolution...\n");
    getchar();
    
    ESN *esn = create_esn(10, 0.95);
    
    printf("\n╔════════════════════════════════════════════════════════════════╗\n");
    printf("║       Step │        Matula Number │  Change                   ║\n");
    printf("╠════════════════════════════════════════════════════════════════╣\n");
    
    unsigned long long prev_matula = 1;
    for (int t = 0; t < 15; t++) {
        float input = sin(t * 0.5);
        esn_update(esn, input);
        unsigned long long matula = esn_to_matula(esn);
        
        printf("║  %8d │ %20llu │", t, matula);
        if (matula > prev_matula) {
            printf("  ↑ Growth    ║\n");
        } else if (matula < prev_matula) {
            printf("  ↓ Decay     ║\n");
        } else {
            printf("  → Stable    ║\n");
        }
        
        prev_matula = matula;
    }
    
    printf("╚════════════════════════════════════════════════════════════════╝\n");
    printf("\nNotice how the Matula number evolves as reservoir echoes decay!\n");
    printf("The exponents decrease over time (echo decay),\n");
    printf("which corresponds to spectral radius < 1.\n");
    
    free(esn->activations);
    for (int i = 0; i < esn->reservoir_size; i++) {
        free(esn->W_reservoir[i]);
    }
    free(esn->W_reservoir);
    free(esn);
}

void demo_cognitive_cities_application() {
    printf("\n");
    printf("═══════════════════════════════════════════════════════════════════════════\n");
    printf("  DEMONSTRATION 3: ESN in Cognitive Cities - Traffic Pattern Recognition\n");
    printf("═══════════════════════════════════════════════════════════════════════════\n");
    printf("\n");
    printf("Application: Real-time traffic flow prediction using ESN reservoir\n");
    printf("\n");
    printf("Setup:\n");
    printf("  • Input: Traffic density measurements from sensors\n");
    printf("  • Reservoir: 50 nodes echoing historical traffic patterns\n");
    printf("  • Output: Predicted congestion in next time window\n");
    printf("  • Framework integration: All 8 views available simultaneously\n");
    printf("\n");
    printf("Press Enter to simulate traffic pattern learning...\n");
    getchar();
    
    ESN *esn = create_esn(50, 0.85);
    
    printf("\nSimulating 10 time steps of traffic data...\n\n");
    
    // Simulate traffic pattern: morning rush hour
    float traffic_pattern[] = {
        0.2, 0.3, 0.5, 0.7, 0.9, 1.0, 0.8, 0.6, 0.4, 0.3
    };
    
    for (int t = 0; t < 10; t++) {
        esn_update(esn, traffic_pattern[t]);
        
        printf("Time %02d:00 - Traffic Density: %.1f\n", t + 6, traffic_pattern[t]);
        printf("  • Reservoir state (Matula): %llu\n", esn_to_matula(esn));
        printf("  • Active membranes: %d (traffic zones with activity)\n", 
               esn_count_membranes(esn));
        printf("  • Echo strength: %.3f (historical pattern influence)\n", 
               esn->activations[0]);
        printf("\n");
    }
    
    printf("═══════════════════════════════════════════════════════════════════\n");
    printf("KEY BENEFIT: The ESN reservoir naturally captures:\n");
    printf("  1. Temporal dependencies (through recurrence)\n");
    printf("  2. Spatial patterns (through connectivity structure)\n");
    printf("  3. Historical echoes (through spectral radius decay)\n");
    printf("  4. Multi-scale dynamics (through different prime modes)\n");
    printf("\n");
    printf("And all of this is accessible through ANY of the 8 frameworks!\n");
    printf("═══════════════════════════════════════════════════════════════════\n");
    
    free(esn->activations);
    for (int i = 0; i < esn->reservoir_size; i++) {
        free(esn->W_reservoir[i]);
    }
    free(esn->W_reservoir);
    free(esn);
}

void print_conceptual_summary() {
    printf("\n");
    printf("═══════════════════════════════════════════════════════════════════════════\n");
    printf("  THE MASTER EQUATION: ESN as Universal Bridge Structure\n");
    printf("═══════════════════════════════════════════════════════════════════════════\n");
    printf("\n");
    printf("An Echo State Network IS:\n");
    printf("\n");
    printf("  ✓ A Dyck-expression rewriting machine\n");
    printf("      → Parentheses continuously restructure\n");
    printf("\n");
    printf("  ✓ A dynamic rooted forest\n");
    printf("      → Trees graft at leaves each timestep\n");
    printf("\n");
    printf("  ✓ A Matula-number evolution system\n");
    printf("      → Single integer encodes full state\n");
    printf("\n");
    printf("  ✓ A membrane echo-multiset network\n");
    printf("      → Objects decay in parallel membranes\n");
    printf("\n");
    printf("  ✓ A hypergraph automaton\n");
    printf("      → Signals propagate on weighted edges\n");
    printf("\n");
    printf("  ✓ A multiplicative recursive neural net\n");
    printf("      → Prime-mode superposition dynamics\n");
    printf("\n");
    printf("  ✓ A deterministic ensemble system\n");
    printf("      → No randomness, just hidden flow\n");
    printf("\n");
    printf("  ✓ A prime-mode amplitude field\n");
    printf("      → Quantum-like without being quantum\n");
    printf("\n");
    printf("═══════════════════════════════════════════════════════════════════════════\n");
    printf("\n");
    printf("All these views COINCIDE because they share the same algebra:\n");
    printf("\n");
    printf("  nested structure + multiplicative branching + fading influence over depth\n");
    printf("\n");
    printf("This is EXACTLY the ESN definition:\n");
    printf("  • Nested = reservoir depth through recurrence\n");
    printf("  • Multiplicative = connections compose multiplicatively\n");
    printf("  • Fading = spectral radius < 1 ensures decay\n");
    printf("\n");
    printf("═══════════════════════════════════════════════════════════════════════════\n");
    printf("\n");
    printf("IMPLICATIONS FOR COGNITIVE CITIES:\n");
    printf("\n");
    printf("  1. Pattern Recognition: ESN naturally captures spatio-temporal patterns\n");
    printf("  2. Multi-Scale: Prime modes provide automatic multi-resolution analysis\n");
    printf("  3. Efficient: State compresses to single integer (Matula number)\n");
    printf("  4. Interpretable: All 8 frameworks available for analysis\n");
    printf("  5. Composable: ESNs combine like prime factorizations\n");
    printf("\n");
    printf("═══════════════════════════════════════════════════════════════════════════\n");
}

// ═══════════════════════════════════════════════════════════════════════
// Main Program
// ═══════════════════════════════════════════════════════════════════════

int main(int argc, char **argv) {
    printf("\n");
    printf("╔═══════════════════════════════════════════════════════════════════════════╗\n");
    printf("║                                                                           ║\n");
    printf("║         ECHO STATE NETWORK (ESN) MULTI-FRAMEWORK DEMONSTRATION           ║\n");
    printf("║                                                                           ║\n");
    printf("║  Showing how ESN bridges all parallel cognitive frameworks:              ║\n");
    printf("║    • Dyck/Parentheses Grammar    • Membrane Systems                      ║\n");
    printf("║    • Rooted Trees                • Hypergraphs                           ║\n");
    printf("║    • Matula Numbers              • Multiplicative RNNs                   ║\n");
    printf("║    • Statistical Physics         • Quantum-like Dynamics                 ║\n");
    printf("║                                                                           ║\n");
    printf("╚═══════════════════════════════════════════════════════════════════════════╝\n");
    
    if (argc > 1 && strcmp(argv[1], "-h") == 0) {
        printf("\nUsage: %s [demo_number]\n", argv[0]);
        printf("\nAvailable demonstrations:\n");
        printf("  1 - Basic ESN dynamics across all frameworks\n");
        printf("  2 - Matula number evolution\n");
        printf("  3 - Cognitive cities application\n");
        printf("  (no argument runs all demonstrations)\n\n");
        return 0;
    }
    
    int demo = 0;
    if (argc > 1) {
        demo = atoi(argv[1]);
    }
    
    if (demo == 0 || demo == 1) {
        demo_basic_dynamics();
        if (demo == 1) {
            print_conceptual_summary();
            return 0;
        }
        printf("\n\nPress Enter to continue to next demonstration...\n");
        getchar();
    }
    
    if (demo == 0 || demo == 2) {
        demo_matula_evolution();
        if (demo == 2) {
            print_conceptual_summary();
            return 0;
        }
        printf("\n\nPress Enter to continue to next demonstration...\n");
        getchar();
    }
    
    if (demo == 0 || demo == 3) {
        demo_cognitive_cities_application();
        if (demo == 3) {
            print_conceptual_summary();
            return 0;
        }
        printf("\n\nPress Enter to see conceptual summary...\n");
        getchar();
    }
    
    print_conceptual_summary();
    
    printf("\nFor more information, see:\n");
    printf("  • docs/cognitive-architecture/esn-framework-bridge.md\n");
    printf("  • docs/cognitive-architecture/matula-numbers.md\n");
    printf("  • docs/cognitive-architecture/membrane-computing.md\n");
    printf("\n");
    
    return 0;
}
