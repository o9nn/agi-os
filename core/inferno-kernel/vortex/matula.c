/**
 * Matula-Göbel Numbering Implementation
 * 
 * "SAME STRUCTURE = SAME NUMBER = SAME THING"
 */

#include "matula.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

/* Prime number table (first 100 primes) */
static const uint64_t PRIMES[] = {
    2, 3, 5, 7, 11, 13, 17, 19, 23, 29,
    31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
    73, 79, 83, 89, 97, 101, 103, 107, 109, 113,
    127, 131, 137, 139, 149, 151, 157, 163, 167, 173,
    179, 181, 191, 193, 197, 199, 211, 223, 227, 229,
    233, 239, 241, 251, 257, 263, 269, 271, 277, 281,
    283, 293, 307, 311, 313, 317, 331, 337, 347, 349,
    353, 359, 367, 373, 379, 383, 389, 397, 401, 409,
    419, 421, 431, 433, 439, 443, 449, 457, 461, 463,
    467, 479, 487, 491, 499, 503, 509, 521, 523, 541
};

#define N_PRIMES (sizeof(PRIMES) / sizeof(PRIMES[0]))

/* Helper: Allocate and zero memory */
static void* zalloc(size_t size) {
    void* ptr = malloc(size);
    if (ptr) {
        memset(ptr, 0, size);
    }
    return ptr;
}

/* Get nth prime (1-indexed) */
uint64_t prime_nth(int n) {
    if (n < 1 || n > N_PRIMES) {
        return 0;  /* Out of range */
    }
    return PRIMES[n - 1];
}

/* Check if number is prime */
bool prime_is_prime(uint64_t n) {
    if (n < 2) return false;
    if (n == 2) return true;
    if (n % 2 == 0) return false;
    
    uint64_t sqrt_n = (uint64_t)sqrt((double)n);
    for (uint64_t i = 3; i <= sqrt_n; i += 2) {
        if (n % i == 0) return false;
    }
    return true;
}

/* Prime factorization */
PrimeFactors* prime_factor(uint64_t n) {
    if (n == 0) return NULL;
    if (n == 1) {
        /* Special case: 1 has no prime factors */
        PrimeFactors* factors = zalloc(sizeof(PrimeFactors));
        factors->n_factors = 0;
        return factors;
    }
    
    PrimeFactors* factors = zalloc(sizeof(PrimeFactors));
    if (!factors) return NULL;
    
    /* Allocate arrays (max possible factors) */
    factors->primes = malloc(sizeof(uint64_t) * 64);
    factors->exponents = malloc(sizeof(int) * 64);
    if (!factors->primes || !factors->exponents) {
        prime_factors_free(factors);
        return NULL;
    }
    
    factors->n_factors = 0;
    
    /* Factor out each prime */
    for (int i = 0; i < N_PRIMES && n > 1; i++) {
        uint64_t p = PRIMES[i];
        if (n % p == 0) {
            int exp = 0;
            while (n % p == 0) {
                exp++;
                n /= p;
            }
            factors->primes[factors->n_factors] = p;
            factors->exponents[factors->n_factors] = exp;
            factors->n_factors++;
        }
    }
    
    /* If n > 1, there's a large prime factor we can't handle */
    if (n > 1) {
        /* Store it as-is */
        factors->primes[factors->n_factors] = n;
        factors->exponents[factors->n_factors] = 1;
        factors->n_factors++;
    }
    
    return factors;
}

/* Free prime factors */
void prime_factors_free(PrimeFactors* factors) {
    if (!factors) return;
    if (factors->primes) free(factors->primes);
    if (factors->exponents) free(factors->exponents);
    free(factors);
}

/* Compute power (base^exp) with overflow check */
static uint64_t power_checked(uint64_t base, int exp) {
    if (exp == 0) return 1;
    if (exp == 1) return base;
    
    uint64_t result = 1;
    for (int i = 0; i < exp; i++) {
        /* Check for overflow */
        if (result > UINT64_MAX / base) {
            return 0;  /* Overflow */
        }
        result *= base;
    }
    return result;
}

/* Create empty tree */
TreeNode* tree_create_empty(void) {
    TreeNode* tree = zalloc(sizeof(TreeNode));
    if (!tree) return NULL;
    
    tree->matula = 1;  /* Empty tree has Matula number 1 */
    tree->n_children = 0;
    tree->children = NULL;
    tree->data = NULL;
    
    return tree;
}

/* Create tree with children */
TreeNode* tree_create(TreeNode** children, int n_children) {
    TreeNode* tree = zalloc(sizeof(TreeNode));
    if (!tree) return NULL;
    
    tree->n_children = n_children;
    tree->children = malloc(sizeof(TreeNode*) * n_children);
    if (!tree->children && n_children > 0) {
        free(tree);
        return NULL;
    }
    
    /* Copy children */
    for (int i = 0; i < n_children; i++) {
        tree->children[i] = children[i];
    }
    
    /* Compute Matula number */
    tree->matula = matula_from_tree(tree);
    
    return tree;
}

/* Add child to tree */
int tree_add_child(TreeNode* parent, TreeNode* child) {
    if (!parent || !child) return -1;
    
    /* Reallocate children array */
    TreeNode** new_children = realloc(parent->children,
        sizeof(TreeNode*) * (parent->n_children + 1));
    if (!new_children) return -1;
    
    parent->children = new_children;
    parent->children[parent->n_children] = child;
    parent->n_children++;
    
    /* Recompute Matula number */
    parent->matula = matula_from_tree(parent);
    
    return 0;
}

/* Free tree recursively */
void tree_free(TreeNode* tree) {
    if (!tree) return;
    
    /* Free children recursively */
    for (int i = 0; i < tree->n_children; i++) {
        tree_free(tree->children[i]);
    }
    
    if (tree->children) free(tree->children);
    free(tree);
}

/* Get tree depth */
int tree_depth(TreeNode* tree) {
    if (!tree || tree->n_children == 0) return 0;
    
    int max_depth = 0;
    for (int i = 0; i < tree->n_children; i++) {
        int depth = tree_depth(tree->children[i]);
        if (depth > max_depth) max_depth = depth;
    }
    
    return max_depth + 1;
}

/* Get tree node count */
int tree_node_count(TreeNode* tree) {
    if (!tree) return 0;
    
    int count = 1;  /* This node */
    for (int i = 0; i < tree->n_children; i++) {
        count += tree_node_count(tree->children[i]);
    }
    
    return count;
}

/* Compute Matula number from rooted tree */
uint64_t matula_from_tree(TreeNode* tree) {
    if (!tree) return 0;
    
    /* Empty tree (no children) has Matula number 1 */
    if (tree->n_children == 0) {
        return 1;
    }
    
    /* Tree with children: matula = p1^m1 × p2^m2 × ... × pn^mn
     * where pi is the i-th prime and mi is the Matula number of the i-th child
     */
    uint64_t matula = 1;
    
    for (int i = 0; i < tree->n_children; i++) {
        /* Get i-th prime */
        uint64_t prime = prime_nth(i + 1);
        if (prime == 0) {
            /* Out of primes */
            return 0;
        }
        
        /* Get Matula number of child */
        uint64_t child_matula = matula_from_tree(tree->children[i]);
        if (child_matula == 0) return 0;
        
        /* Compute prime^child_matula */
        uint64_t power = power_checked(prime, child_matula);
        if (power == 0) {
            /* Overflow */
            return 0;
        }
        
        /* Multiply into result */
        if (matula > UINT64_MAX / power) {
            /* Overflow */
            return 0;
        }
        matula *= power;
    }
    
    return matula;
}

/* Compute rooted tree from Matula number */
TreeNode* tree_from_matula(uint64_t matula) {
    if (matula == 0) return NULL;
    
    /* Matula number 1 = empty tree */
    if (matula == 1) {
        return tree_create_empty();
    }
    
    /* Factor the Matula number */
    PrimeFactors* factors = prime_factor(matula);
    if (!factors) return NULL;
    
    /* Create tree with n_factors children */
    TreeNode* tree = zalloc(sizeof(TreeNode));
    if (!tree) {
        prime_factors_free(factors);
        return NULL;
    }
    
    tree->matula = matula;
    tree->n_children = factors->n_factors;
    tree->children = malloc(sizeof(TreeNode*) * factors->n_factors);
    if (!tree->children && factors->n_factors > 0) {
        free(tree);
        prime_factors_free(factors);
        return NULL;
    }
    
    /* Recursively create children from exponents */
    for (int i = 0; i < factors->n_factors; i++) {
        tree->children[i] = tree_from_matula(factors->exponents[i]);
        if (!tree->children[i]) {
            tree_free(tree);
            prime_factors_free(factors);
            return NULL;
        }
    }
    
    prime_factors_free(factors);
    return tree;
}

/* Prime factorization of Matula number */
PrimeFactors* factor_matula(uint64_t matula) {
    return prime_factor(matula);
}

/* Get children Matula numbers from parent */
uint64_t* children_from_matula(uint64_t matula, int* n_children) {
    if (!n_children) return NULL;
    
    PrimeFactors* factors = factor_matula(matula);
    if (!factors) {
        *n_children = 0;
        return NULL;
    }
    
    *n_children = factors->n_factors;
    
    if (factors->n_factors == 0) {
        prime_factors_free(factors);
        return NULL;
    }
    
    /* Copy exponents (which are the child Matula numbers) */
    uint64_t* children = malloc(sizeof(uint64_t) * factors->n_factors);
    if (!children) {
        prime_factors_free(factors);
        *n_children = 0;
        return NULL;
    }
    
    for (int i = 0; i < factors->n_factors; i++) {
        children[i] = factors->exponents[i];
    }
    
    prime_factors_free(factors);
    return children;
}

/* Compose two trees */
uint64_t matula_compose(uint64_t matula1, uint64_t matula2) {
    /* Composed tree has two children with Matula numbers matula1 and matula2
     * matula(composed) = 2^matula1 × 3^matula2
     */
    
    uint64_t power1 = power_checked(2, matula1);
    if (power1 == 0) return 0;  /* Overflow */
    
    uint64_t power2 = power_checked(3, matula2);
    if (power2 == 0) return 0;  /* Overflow */
    
    if (power1 > UINT64_MAX / power2) return 0;  /* Overflow */
    
    return power1 * power2;
}

/* Compare two Matula numbers */
bool matula_equal(uint64_t m1, uint64_t m2) {
    return m1 == m2;
}

/* Print tree structure */
void tree_print(TreeNode* tree, int indent) {
    if (!tree) return;
    
    for (int i = 0; i < indent; i++) printf("  ");
    printf("Node (Matula: %lu, children: %d)\n", tree->matula, tree->n_children);
    
    for (int i = 0; i < tree->n_children; i++) {
        tree_print(tree->children[i], indent + 1);
    }
}

/* Print Matula number in factored form */
void matula_print_factored(uint64_t matula) {
    if (matula == 1) {
        printf("1 (empty tree)\n");
        return;
    }
    
    PrimeFactors* factors = factor_matula(matula);
    if (!factors) {
        printf("Error factoring %lu\n", matula);
        return;
    }
    
    printf("%lu = ", matula);
    for (int i = 0; i < factors->n_factors; i++) {
        if (i > 0) printf(" × ");
        printf("%lu", factors->primes[i]);
        if (factors->exponents[i] > 1) {
            printf("^%d", factors->exponents[i]);
        }
    }
    printf("\n");
    
    prime_factors_free(factors);
}

/* Validate Matula number */
bool matula_is_valid(uint64_t matula) {
    if (matula == 0) return false;
    if (matula == 1) return true;  /* Empty tree */
    
    /* Try to factor and reconstruct */
    PrimeFactors* factors = factor_matula(matula);
    if (!factors) return false;
    
    /* Check that all prime factors are in our table */
    for (int i = 0; i < factors->n_factors; i++) {
        bool found = false;
        for (int j = 0; j < N_PRIMES; j++) {
            if (factors->primes[i] == PRIMES[j]) {
                found = true;
                break;
            }
        }
        if (!found) {
            prime_factors_free(factors);
            return false;
        }
    }
    
    prime_factors_free(factors);
    return true;
}

/* Stub implementations for namespace operations (to be completed) */

uint64_t matula_from_path(const char* path) {
    /* TODO: Walk directory tree and compute Matula number */
    (void)path;
    return 0;
}

char** paths_from_matula(uint64_t matula, const char* base_path, int* n_paths) {
    /* TODO: Factor Matula and return child paths */
    (void)matula;
    (void)base_path;
    (void)n_paths;
    return NULL;
}

bool paths_same_structure(const char* path1, const char* path2) {
    uint64_t m1 = matula_from_path(path1);
    uint64_t m2 = matula_from_path(path2);
    return matula_equal(m1, m2);
}

/* Stub implementations for serialization (to be completed) */

char* tree_to_string(TreeNode* tree) {
    /* TODO: Serialize to Newick notation */
    (void)tree;
    return NULL;
}

TreeNode* tree_from_string(const char* str) {
    /* TODO: Deserialize from Newick notation */
    (void)str;
    return NULL;
}
