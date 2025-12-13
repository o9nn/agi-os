/**
 * Matula-Göbel Numbering for Structural Content Addressing
 * 
 * "The namespace IS the serialization format."
 * 
 * Establishes a bijection between rooted trees and positive integers,
 * enabling structural content addressing without hash collisions.
 * 
 * EVERY ROOTED TREE ↔ UNIQUE INTEGER
 * SAME STRUCTURE = SAME NUMBER = SAME THING
 * MATH DOESN'T NEED AGREEMENT. IT JUST IS.
 */

#ifndef _MATULA_H_
#define _MATULA_H_

#include <stdint.h>
#include <stdbool.h>

/* Maximum tree depth for Matula computation */
#define MATULA_MAX_DEPTH 64

/* Rooted tree structure */
typedef struct TreeNode {
    uint64_t matula;              /* Cached Matula number */
    int n_children;               /* Number of children */
    struct TreeNode** children;   /* Array of child pointers */
    void* data;                   /* User data */
} TreeNode;

/* Prime factorization */
typedef struct {
    int n_factors;                /* Number of prime factors */
    uint64_t* primes;             /* Prime factors */
    int* exponents;               /* Exponents */
} PrimeFactors;

/* Matula number operations */

/**
 * Compute Matula number from rooted tree
 * 
 * Algorithm:
 *   matula(empty) = 1
 *   matula(tree with children c1, c2, ..., cn) = 
 *       p1^matula(c1) × p2^matula(c2) × ... × pn^matula(cn)
 *   where p1, p2, ..., pn are the first n primes
 * 
 * Returns: Matula number, or 0 on error
 */
uint64_t matula_from_tree(TreeNode* tree);

/**
 * Compute rooted tree from Matula number
 * 
 * Algorithm:
 *   Factor matula into prime powers: p1^e1 × p2^e2 × ... × pn^en
 *   Create tree with n children
 *   Recursively compute child i from exponent ei
 * 
 * Returns: Tree root, or NULL on error
 */
TreeNode* tree_from_matula(uint64_t matula);

/**
 * Prime factorization of Matula number
 * 
 * Returns: Prime factors and exponents, or NULL on error
 */
PrimeFactors* factor_matula(uint64_t matula);

/**
 * Get children Matula numbers from parent
 * 
 * Extracts exponents from prime factorization.
 * 
 * Returns: Array of child Matula numbers, or NULL on error
 */
uint64_t* children_from_matula(uint64_t matula, int* n_children);

/**
 * Compose two trees by multiplication
 * 
 * Creates a new tree with two children: tree1 and tree2
 * matula(composed) = 2^matula(tree1) × 3^matula(tree2)
 * 
 * Returns: Matula number of composed tree
 */
uint64_t matula_compose(uint64_t matula1, uint64_t matula2);

/**
 * Compare two Matula numbers for equality
 * 
 * Returns: true if same structure, false otherwise
 */
bool matula_equal(uint64_t m1, uint64_t m2);

/* Tree operations */

/**
 * Create empty tree (Matula = 1)
 */
TreeNode* tree_create_empty(void);

/**
 * Create tree with children
 */
TreeNode* tree_create(TreeNode** children, int n_children);

/**
 * Add child to tree
 */
int tree_add_child(TreeNode* parent, TreeNode* child);

/**
 * Free tree recursively
 */
void tree_free(TreeNode* tree);

/**
 * Get tree depth
 */
int tree_depth(TreeNode* tree);

/**
 * Get tree node count
 */
int tree_node_count(TreeNode* tree);

/**
 * Serialize tree to string representation
 * 
 * Format: Newick notation
 * Example: ((A,B)C,(D,E)F)G
 */
char* tree_to_string(TreeNode* tree);

/**
 * Deserialize tree from string
 */
TreeNode* tree_from_string(const char* str);

/* Prime number utilities */

/**
 * Get nth prime (1-indexed)
 * 
 * prime_nth(1) = 2
 * prime_nth(2) = 3
 * prime_nth(3) = 5
 * ...
 */
uint64_t prime_nth(int n);

/**
 * Check if number is prime
 */
bool prime_is_prime(uint64_t n);

/**
 * Prime factorization
 */
PrimeFactors* prime_factor(uint64_t n);

/**
 * Free prime factors
 */
void prime_factors_free(PrimeFactors* factors);

/* Namespace operations */

/**
 * Compute Matula number from filesystem path
 * 
 * Walks the directory tree and computes Matula number
 * from the structure.
 * 
 * Returns: Matula number, or 0 on error
 */
uint64_t matula_from_path(const char* path);

/**
 * Get child paths from Matula number
 * 
 * Factors the Matula number and returns the names
 * of child directories/files.
 * 
 * Returns: Array of path strings, or NULL on error
 */
char** paths_from_matula(uint64_t matula, const char* base_path, int* n_paths);

/**
 * Verify two paths have same structure
 * 
 * Computes Matula numbers and compares.
 * O(1) comparison instead of O(n) tree walk.
 * 
 * Returns: true if same structure, false otherwise
 */
bool paths_same_structure(const char* path1, const char* path2);

/* Debugging and utilities */

/**
 * Print tree structure
 */
void tree_print(TreeNode* tree, int indent);

/**
 * Print Matula number in factored form
 * 
 * Example: 74207281 = 7 × 10601041
 */
void matula_print_factored(uint64_t matula);

/**
 * Validate Matula number
 * 
 * Checks if number is a valid Matula encoding
 * (i.e., can be factored into tree structure)
 */
bool matula_is_valid(uint64_t matula);

#endif /* _MATULA_H_ */
