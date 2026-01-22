#ifndef _MATULA_H_
#define _MATULA_H_
#include <stdint.h>
#include <stdbool.h>
#define MATULA_MAX_DEPTH 64
typedef struct TreeNode {
uint64_t matula;
int n_children;
struct TreeNode** children;
void* data;
} TreeNode;
typedef struct {
int n_factors;
uint64_t* primes;
int* exponents;
} PrimeFactors;
uint64_t matula_from_tree(TreeNode* tree);
TreeNode* tree_from_matula(uint64_t matula);
PrimeFactors* factor_matula(uint64_t matula);
uint64_t* children_from_matula(uint64_t matula, int* n_children);
uint64_t matula_compose(uint64_t matula1, uint64_t matula2);
bool matula_equal(uint64_t m1, uint64_t m2);
TreeNode* tree_create_empty(void);
TreeNode* tree_create(TreeNode** children, int n_children);
int tree_add_child(TreeNode* parent, TreeNode* child);
void tree_free(TreeNode* tree);
int tree_depth(TreeNode* tree);
int tree_node_count(TreeNode* tree);
char* tree_to_string(TreeNode* tree);
TreeNode* tree_from_string(const char* str);
uint64_t prime_nth(int n);
bool prime_is_prime(uint64_t n);
PrimeFactors* prime_factor(uint64_t n);
void prime_factors_free(PrimeFactors* factors);
uint64_t matula_from_path(const char* path);
char** paths_from_matula(uint64_t matula, const char* base_path, int* n_paths);
bool paths_same_structure(const char* path1, const char* path2);
void tree_print(TreeNode* tree, int indent);
void matula_print_factored(uint64_t matula);
bool matula_is_valid(uint64_t matula);
#endif