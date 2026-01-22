#include "matula.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
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
static void* zalloc(size_t size) {
void* ptr = malloc(size);
if (ptr) {
memset(ptr, 0, size);
}
return ptr;
}
uint64_t prime_nth(int n) {
if (n < 1 || n > N_PRIMES) {
return 0;
}
return PRIMES[n - 1];
}
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
PrimeFactors* prime_factor(uint64_t n) {
if (n == 0) return NULL;
if (n == 1) {
PrimeFactors* factors = zalloc(sizeof(PrimeFactors));
factors->n_factors = 0;
return factors;
}
PrimeFactors* factors = zalloc(sizeof(PrimeFactors));
if (!factors) return NULL;
factors->primes = malloc(sizeof(uint64_t) * 64);
factors->exponents = malloc(sizeof(int) * 64);
if (!factors->primes || !factors->exponents) {
prime_factors_free(factors);
return NULL;
}
factors->n_factors = 0;
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
if (n > 1) {
factors->primes[factors->n_factors] = n;
factors->exponents[factors->n_factors] = 1;
factors->n_factors++;
}
return factors;
}
void prime_factors_free(PrimeFactors* factors) {
if (!factors) return;
if (factors->primes) free(factors->primes);
if (factors->exponents) free(factors->exponents);
free(factors);
}
static uint64_t power_checked(uint64_t base, int exp) {
if (exp == 0) return 1;
if (exp == 1) return base;
uint64_t result = 1;
for (int i = 0; i < exp; i++) {
if (result > UINT64_MAX / base) {
return 0;
}
result *= base;
}
return result;
}
TreeNode* tree_create_empty(void) {
TreeNode* tree = zalloc(sizeof(TreeNode));
if (!tree) return NULL;
tree->matula = 1;
tree->n_children = 0;
tree->children = NULL;
tree->data = NULL;
return tree;
}
TreeNode* tree_create(TreeNode** children, int n_children) {
TreeNode* tree = zalloc(sizeof(TreeNode));
if (!tree) return NULL;
tree->n_children = n_children;
tree->children = malloc(sizeof(TreeNode*) * n_children);
if (!tree->children && n_children > 0) {
free(tree);
return NULL;
}
for (int i = 0; i < n_children; i++) {
tree->children[i] = children[i];
}
tree->matula = matula_from_tree(tree);
return tree;
}
int tree_add_child(TreeNode* parent, TreeNode* child) {
if (!parent || !child) return -1;
TreeNode** new_children = realloc(parent->children,
sizeof(TreeNode*) * (parent->n_children + 1));
if (!new_children) return -1;
parent->children = new_children;
parent->children[parent->n_children] = child;
parent->n_children++;
parent->matula = matula_from_tree(parent);
return 0;
}
void tree_free(TreeNode* tree) {
if (!tree) return;
for (int i = 0; i < tree->n_children; i++) {
tree_free(tree->children[i]);
}
if (tree->children) free(tree->children);
free(tree);
}
int tree_depth(TreeNode* tree) {
if (!tree || tree->n_children == 0) return 0;
int max_depth = 0;
for (int i = 0; i < tree->n_children; i++) {
int depth = tree_depth(tree->children[i]);
if (depth > max_depth) max_depth = depth;
}
return max_depth + 1;
}
int tree_node_count(TreeNode* tree) {
if (!tree) return 0;
int count = 1;
for (int i = 0; i < tree->n_children; i++) {
count += tree_node_count(tree->children[i]);
}
return count;
}
uint64_t matula_from_tree(TreeNode* tree) {
if (!tree) return 0;
if (tree->n_children == 0) {
return 1;
}
uint64_t matula = 1;
for (int i = 0; i < tree->n_children; i++) {
uint64_t prime = prime_nth(i + 1);
if (prime == 0) {
return 0;
}
uint64_t child_matula = matula_from_tree(tree->children[i]);
if (child_matula == 0) return 0;
uint64_t power = power_checked(prime, child_matula);
if (power == 0) {
return 0;
}
if (matula > UINT64_MAX / power) {
return 0;
}
matula *= power;
}
return matula;
}
TreeNode* tree_from_matula(uint64_t matula) {
if (matula == 0) return NULL;
if (matula == 1) {
return tree_create_empty();
}
PrimeFactors* factors = prime_factor(matula);
if (!factors) return NULL;
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
PrimeFactors* factor_matula(uint64_t matula) {
return prime_factor(matula);
}
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
uint64_t matula_compose(uint64_t matula1, uint64_t matula2) {
uint64_t power1 = power_checked(2, matula1);
if (power1 == 0) return 0;
uint64_t power2 = power_checked(3, matula2);
if (power2 == 0) return 0;
if (power1 > UINT64_MAX / power2) return 0;
return power1 * power2;
}
bool matula_equal(uint64_t m1, uint64_t m2) {
return m1 == m2;
}
void tree_print(TreeNode* tree, int indent) {
if (!tree) return;
for (int i = 0; i < indent; i++) printf("  ");
printf("Node (Matula: %lu, children: %d)\n", tree->matula, tree->n_children);
for (int i = 0; i < tree->n_children; i++) {
tree_print(tree->children[i], indent + 1);
}
}
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
bool matula_is_valid(uint64_t matula) {
if (matula == 0) return false;
if (matula == 1) return true;
PrimeFactors* factors = factor_matula(matula);
if (!factors) return false;
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
uint64_t matula_from_path(const char* path) {
(void)path;
return 0;
}
char** paths_from_matula(uint64_t matula, const char* base_path, int* n_paths) {
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
char* tree_to_string(TreeNode* tree) {
(void)tree;
return NULL;
}
TreeNode* tree_from_string(const char* str) {
(void)str;
return NULL;
}