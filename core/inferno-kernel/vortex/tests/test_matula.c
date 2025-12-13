/**
 * Matula number tests
 * 
 * Tests the bijection between rooted trees and integers.
 */

#include "../matula.h"
#include <stdio.h>
#include <assert.h>

void test_empty_tree() {
    printf("Test: Empty tree...\n");
    
    TreeNode* tree = tree_create_empty();
    assert(tree != NULL);
    assert(tree->matula == 1);
    assert(tree->n_children == 0);
    
    tree_free(tree);
    printf("  PASS\n");
}

void test_single_child() {
    printf("Test: Single child...\n");
    
    /* Tree with one child (empty tree)
     * Matula = 2^1 = 2
     */
    TreeNode* child = tree_create_empty();
    TreeNode* children[] = {child};
    TreeNode* tree = tree_create(children, 1);
    
    assert(tree != NULL);
    assert(tree->matula == 2);
    
    tree_free(tree);
    printf("  PASS\n");
}

void test_two_children() {
    printf("Test: Two identical children...\n");
    
    /* Tree with two children (both empty)
     * Matula = 2^1 × 3^1 = 6
     */
    TreeNode* child1 = tree_create_empty();
    TreeNode* child2 = tree_create_empty();
    TreeNode* children[] = {child1, child2};
    TreeNode* tree = tree_create(children, 2);
    
    assert(tree != NULL);
    assert(tree->matula == 6);
    
    tree_free(tree);
    printf("  PASS\n");
}

void test_nested_tree() {
    printf("Test: Nested tree...\n");
    
    /* Tree structure:
     *       •
     *      / \
     *     •   •
     *         |
     *         •
     * 
     * Child 1: empty (Matula = 1)
     * Child 2: single child (Matula = 2)
     * Parent: Matula = 2^1 × 3^2 = 2 × 9 = 18
     */
    TreeNode* leaf1 = tree_create_empty();
    TreeNode* leaf2 = tree_create_empty();
    TreeNode* children2[] = {leaf2};
    TreeNode* branch = tree_create(children2, 1);
    
    TreeNode* children[] = {leaf1, branch};
    TreeNode* tree = tree_create(children, 2);
    
    assert(tree != NULL);
    assert(branch->matula == 2);
    assert(tree->matula == 18);
    
    tree_free(tree);
    printf("  PASS\n");
}

void test_tree_from_matula() {
    printf("Test: Tree from Matula number...\n");
    
    /* Matula = 6 = 2 × 3
     * Should give tree with two children (both empty)
     */
    TreeNode* tree = tree_from_matula(6);
    assert(tree != NULL);
    assert(tree->matula == 6);
    assert(tree->n_children == 2);
    assert(tree->children[0]->matula == 1);
    assert(tree->children[1]->matula == 1);
    
    tree_free(tree);
    printf("  PASS\n");
}

void test_bijection() {
    printf("Test: Bijection (tree → matula → tree)...\n");
    
    /* Create a complex tree */
    TreeNode* leaf1 = tree_create_empty();
    TreeNode* leaf2 = tree_create_empty();
    TreeNode* leaf3 = tree_create_empty();
    
    TreeNode* children1[] = {leaf2};
    TreeNode* branch1 = tree_create(children1, 1);
    
    TreeNode* children[] = {leaf1, branch1, leaf3};
    TreeNode* tree1 = tree_create(children, 3);
    
    uint64_t matula = tree1->matula;
    printf("  Original Matula: %lu\n", matula);
    
    /* Reconstruct from Matula */
    TreeNode* tree2 = tree_from_matula(matula);
    assert(tree2 != NULL);
    assert(tree2->matula == matula);
    assert(tree2->n_children == tree1->n_children);
    
    tree_free(tree1);
    tree_free(tree2);
    printf("  PASS\n");
}

void test_prime_factorization() {
    printf("Test: Prime factorization...\n");
    
    /* 18 = 2 × 3^2 */
    PrimeFactors* factors = prime_factor(18);
    assert(factors != NULL);
    assert(factors->n_factors == 2);
    assert(factors->primes[0] == 2);
    assert(factors->exponents[0] == 1);
    assert(factors->primes[1] == 3);
    assert(factors->exponents[1] == 2);
    
    prime_factors_free(factors);
    printf("  PASS\n");
}

void test_composition() {
    printf("Test: Tree composition...\n");
    
    /* Compose two empty trees
     * matula(empty) = 1
     * matula(composed) = 2^1 × 3^1 = 6
     */
    uint64_t m1 = 1;
    uint64_t m2 = 1;
    uint64_t composed = matula_compose(m1, m2);
    
    assert(composed == 6);
    
    printf("  PASS\n");
}

void test_print_factored() {
    printf("Test: Print factored form...\n");
    
    printf("  ");
    matula_print_factored(1);
    printf("  ");
    matula_print_factored(2);
    printf("  ");
    matula_print_factored(6);
    printf("  ");
    matula_print_factored(18);
    
    printf("  PASS\n");
}

int main() {
    printf("=== Matula Number Tests ===\n\n");
    
    test_empty_tree();
    test_single_child();
    test_two_children();
    test_nested_tree();
    test_tree_from_matula();
    test_bijection();
    test_prime_factorization();
    test_composition();
    test_print_factored();
    
    printf("\n=== All tests passed! ===\n");
    return 0;
}
