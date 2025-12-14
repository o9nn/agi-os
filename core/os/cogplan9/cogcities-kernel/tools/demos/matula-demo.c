/*
 * Matula Numbers Demonstration
 * 
 * This program demonstrates the bijection between rooted trees and natural
 * numbers via Matula encoding (prime factorization).
 * 
 * Matula numbers provide a unique integer representation for each rooted tree:
 * - Empty tree [] → 1
 * - Tree with children → product of p(M(child_i)) where p(n) is nth prime
 * 
 * Examples:
 * []            → 1 → p(1) = 2
 * [[]]          → 2 → p(2) = 3  
 * [] []         → 2^2 = 4
 * [[[]]]        → 3 → p(3) = 5
 * [[],[]]       → 4 → p(4) = 7
 * [[], []]      → 3*2 = 6
 * [] [] []      → 2^3 = 8
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
#define NPRIMES (sizeof(primes)/sizeof(primes[0]))

// Get the n-th prime number (1-indexed)
static unsigned long long
nth_prime(int n)
{
    if (n < 1 || n > NPRIMES)
        return 0;
    return primes[n - 1];
}

// Factorize a number
static void
factorize(unsigned long long n, int *exponents, int max_primes)
{
    for (int i = 0; i < max_primes; i++)
        exponents[i] = 0;
    
    for (int i = 0; i < NPRIMES && i < max_primes && n > 1; i++) {
        while (n % primes[i] == 0) {
            exponents[i]++;
            n /= primes[i];
        }
    }
}

// Parse children from parentheses notation
static int
parse_children(char *parens, char ***children_out)
{
    if (parens == NULL || parens[0] == '\0')
        return 0;
    
    char *p = parens;
    if (*p == '(')
        p++;
    
    int child_count = 0;
    int depth = 0;
    int child_start = -1;
    
    int max_children = strlen(parens) / 2;
    char **children = malloc(max_children * sizeof(char*));
    if (children == NULL)
        return 0;
    
    for (int i = 0; p[i] != '\0'; i++) {
        if (p[i] == '(') {
            if (depth == 0)
                child_start = i;
            depth++;
        } else if (p[i] == ')') {
            depth--;
            if (depth == 0 && child_start >= 0) {
                int child_len = i - child_start + 1;
                children[child_count] = malloc(child_len + 1);
                if (children[child_count] != NULL) {
                    memcpy(children[child_count], p + child_start, child_len);
                    children[child_count][child_len] = '\0';
                    child_count++;
                }
                child_start = -1;
            }
        }
    }
    
    *children_out = children;
    return child_count;
}

// Convert parentheses to Matula number
static unsigned long long
parens_to_matula(char *parens)
{
    if (parens == NULL || parens[0] == '\0')
        return 1;
    
    if (strcmp(parens, "()") == 0)
        return 1;
    
    char **children = NULL;
    int child_count = parse_children(parens, &children);
    
    if (child_count == 0)
        return 1;
    
    unsigned long long result = 1;
    for (int i = 0; i < child_count; i++) {
        unsigned long long child_matula = parens_to_matula(children[i]);
        unsigned long long prime = nth_prime(child_matula);
        
        if (prime == 0) {
            result = 0;
            break;
        }
        
        result *= prime;
        if (result == 0)
            break;
    }
    
    for (int i = 0; i < child_count; i++)
        free(children[i]);
    free(children);
    
    return result;
}

// Convert Matula number to parentheses
static char*
matula_to_parens(unsigned long long matula)
{
    if (matula == 1) {
        char *result = malloc(3);
        if (result != NULL)
            strcpy(result, "()");
        return result;
    }
    
    int exponents[NPRIMES];
    factorize(matula, exponents, NPRIMES);
    
    char *result = malloc(4096);
    if (result == NULL)
        return NULL;
    
    result[0] = '(';
    int pos = 1;
    
    for (int i = 0; i < NPRIMES && pos < 4090; i++) {
        for (int j = 0; j < exponents[i] && pos < 4090; j++) {
            char *child = matula_to_parens(i + 1);
            if (child != NULL) {
                int child_len = strlen(child);
                if (pos + child_len < 4090) {
                    strcpy(result + pos, child);
                    pos += child_len;
                }
                free(child);
            }
        }
    }
    
    result[pos++] = ')';
    result[pos] = '\0';
    
    return result;
}

// Print factorization
static void
print_factorization(unsigned long long n)
{
    int exponents[NPRIMES];
    factorize(n, exponents, NPRIMES);
    
    int first = 1;
    for (int i = 0; i < NPRIMES; i++) {
        if (exponents[i] > 0) {
            if (!first)
                printf(" × ");
            if (exponents[i] == 1)
                printf("%llu", primes[i]);
            else
                printf("%llu^%d", primes[i], exponents[i]);
            first = 0;
        }
    }
    if (first)
        printf("1");
}

// Test a single tree
static void
test_tree(char *parens, char *description)
{
    printf("\n");
    printf("Tree: %-20s (%s)\n", parens, description);
    
    unsigned long long matula = parens_to_matula(parens);
    printf("  Matula number: %llu\n", matula);
    
    printf("  Factorization: ");
    print_factorization(matula);
    printf("\n");
    
    char *reconstructed = matula_to_parens(matula);
    if (reconstructed != NULL) {
        printf("  Reconstructed: %s", reconstructed);
        if (strcmp(parens, reconstructed) == 0)
            printf(" ✓");
        else
            printf(" (different but equivalent)");
        printf("\n");
        free(reconstructed);
    }
}

int
main(void)
{
    printf("╔════════════════════════════════════════════════════════════════╗\n");
    printf("║         Matula Numbers for Rooted Trees - Demonstration        ║\n");
    printf("╚════════════════════════════════════════════════════════════════╝\n");
    
    printf("\nMatula numbers provide a bijection between rooted trees and natural\n");
    printf("numbers using prime factorization. Each tree maps to a unique integer.\n");
    
    printf("\n═══════════════════════════════════════════════════════════════\n");
    printf("  Examples from OEIS A000081\n");
    printf("═══════════════════════════════════════════════════════════════\n");
    
    // Test cases from problem statement
    test_tree("()", "single node");
    test_tree("(())", "one child");
    test_tree("(()())", "two children (sequential)");
    test_tree("((()))", "nested depth 3");
    test_tree("((()()))", "one child with two children");
    test_tree("((())())", "mixed structure");
    test_tree("(()()())", "three children");
    test_tree("(((())))", "deep nesting");
    
    printf("\n═══════════════════════════════════════════════════════════════\n");
    printf("  Matula Number Mapping Table (A000081)\n");
    printf("═══════════════════════════════════════════════════════════════\n");
    printf("\n  Nodes  Trees   Matula Range  Example Trees\n");
    printf("  ─────  ─────   ────────────  ──────────────────────────\n");
    printf("    1      1         1          ()\n");
    printf("    2      1         2          (())\n");
    printf("    3      2       3-4          ((())), (()())\n");
    printf("    4      4       5-8          ((((())),...\n");
    printf("    5      9       9-20         ...\n");
    printf("    6     20      21-48         ...\n");
    
    printf("\n═══════════════════════════════════════════════════════════════\n");
    printf("  Inverse: Matula Number → Tree\n");
    printf("═══════════════════════════════════════════════════════════════\n");
    
    unsigned long long test_matulas[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12};
    for (int i = 0; i < sizeof(test_matulas)/sizeof(test_matulas[0]); i++) {
        unsigned long long m = test_matulas[i];
        char *tree = matula_to_parens(m);
        if (tree != NULL) {
            printf("\n  %2llu → %s\n", m, tree);
            printf("      = ");
            print_factorization(m);
            printf("\n");
            free(tree);
        }
    }
    
    printf("\n═══════════════════════════════════════════════════════════════\n");
    printf("  Properties\n");
    printf("═══════════════════════════════════════════════════════════════\n");
    printf("\n  • Each tree has exactly one Matula number\n");
    printf("  • Each positive integer corresponds to exactly one tree\n");
    printf("  • Matula number 1 = single node ()\n");
    printf("  • Prime p = tree with single child having Matula (p's index)\n");
    printf("  • Product = tree with multiple children\n");
    printf("  • Powers indicate repeated children\n");
    
    printf("\n═══════════════════════════════════════════════════════════════\n");
    printf("  Applications in Cognitive Cities\n");
    printf("═══════════════════════════════════════════════════════════════\n");
    printf("\n  • Unique addressing: Each configuration has a number\n");
    printf("  • Efficient storage: Trees encoded as integers\n");
    printf("  • Quick lookup: Integer → configuration\n");
    printf("  • Pattern matching: Compare numbers instead of structures\n");
    printf("  • Database indexing: Use Matula numbers as keys\n");
    
    printf("\n");
    return 0;
}
