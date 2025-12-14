/*
 * Matula Number Test Suite
 * 
 * Comprehensive tests for the Matula number encoding/decoding implementation.
 * Tests edge cases, roundtrip conversion, and mathematical properties.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Prime number table
static unsigned long long primes[] = {
    2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
    73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151,
    157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229
};
#define NPRIMES (sizeof(primes)/sizeof(primes[0]))

static unsigned long long
nth_prime(int n)
{
    if (n < 1 || n > NPRIMES)
        return 0;
    return primes[n - 1];
}

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

// Test helper
static int test_count = 0;
static int test_passed = 0;
static int test_failed = 0;

#define TEST(name) \
    printf("\n  Test %d: %s\n", ++test_count, name);

#define ASSERT_EQ(actual, expected, msg) \
    do { \
        if ((actual) == (expected)) { \
            printf("    ✓ %s\n", msg); \
            test_passed++; \
        } else { \
            printf("    ✗ %s (expected %llu, got %llu)\n", msg, \
                   (unsigned long long)(expected), (unsigned long long)(actual)); \
            test_failed++; \
        } \
    } while(0)

#define ASSERT_STR_EQ(actual, expected, msg) \
    do { \
        if (strcmp((actual), (expected)) == 0) { \
            printf("    ✓ %s\n", msg); \
            test_passed++; \
        } else { \
            printf("    ✗ %s (expected '%s', got '%s')\n", msg, expected, actual); \
            test_failed++; \
        } \
    } while(0)

void test_basic_encoding()
{
    TEST("Basic encoding");
    
    ASSERT_EQ(parens_to_matula("()"), 1, "Single node");
    ASSERT_EQ(parens_to_matula("(())"), 2, "One child");
    ASSERT_EQ(parens_to_matula("(()())"), 4, "Two children");
    ASSERT_EQ(parens_to_matula("((()))"), 3, "Nested depth 3");
    ASSERT_EQ(parens_to_matula("(()()())"), 8, "Three children");
}

void test_prime_encoding()
{
    TEST("Prime number encoding");
    
    // Primes should encode trees with single children
    ASSERT_EQ(parens_to_matula("(())"), 2, "p(1) = 2");
    ASSERT_EQ(parens_to_matula("((()))"), 3, "p(2) = 3");
    ASSERT_EQ(parens_to_matula("(((())))"), 5, "p(3) = 5");
    ASSERT_EQ(parens_to_matula("((()()))"), 7, "p(4) = 7");
}

void test_composite_encoding()
{
    TEST("Composite number encoding");
    
    // 4 = 2^2 (two children, both ())
    ASSERT_EQ(parens_to_matula("(()())"), 4, "2^2 = two leaf children");
    
    // 6 = 2 × 3 (two children: () and (()))
    unsigned long long m6 = parens_to_matula("(()(()))");
    ASSERT_EQ(m6, 6, "2 × 3 = children with Matula 1 and 2");
    
    // 8 = 2^3 (three leaf children)
    ASSERT_EQ(parens_to_matula("(()()())"), 8, "2^3 = three leaf children");
    
    // 9 = 3^2 (two children, both (()))
    unsigned long long m9 = parens_to_matula("((())(()))");
    ASSERT_EQ(m9, 9, "3^2 = two depth-3 children");
}

void test_decoding()
{
    TEST("Basic decoding");
    
    char *t1 = matula_to_parens(1);
    ASSERT_STR_EQ(t1, "()", "Decode 1");
    free(t1);
    
    char *t2 = matula_to_parens(2);
    ASSERT_STR_EQ(t2, "(())", "Decode 2");
    free(t2);
    
    char *t3 = matula_to_parens(3);
    ASSERT_STR_EQ(t3, "((()))", "Decode 3");
    free(t3);
    
    char *t4 = matula_to_parens(4);
    ASSERT_STR_EQ(t4, "(()())", "Decode 4");
    free(t4);
}

void test_roundtrip()
{
    TEST("Roundtrip conversion");
    
    struct {
        char *parens;
        unsigned long long expected_matula;
    } tests[] = {
        {"()", 1},
        {"(())", 2},
        {"((()))", 3},
        {"(()())", 4},
        {"(((()))))", 5},
        {"(()()())", 8},
    };
    
    for (int i = 0; i < sizeof(tests)/sizeof(tests[0]); i++) {
        // Encode
        unsigned long long matula = parens_to_matula(tests[i].parens);
        ASSERT_EQ(matula, tests[i].expected_matula, "Encode matches expected");
        
        // Decode
        char *decoded = matula_to_parens(matula);
        
        // Re-encode to verify equivalence
        unsigned long long re_encoded = parens_to_matula(decoded);
        ASSERT_EQ(re_encoded, matula, "Roundtrip preserves Matula number");
        
        free(decoded);
    }
}

void test_factorization()
{
    TEST("Prime factorization");
    
    int exp[NPRIMES];
    
    // 12 = 2^2 × 3
    factorize(12, exp, NPRIMES);
    ASSERT_EQ(exp[0], 2, "12: exponent of 2 is 2");
    ASSERT_EQ(exp[1], 1, "12: exponent of 3 is 1");
    ASSERT_EQ(exp[2], 0, "12: exponent of 5 is 0");
    
    // 18 = 2 × 3^2
    factorize(18, exp, NPRIMES);
    ASSERT_EQ(exp[0], 1, "18: exponent of 2 is 1");
    ASSERT_EQ(exp[1], 2, "18: exponent of 3 is 2");
    
    // Prime: 7
    factorize(7, exp, NPRIMES);
    ASSERT_EQ(exp[0], 0, "7: exponent of 2 is 0");
    ASSERT_EQ(exp[3], 1, "7: exponent of 7 is 1");
}

void test_edge_cases()
{
    TEST("Edge cases");
    
    // Empty/null input
    ASSERT_EQ(parens_to_matula(""), 1, "Empty string");
    ASSERT_EQ(parens_to_matula(NULL), 1, "NULL input");
    
    // Just parentheses
    ASSERT_EQ(parens_to_matula("()"), 1, "Single pair");
    
    // Deep nesting
    unsigned long long deep = parens_to_matula("(((((((())))))))");
    printf("    Deep nesting Matula: %llu\n", deep);
    test_passed++; // Just verify it doesn't crash
}

void test_known_sequence()
{
    TEST("OEIS A000081 correspondence");
    
    // These are known values from the problem statement
    struct {
        char *description;
        char *parens;
        unsigned long long matula;
    } known[] = {
        {"[]", "()", 1},
        {"[[]]", "(())", 2},
        {"[] []", "(()())", 4},
        {"[[[]]]", "((()))", 3},
        {"[[],[]]", "((()()))", 7},
        {"[[], []]", "(()(()))", 6},
        {"[] [] []", "(()()())", 8},
        {"[[[[]]]]", "(((()))))", 5},
    };
    
    for (int i = 0; i < sizeof(known)/sizeof(known[0]); i++) {
        unsigned long long m = parens_to_matula(known[i].parens);
        ASSERT_EQ(m, known[i].matula, known[i].description);
    }
}

int main()
{
    printf("╔════════════════════════════════════════════════════════════════╗\n");
    printf("║          Matula Numbers - Comprehensive Test Suite            ║\n");
    printf("╚════════════════════════════════════════════════════════════════╝\n");
    
    test_basic_encoding();
    test_prime_encoding();
    test_composite_encoding();
    test_decoding();
    test_roundtrip();
    test_factorization();
    test_edge_cases();
    test_known_sequence();
    
    printf("\n═══════════════════════════════════════════════════════════════\n");
    printf("  Test Summary\n");
    printf("═══════════════════════════════════════════════════════════════\n");
    printf("  Total tests: %d\n", test_count);
    printf("  Assertions passed: %d\n", test_passed);
    printf("  Assertions failed: %d\n", test_failed);
    
    if (test_failed == 0) {
        printf("\n  ✓ All tests passed!\n\n");
        return 0;
    } else {
        printf("\n  ✗ Some tests failed.\n\n");
        return 1;
    }
}
