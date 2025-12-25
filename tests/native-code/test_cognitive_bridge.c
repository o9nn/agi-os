/**
 * @file test_cognitive_bridge.c
 * @brief Unit tests for Cognitive Synergy Bridge
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "cognitive_synergy_bridge.h"

#define TEST_PASS() printf("  [PASS] %s\n", __func__)
#define TEST_FAIL(msg) do { printf("  [FAIL] %s: %s\n", __func__, msg); return 1; } while(0)

/* Test context initialization */
int test_context_init(void) {
    cog_context_t* ctx = cog_context_create(NULL);
    if (!ctx) TEST_FAIL("Failed to create context");
    
    cog_context_destroy(ctx);
    TEST_PASS();
    return 0;
}

/* Test atom creation */
int test_atom_creation(void) {
    cog_context_t* ctx = cog_context_create(NULL);
    if (!ctx) TEST_FAIL("Failed to create context");
    
    cog_atom_t concept = cog_create_concept(ctx, "TestConcept");
    if (!concept) TEST_FAIL("Failed to create concept");
    
    cog_atom_t node = cog_create_node(ctx, COG_NODE_CONCEPT, "TestNode");
    if (!node) TEST_FAIL("Failed to create node");
    
    cog_context_destroy(ctx);
    TEST_PASS();
    return 0;
}

/* Test link creation */
int test_link_creation(void) {
    cog_context_t* ctx = cog_context_create(NULL);
    if (!ctx) TEST_FAIL("Failed to create context");
    
    cog_atom_t a = cog_create_concept(ctx, "A");
    cog_atom_t b = cog_create_concept(ctx, "B");
    
    cog_atom_t outgoing[] = {a, b};
    cog_atom_t link = cog_create_link(ctx, COG_LINK_INHERITANCE, outgoing, 2);
    if (!link) TEST_FAIL("Failed to create link");
    
    cog_context_destroy(ctx);
    TEST_PASS();
    return 0;
}

/* Test truth value operations */
int test_truth_values(void) {
    cog_context_t* ctx = cog_context_create(NULL);
    if (!ctx) TEST_FAIL("Failed to create context");
    
    cog_atom_t concept = cog_create_concept(ctx, "TruthTest");
    
    cog_truth_value_t tv = {0.8f, 0.9f};
    int result = cog_set_truth_value(ctx, concept, &tv);
    if (result != 0) TEST_FAIL("Failed to set truth value");
    
    cog_truth_value_t retrieved;
    result = cog_get_truth_value(ctx, concept, &retrieved);
    if (result != 0) TEST_FAIL("Failed to get truth value");
    
    if (retrieved.strength < 0.79f || retrieved.strength > 0.81f)
        TEST_FAIL("Truth value strength mismatch");
    
    cog_context_destroy(ctx);
    TEST_PASS();
    return 0;
}

/* Test reasoning cycle */
int test_reasoning_cycle(void) {
    cog_context_t* ctx = cog_context_create(NULL);
    if (!ctx) TEST_FAIL("Failed to create context");
    
    int result = cog_reason(ctx, 10);
    if (result < 0) TEST_FAIL("Reasoning cycle failed");
    
    cog_context_destroy(ctx);
    TEST_PASS();
    return 0;
}

/* Main test runner */
int main(int argc, char** argv) {
    (void)argc;
    (void)argv;
    
    printf("Cognitive Synergy Bridge Tests\n");
    printf("==============================\n\n");
    
    int failures = 0;
    
    failures += test_context_init();
    failures += test_atom_creation();
    failures += test_link_creation();
    failures += test_truth_values();
    failures += test_reasoning_cycle();
    
    printf("\n==============================\n");
    if (failures == 0) {
        printf("All tests passed!\n");
        return 0;
    } else {
        printf("%d test(s) failed\n", failures);
        return 1;
    }
}
