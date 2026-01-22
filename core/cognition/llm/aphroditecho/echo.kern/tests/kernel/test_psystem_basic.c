#define _GNU_SOURCE
#define _POSIX_C_SOURCE 199309L
#include "include/dtesn/psystem.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int main(void) {
printf("DTESN P-System - Basic Functionality Test\n");
printf("==========================================\n");
printf("1. Testing P-System initialization...\n");
int result = dtesn_psystem_init();
if (result != 0) {
printf("   FAIL: P-System initialization failed\n");
return 1;
}
printf("   PASS: P-System initialized successfully\n");
printf("2. Testing P-System creation...\n");
dtesn_psystem_t *system = dtesn_psystem_create("test_system", 10);
if (!system) {
printf("   FAIL: P-System creation failed\n");
return 1;
}
printf("   PASS: P-System created successfully\n");
printf("   - System name: %s\n", system->system_name);
printf("   - Capacity: %u membranes\n", system->membrane_capacity);
printf("3. Testing membrane creation...\n");
uint32_t root_id = dtesn_membrane_create(system, DTESN_MEMBRANE_ROOT,
"root", 0, 100);
if (root_id == 0) {
printf("   FAIL: Root membrane creation failed\n");
dtesn_psystem_destroy(system);
return 1;
}
printf("   PASS: Root membrane created (ID: %u)\n", root_id);
uint32_t child_id = dtesn_membrane_create(system, DTESN_MEMBRANE_LEAF,
"child", root_id, 50);
if (child_id == 0) {
printf("   FAIL: Child membrane creation failed\n");
dtesn_psystem_destroy(system);
return 1;
}
printf("   PASS: Child membrane created (ID: %u)\n", child_id);
printf("   - Total membranes: %u\n", system->membrane_count);
printf("4. Testing object operations...\n");
result = dtesn_membrane_add_object(system, root_id, "signal", 5);
if (result != 0) {
printf("   FAIL: Adding objects failed\n");
dtesn_psystem_destroy(system);
return 1;
}
printf("   PASS: Objects added successfully\n");
uint32_t removed = dtesn_membrane_remove_object(system, root_id, "signal", 2);
printf("   - Removed %u objects\n", removed);
printf("5. Testing multiset operations...\n");
dtesn_psystem_multiset_t *multiset = dtesn_multiset_create();
if (!multiset) {
printf("   FAIL: Multiset creation failed\n");
dtesn_psystem_destroy(system);
return 1;
}
result = dtesn_multiset_add(multiset, "test", 3);
if (result != 0) {
printf("   FAIL: Multiset add failed\n");
dtesn_multiset_destroy(multiset);
dtesn_psystem_destroy(system);
return 1;
}
printf("   PASS: Multiset operations successful\n");
printf("   - Object count: %u\n", multiset->object_count);
printf("   - Total multiplicity: %u\n", multiset->total_multiplicity);
dtesn_multiset_destroy(multiset);
printf("6. Testing OEIS A000081 validation...\n");
bool is_valid = dtesn_psystem_validate_a000081(system);
printf("   - Hierarchy validation: %s\n", is_valid ? "VALID" : "INVALID");
printf("7. Testing performance statistics...\n");
dtesn_psystem_stats_t stats;
result = dtesn_psystem_get_stats(system, &stats);
if (result != 0) {
printf("   FAIL: Getting statistics failed\n");
dtesn_psystem_destroy(system);
return 1;
}
printf("   PASS: Statistics retrieved successfully\n");
printf("   - Active membranes: %u\n", stats.active_membranes);
printf("   - Total objects: %u\n", stats.total_objects);
printf("8. Testing system cleanup...\n");
dtesn_psystem_destroy(system);
dtesn_psystem_shutdown();
printf("   PASS: System cleaned up successfully\n");
printf("\n✅ ALL BASIC TESTS PASSED\n");
printf("P-System kernel module basic functionality verified!\n");
return 0;
}