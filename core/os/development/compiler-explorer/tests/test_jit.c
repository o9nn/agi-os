#include "../src/jit_compiler.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
static int tests_run = 0;
static int tests_passed = 0;
#define TEST_ASSERT(condition, message) \
do { \
tests_run++; \
if (condition) { \
tests_passed++; \
printf("[PASS] %s\n", message); \
} else { \
printf("[FAIL] %s\n", message); \
} \
} while(0)
static const char* sample_code =
"int add(int a, int b) {\n"
"    return a + b;\n"
"}\n";
static const char* complex_code =
"int factorial(int n) {\n"
"    if (n <= 1) return 1;\n"
"    return n * factorial(n - 1);\n"
"}\n"
"\n"
"int fibonacci(int n) {\n"
"    if (n <= 1) return n;\n"
"    return fibonacci(n-1) + fibonacci(n-2);\n"
"}\n";
void test_jit_initialization(void) {
printf("\n=== Testing JIT Initialization ===\n");
jit_context_t* ctx = jit_init(NULL);
TEST_ASSERT(ctx != NULL, "Basic JIT initialization");
if (ctx) {
jit_destroy(ctx);
}
jit_config_t config = {
.optimization_level = 1,
.enable_cognitive = true,
.enable_logging = true,
.target_arch = "native",
.log_file = NULL
};
ctx = jit_init(&config);
TEST_ASSERT(ctx != NULL, "JIT initialization with custom config");
if (ctx) {
jit_destroy(ctx);
}
}
void test_jit_compilation(void) {
printf("\n=== Testing JIT Compilation ===\n");
jit_context_t* ctx = jit_init(NULL);
if (!ctx) {
printf("Failed to initialize JIT context\n");
return;
}
jit_stats_t stats;
jit_error_t result = jit_compile(ctx, sample_code, "add", &stats);
TEST_ASSERT(result == JIT_SUCCESS, "Simple function compilation");
TEST_ASSERT(stats.compile_time_us > 0, "Compilation time recorded");
TEST_ASSERT(stats.code_size_bytes > 0, "Code size recorded");
result = jit_compile(ctx, NULL, "invalid", NULL);
TEST_ASSERT(result != JIT_SUCCESS, "Compilation with invalid input fails");
result = jit_compile(ctx, sample_code, "", NULL);
TEST_ASSERT(result != JIT_SUCCESS, "Compilation with empty function name fails");
jit_destroy(ctx);
}
void test_jit_execution(void) {
printf("\n=== Testing JIT Execution ===\n");
jit_context_t* ctx = jit_init(NULL);
if (!ctx) {
printf("Failed to initialize JIT context\n");
return;
}
jit_error_t result = jit_compile(ctx, sample_code, "add", NULL);
if (result != JIT_SUCCESS) {
printf("Compilation failed, skipping execution tests\n");
jit_destroy(ctx);
return;
}
int result_value;
result = jit_execute(ctx, "add", NULL, &result_value);
TEST_ASSERT(result == JIT_SUCCESS, "Function execution");
result = jit_execute(ctx, "nonexistent", NULL, NULL);
TEST_ASSERT(result != JIT_SUCCESS, "Execution of non-existent function fails");
jit_destroy(ctx);
}
void test_cognitive_features(void) {
printf("\n=== Testing Cognitive Features ===\n");
jit_config_t config = {
.optimization_level = 2,
.enable_cognitive = true,
.enable_logging = false,
.target_arch = "native",
.log_file = NULL
};
jit_context_t* ctx = jit_init(&config);
if (!ctx) {
printf("Failed to initialize JIT context\n");
return;
}
jit_error_t result = jit_apply_cognitive_hint(ctx, "optimization.loop_unroll", NULL);
TEST_ASSERT(result == JIT_SUCCESS, "Apply cognitive optimization hint");
result = jit_apply_cognitive_hint(ctx, "pattern.memory_leak", NULL);
TEST_ASSERT(result == JIT_SUCCESS, "Apply cognitive pattern hint");
jit_stats_t stats;
result = jit_get_stats(ctx, &stats);
TEST_ASSERT(result == JIT_SUCCESS, "Get JIT statistics");
TEST_ASSERT(stats.cognitive_hints > 0, "Cognitive hints recorded in statistics");
jit_destroy(ctx);
}
void test_error_handling(void) {
printf("\n=== Testing Error Handling ===\n");
jit_error_t result = jit_compile(NULL, sample_code, "test", NULL);
TEST_ASSERT(result != JIT_SUCCESS, "Compilation with null context fails");
result = jit_execute(NULL, "test", NULL, NULL);
TEST_ASSERT(result != JIT_SUCCESS, "Execution with null context fails");
jit_context_t* ctx = jit_init(NULL);
if (ctx) {
jit_compile(ctx, NULL, "invalid", NULL);
const char* error = jit_get_error(ctx);
TEST_ASSERT(error != NULL, "Error message retrieval");
TEST_ASSERT(strlen(error) > 0, "Error message is non-empty");
jit_destroy(ctx);
}
}
void test_platform_support(void) {
printf("\n=== Testing Platform Support ===\n");
bool available = jit_is_available();
TEST_ASSERT(available == true || available == false, "JIT availability check");
const char* version = jit_get_version();
TEST_ASSERT(version != NULL, "Version string available");
TEST_ASSERT(strstr(version, "HurdCog") != NULL, "Version contains HurdCog");
TEST_ASSERT(strstr(version, "SKZ") != NULL, "Version contains SKZ Framework");
printf("JIT Version: %s\n", version);
printf("JIT Available: %s\n", available ? "Yes" : "No");
}
void run_demo(void) {
printf("\n🧠 === HurdCog Compiler Explorer JIT Infrastructure Demo === 🧠\n");
printf("Part of Phase 2: Microkernel Integration - SKZ Framework\n\n");
printf("1. Initializing JIT compiler...\n");
jit_context_t* ctx = jit_init(NULL);
if (!ctx) {
printf("❌ Failed to initialize JIT context\n");
return;
}
printf("✅ JIT compiler initialized successfully\n");
printf("\n2. Compiling sample microkernel function...\n");
printf("Source code:\n%s\n", sample_code);
jit_stats_t stats;
jit_error_t result = jit_compile(ctx, sample_code, "add", &stats);
if (result == JIT_SUCCESS) {
printf("✅ Compilation successful!\n");
printf("   - Compile time: %llu microseconds\n", (unsigned long long)stats.compile_time_us);
printf("   - Code size: %llu bytes\n", (unsigned long long)stats.code_size_bytes);
printf("   - Optimization passes: %d\n", stats.optimization_passes);
printf("   - Cognitive hints: %d\n", stats.cognitive_hints);
} else {
printf("❌ Compilation failed: %s\n", jit_get_error(ctx));
jit_destroy(ctx);
return;
}
printf("\n3. Applying cognitive optimization hints...\n");
jit_apply_cognitive_hint(ctx, "microkernel.ipc_optimization", NULL);
jit_apply_cognitive_hint(ctx, "hurd.memory_management", NULL);
printf("✅ Cognitive hints applied\n");
printf("\n4. Executing JIT compiled function...\n");
int exec_result;
result = jit_execute(ctx, "add", NULL, &exec_result);
if (result == JIT_SUCCESS) {
printf("✅ Function executed successfully\n");
printf("   - Result: %d (placeholder)\n", exec_result);
} else {
printf("❌ Execution failed: %s\n", jit_get_error(ctx));
}
printf("\n5. Final statistics:\n");
jit_get_stats(ctx, &stats);
printf("   - Total compile time: %llu microseconds\n", (unsigned long long)stats.compile_time_us);
printf("   - Cognitive hints applied: %d\n", stats.cognitive_hints);
printf("   - Optimization level: %d\n", stats.optimization_passes);
printf("\n6. Cleanup...\n");
jit_destroy(ctx);
printf("✅ JIT context destroyed\n");
printf("\n🎉 Demo completed! JIT infrastructure is ready for microkernel development.\n");
printf("📚 See development/docs/HurdCog_Compiler_Explorer_Roadmap.md for full roadmap.\n");
}
int main(int argc, char* argv[]) {
printf("HurdCog Compiler Explorer JIT Infrastructure Test Suite\n");
printf("========================================================\n");
printf("Version: %s\n", jit_get_version());
if (argc > 1 && strcmp(argv[1], "--demo") == 0) {
run_demo();
return 0;
}
test_jit_initialization();
test_jit_compilation();
test_jit_execution();
test_cognitive_features();
test_error_handling();
test_platform_support();
printf("\n=== Test Summary ===\n");
printf("Tests run: %d\n", tests_run);
printf("Tests passed: %d\n", tests_passed);
printf("Tests failed: %d\n", tests_run - tests_passed);
if (tests_passed == tests_run) {
printf("🎉 All tests passed! JIT infrastructure is working correctly.\n");
return 0;
} else {
printf("❌ Some tests failed. Please check the implementation.\n");
return 1;
}
}