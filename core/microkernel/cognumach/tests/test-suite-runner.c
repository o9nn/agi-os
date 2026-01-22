#include <syscalls.h>
#include <testlib.h>
typedef struct {
const char* name;
const char* description;
int (*test_func)(void);
const char* category;
} test_case_t;
typedef struct {
int total_tests;
int passed_tests;
int failed_tests;
int skipped_tests;
uint64_t start_time;
uint64_t end_time;
} test_results_t;
static test_results_t g_results = {0};
int test_hello_functionality(void)
{
printf("Testing basic hello functionality\n");
int ret = printf("hello from test suite!\n");
return (ret > 0) ? 0 : -1;
}
int test_basic_assertions(void)
{
printf("Testing assertion macros\n");
ASSERT(1 == 1, "basic equality should work");
ASSERT(5 > 3, "basic comparison should work");
return 0;
}
int test_error_handling(void)
{
printf("Testing error handling\n");
const char* error_str = e2s(KERN_SUCCESS);
ASSERT(error_str != NULL, "error string should not be null");
error_str = e2s(KERN_FAILURE);
ASSERT(error_str != NULL, "error string for failure should not be null");
return 0;
}
int test_memory_basic(void)
{
printf("Testing basic memory operations\n");
vm_address_t addr;
kern_return_t kr;
kr = vm_allocate(mach_task_self(), &addr, vm_page_size, TRUE);
if (kr != KERN_SUCCESS) {
printf("Memory allocation failed: %s\n", e2s(kr));
return -1;
}
kr = vm_deallocate(mach_task_self(), addr, vm_page_size);
if (kr != KERN_SUCCESS) {
printf("Memory deallocation failed: %s\n", e2s(kr));
return -1;
}
return 0;
}
int test_port_basic(void)
{
printf("Testing basic port operations\n");
mach_port_t port;
kern_return_t kr;
kr = mach_port_allocate(mach_task_self(), MACH_PORT_RIGHT_RECEIVE, &port);
if (kr != KERN_SUCCESS) {
printf("Port allocation failed: %s\n", e2s(kr));
return -1;
}
kr = mach_port_deallocate(mach_task_self(), port);
if (kr != KERN_SUCCESS) {
printf("Port deallocation failed: %s\n", e2s(kr));
return -1;
}
return 0;
}
int test_task_basic(void)
{
printf("Testing basic task operations\n");
struct task_basic_info info;
mach_msg_type_number_t count = TASK_BASIC_INFO_COUNT;
kern_return_t kr;
kr = task_info(mach_task_self(), TASK_BASIC_INFO,
(task_info_t)&info, &count);
if (kr != KERN_SUCCESS) {
printf("Task info failed: %s\n", e2s(kr));
return -1;
}
ASSERT(info.virtual_size > 0, "Virtual size should be positive");
ASSERT(info.resident_size > 0, "Resident size should be positive");
return 0;
}
static test_case_t test_cases[] = {
{"hello", "Basic hello functionality test", test_hello_functionality, "basic"},
{"assertions", "Test assertion macros", test_basic_assertions, "basic"},
{"error_handling", "Test error handling utilities", test_error_handling, "basic"},
{"memory_basic", "Basic memory operations", test_memory_basic, "memory"},
{"port_basic", "Basic port operations", test_port_basic, "ipc"},
{"task_basic", "Basic task operations", test_task_basic, "task"},
{NULL, NULL, NULL, NULL}
};
void print_test_header(void)
{
printf("========================================\n");
printf("GNU Mach Test Suite Runner\n");
printf("Enhanced Test Framework v1.0\n");
printf("========================================\n");
}
void print_test_summary(void)
{
uint64_t duration = g_results.end_time - g_results.start_time;
printf("\n========================================\n");
printf("Test Suite Summary\n");
printf("========================================\n");
printf("Total tests:   %d\n", g_results.total_tests);
printf("Passed:        %d\n", g_results.passed_tests);
printf("Failed:        %d\n", g_results.failed_tests);
printf("Skipped:       %d\n", g_results.skipped_tests);
printf("Duration:      %llu microseconds\n", duration);
printf("Success rate:  %.1f%%\n",
g_results.total_tests > 0 ?
(100.0 * g_results.passed_tests / g_results.total_tests) : 0.0);
if (g_results.failed_tests > 0) {
printf("\nWARNING: %d test(s) failed!\n", g_results.failed_tests);
} else {
printf("\nAll tests passed successfully!\n");
}
printf("========================================\n");
}
void run_test_case(const test_case_t *test)
{
printf("\n--- Running: %s ---\n", test->name);
printf("Description: %s\n", test->description);
printf("Category: %s\n", test->category);
g_results.total_tests++;
uint64_t test_start = get_time_microseconds();
int result = test->test_func();
uint64_t test_end = get_time_microseconds();
if (result == 0) {
printf("PASS: %s (%llu us)\n", test->name, test_end - test_start);
g_results.passed_tests++;
} else {
printf("FAIL: %s (%llu us) - returned %d\n",
test->name, test_end - test_start, result);
g_results.failed_tests++;
}
}
void run_category_tests(const char *category)
{
printf("\n=== Running %s tests ===\n", category);
for (int i = 0; test_cases[i].name != NULL; i++) {
if (category == NULL ||
(test_cases[i].category && strcmp(test_cases[i].category, category) == 0)) {
run_test_case(&test_cases[i]);
}
}
}
void list_available_tests(void)
{
printf("\nAvailable tests:\n");
printf("Category  | Name         | Description\n");
printf("----------|--------------|----------------------------------\n");
for (int i = 0; test_cases[i].name != NULL; i++) {
printf("%-9s | %-12s | %s\n",
test_cases[i].category ? test_cases[i].category : "general",
test_cases[i].name,
test_cases[i].description);
}
printf("\n");
}
int main(int argc, char *argv[], int envc, char *envp[])
{
print_test_header();
g_results.start_time = get_time_microseconds();
if (argc > 1) {
if (strcmp(argv[1], "--list") == 0) {
list_available_tests();
return 0;
} else if (strcmp(argv[1], "--category") == 0 && argc > 2) {
run_category_tests(argv[2]);
} else if (strcmp(argv[1], "--help") == 0) {
printf("Usage: %s [options]\n", argv[0]);
printf("Options:\n");
printf("  --list          List available tests\n");
printf("  --category CAT  Run tests in specific category\n");
printf("  --help          Show this help\n");
printf("\nCategories: basic, memory, ipc, task\n");
return 0;
} else {
printf("Unknown option: %s\n", argv[1]);
printf("Use --help for usage information\n");
return 1;
}
} else {
printf("Running all tests...\n");
list_available_tests();
run_category_tests(NULL);
}
g_results.end_time = get_time_microseconds();
print_test_summary();
return (g_results.failed_tests > 0) ? 1 : 0;
}