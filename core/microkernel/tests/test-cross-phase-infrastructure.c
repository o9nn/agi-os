#include <testlib.h>
#include <mach/mach_safety.h>
static void test_integer_overflow_protection(void);
static void test_bounds_checking(void);
static void test_resource_validation(void);
static void test_vm_safety_checks(void);
void
test_cross_phase_infrastructure(void)
{
test_msg("=== Cross-Phase Infrastructure Tests ===");
test_msg("Testing Integer Overflow Protection...");
test_integer_overflow_protection();
test_msg("Testing Bounds Checking...");
test_bounds_checking();
test_msg("Testing Resource Validation...");
test_resource_validation();
test_msg("Testing VM Safety Checks...");
test_vm_safety_checks();
test_msg("=== Cross-Phase Infrastructure Tests Complete ===");
}
static void
test_integer_overflow_protection(void)
{
uint32_t a, b, result;
kern_return_t ret;
a = 1000;
b = 2000;
ret = MACH_SAFE_ADD(a, b, &result, UINT32);
ASSERT(ret == KERN_SUCCESS, "Safe addition should succeed for valid values");
ASSERT(result == 3000, "Safe addition result should be correct");
a = 0xFFFFFFFF;
b = 1;
ret = MACH_SAFE_ADD(a, b, &result, UINT32);
ASSERT(ret == KERN_INVALID_ARGUMENT, "Safe addition should detect overflow");
a = 10;
b = 20;
ret = MACH_SAFE_MUL(a, b, &result, UINT32);
ASSERT(ret == KERN_SUCCESS, "Safe multiplication should succeed for valid values");
ASSERT(result == 200, "Safe multiplication result should be correct");
a = 0x10000;
b = 0x10000;
ret = MACH_SAFE_MUL(a, b, &result, UINT32);
ASSERT(ret == KERN_INVALID_ARGUMENT, "Safe multiplication should detect overflow");
test_msg("Integer overflow protection tests passed");
}
static void
test_bounds_checking(void)
{
char buffer[100];
uintptr_t base = (uintptr_t)buffer;
uintptr_t limit = base + sizeof(buffer);
ASSERT(MACH_BOUNDS_CHECK(base, 50, sizeof(buffer), limit),
"Bounds check should pass for valid offset");
ASSERT(!MACH_BOUNDS_CHECK(base, 150, sizeof(buffer), limit),
"Bounds check should fail for offset beyond buffer");
ASSERT(MACH_RANGE_CHECK(base, 50, limit),
"Range check should pass for valid range");
ASSERT(!MACH_RANGE_CHECK(base, 200, limit),
"Range check should fail for range beyond limit");
test_msg("Bounds checking tests passed");
}
static void
test_resource_validation(void)
{
char buffer[100];
char *valid_ptr = buffer;
char *null_ptr = NULL;
uintptr_t min_addr = (uintptr_t)buffer;
uintptr_t max_addr = min_addr + sizeof(buffer);
ASSERT(MACH_VALIDATE_PTR(valid_ptr, (void*)min_addr, (void*)max_addr),
"Pointer validation should pass for valid pointer");
ASSERT(!MACH_VALIDATE_PTR(null_ptr, (void*)min_addr, (void*)max_addr),
"Pointer validation should fail for NULL pointer");
ASSERT(MACH_VALIDATE_REGION(buffer, sizeof(buffer)),
"Region validation should pass for valid region");
ASSERT(!MACH_VALIDATE_REGION(buffer, 0),
"Region validation should fail for zero-size region");
test_msg("Resource validation tests passed");
}
static void
test_vm_safety_checks(void)
{
struct {
uintptr_t start;
uintptr_t end;
} valid_entry = { 0x1000, 0x2000 };
struct {
uintptr_t start;
uintptr_t end;
} invalid_entry = { 0x2000, 0x1000 };
ASSERT(MACH_VM_ENTRY_VALID(&valid_entry),
"VM entry validation should pass for valid entry");
ASSERT(!MACH_VM_ENTRY_VALID(&invalid_entry),
"VM entry validation should fail for invalid entry");
ASSERT(MACH_VM_ALIGNED((void*)0x1000, 0x1000),
"VM alignment check should pass for aligned address");
ASSERT(!MACH_VM_ALIGNED((void*)0x1001, 0x1000),
"VM alignment check should fail for unaligned address");
test_msg("VM safety check tests passed");
}
int
main(int argc, char *argv[], int envc, char *envp[])
{
test_cross_phase_infrastructure();
return 0;
}