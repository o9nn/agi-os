#include <testlib.h>
#include <mach/valgrind.h>
#include <mach/kern_return.h>
#include <mach/vm_param.h>
void test_valgrind_basic(void)
{
test_start("test-valgrind-basic");
printf("Testing Valgrind basic functionality...\n");
boolean_t was_enabled = valgrind_is_enabled();
printf("Valgrind initially %s\n", was_enabled ? "enabled" : "disabled");
kern_return_t ret = valgrind_enable(TRUE);
if (ret != KERN_SUCCESS) {
printf("Failed to enable Valgrind: %d\n", ret);
test_finish("test-valgrind-basic", "FAIL - could not enable Valgrind");
return;
}
if (!valgrind_is_enabled()) {
test_finish("test-valgrind-basic", "FAIL - Valgrind not enabled after enable call");
return;
}
printf("Valgrind enabled successfully\n");
char test_buffer[1024];
vm_address_t addr = (vm_address_t)test_buffer;
vm_size_t size = sizeof(test_buffer);
ret = valgrind_make_mem_defined(addr, size);
if (ret != KERN_SUCCESS) {
printf("Failed to mark memory as defined: %d\n", ret);
test_finish("test-valgrind-basic", "FAIL - make_mem_defined failed");
return;
}
if (!valgrind_is_mem_defined(addr, size)) {
printf("Memory not marked as defined after make_mem_defined\n");
test_finish("test-valgrind-basic", "FAIL - memory state check failed");
return;
}
ret = valgrind_make_mem_undefined(addr, size);
if (ret != KERN_SUCCESS) {
printf("Failed to mark memory as undefined: %d\n", ret);
test_finish("test-valgrind-basic", "FAIL - make_mem_undefined failed");
return;
}
ret = valgrind_make_mem_noaccess(addr, size);
if (ret != KERN_SUCCESS) {
printf("Failed to mark memory as no-access: %d\n", ret);
test_finish("test-valgrind-basic", "FAIL - make_mem_noaccess failed");
return;
}
if (valgrind_is_mem_addressable(addr, size)) {
printf("Memory marked as addressable after make_mem_noaccess\n");
test_finish("test-valgrind-basic", "FAIL - addressability check failed");
return;
}
printf("Basic Valgrind functionality tests passed\n");
valgrind_enable(was_enabled);
test_finish("test-valgrind-basic", "PASS");
}
void test_valgrind_alloc_tracking(void)
{
test_start("test-valgrind-alloc-tracking");
printf("Testing Valgrind allocation tracking...\n");
boolean_t was_enabled = valgrind_is_enabled();
valgrind_enable(TRUE);
vm_size_t alloc_size = 256;
void *ptr = kalloc(alloc_size);
if (ptr == NULL) {
printf("Failed to allocate memory for tracking test\n");
valgrind_enable(was_enabled);
test_finish("test-valgrind-alloc-tracking", "FAIL - allocation failed");
return;
}
printf("Allocated %u bytes at address 0x%lx\n",
(unsigned)alloc_size, (unsigned long)ptr);
valgrind_mem_state_t state = valgrind_check_mem_state((vm_address_t)ptr);
printf("Memory state after allocation: %d\n", (int)state);
kfree((vm_address_t)ptr, alloc_size);
printf("Memory freed\n");
state = valgrind_check_mem_state((vm_address_t)ptr);
printf("Memory state after free: %d\n", (int)state);
if (state != VALGRIND_MEM_NOACCESS) {
printf("Memory not marked as no-access after free\n");
valgrind_enable(was_enabled);
test_finish("test-valgrind-alloc-tracking", "FAIL - memory state incorrect after free");
return;
}
printf("Allocation tracking tests passed\n");
valgrind_enable(was_enabled);
test_finish("test-valgrind-alloc-tracking", "PASS");
}
void test_valgrind_client_requests(void)
{
test_start("test-valgrind-client-requests");
printf("Testing Valgrind client requests...\n");
boolean_t was_enabled = valgrind_is_enabled();
valgrind_enable(TRUE);
char test_buffer[512];
vm_address_t addr = (vm_address_t)test_buffer;
vm_size_t size = sizeof(test_buffer);
kern_return_t result = valgrind_handle_client_request(
VG_USERREQ_MAKE_MEM_DEFINED, addr, size, 0, 0, 0);
if (result != KERN_SUCCESS) {
printf("Client request MAKE_MEM_DEFINED failed: %d\n", result);
valgrind_enable(was_enabled);
test_finish("test-valgrind-client-requests", "FAIL - MAKE_MEM_DEFINED failed");
return;
}
result = valgrind_handle_client_request(
VG_USERREQ_CHECK_MEM_IS_DEFINED, addr, size, 0, 0, 0);
if (result != KERN_SUCCESS) {
printf("Client request CHECK_MEM_IS_DEFINED failed: %d\n", result);
valgrind_enable(was_enabled);
test_finish("test-valgrind-client-requests", "FAIL - CHECK_MEM_IS_DEFINED failed");
return;
}
result = valgrind_handle_client_request(
VG_USERREQ_MAKE_MEM_UNDEFINED, addr, size, 0, 0, 0);
if (result != KERN_SUCCESS) {
printf("Client request MAKE_MEM_UNDEFINED failed: %d\n", result);
valgrind_enable(was_enabled);
test_finish("test-valgrind-client-requests", "FAIL - MAKE_MEM_UNDEFINED failed");
return;
}
printf("Client request tests passed\n");
valgrind_enable(was_enabled);
test_finish("test-valgrind-client-requests", "PASS");
}
int main(int argc, char *argv[])
{
printf("=== GNU Mach Valgrind Integration Tests ===\n");
test_valgrind_basic();
test_valgrind_alloc_tracking();
test_valgrind_client_requests();
printf("=== Valgrind tests completed ===\n");
return test_exit();
}