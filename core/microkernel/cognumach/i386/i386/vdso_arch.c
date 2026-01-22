#include <kern/vdso.h>
#include <kern/printf.h>
#include <i386/proc_reg.h>
#include <string.h>
static const unsigned char vdso_gettimeofday_stub[] = {
0x55,
0x89, 0xe5,
0xb8, 0x00, 0x00, 0x00, 0x00,
0xcd, 0x80,
0x5d,
0xc3
};
static const unsigned char vdso_clock_gettime_stub[] = {
0x55,
0x89, 0xe5,
0xb8, 0x01, 0x00, 0x00, 0x00,
0xcd, 0x80,
0x5d,
0xc3
};
static const unsigned char vdso_time_stub[] = {
0x55,
0x89, 0xe5,
0xb8, 0x02, 0x00, 0x00, 0x00,
0xcd, 0x80,
0x5d,
0xc3
};
static const unsigned char vdso_getpid_stub[] = {
0x55,
0x89, 0xe5,
0xb8, 0x03, 0x00, 0x00, 0x00,
0xcd, 0x80,
0x5d,
0xc3
};
void
vdso_arch_init(void)
{
printf("VDSO: i386 architecture initialization\n");
printf("VDSO: i386 architecture ready\n");
}
kern_return_t
vdso_arch_setup_page(vm_offset_t page)
{
vdso_header_t *header;
unsigned char *code_area;
vm_offset_t offset;
printf("VDSO: Setting up i386 VDSO page at 0x%lx\n", page);
header = (vdso_header_t *)page;
code_area = (unsigned char *)(page + sizeof(vdso_header_t));
offset = sizeof(vdso_header_t);
if (offset + sizeof(vdso_gettimeofday_stub) < VDSO_PAGE_SIZE) {
memcpy(code_area, vdso_gettimeofday_stub, sizeof(vdso_gettimeofday_stub));
header->symbols[VDSO_SYM_GETTIMEOFDAY].offset = offset;
offset += sizeof(vdso_gettimeofday_stub);
code_area += sizeof(vdso_gettimeofday_stub);
}
if (offset + sizeof(vdso_clock_gettime_stub) < VDSO_PAGE_SIZE) {
memcpy(code_area, vdso_clock_gettime_stub, sizeof(vdso_clock_gettime_stub));
header->symbols[VDSO_SYM_CLOCK_GETTIME].offset = offset;
offset += sizeof(vdso_clock_gettime_stub);
code_area += sizeof(vdso_clock_gettime_stub);
}
if (offset + sizeof(vdso_time_stub) < VDSO_PAGE_SIZE) {
memcpy(code_area, vdso_time_stub, sizeof(vdso_time_stub));
header->symbols[VDSO_SYM_TIME].offset = offset;
offset += sizeof(vdso_time_stub);
code_area += sizeof(vdso_time_stub);
}
if (offset + sizeof(vdso_getpid_stub) < VDSO_PAGE_SIZE) {
memcpy(code_area, vdso_getpid_stub, sizeof(vdso_getpid_stub));
header->symbols[VDSO_SYM_GETPID].offset = offset;
offset += sizeof(vdso_getpid_stub);
}
printf("VDSO: i386 VDSO page setup complete, used %lu bytes\n",
offset - sizeof(vdso_header_t));
return KERN_SUCCESS;
}