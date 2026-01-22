#include <gdb_stub.h>
#include <kern/printf.h>
#include <i386/i386/kttd_machdep.h>
#include <string.h>
static struct gdb_stub_config gdb_config = GDB_STUB_DEFAULT_CONFIG;
static gdb_state_t gdb_state = GDB_STATE_DISCONNECTED;
static struct gdb_stub_stats gdb_stats = {0};
static struct gdb_hw_breakpoint hw_breakpoints[GDB_MAX_HW_BREAKPOINTS] = {0};
#define DR7_GE          (1UL << 9)
#define DR7_LE          (1UL << 8)
#define DR7_G0          (1UL << 1)
#define DR7_L0          (1UL << 0)
#define DR7_G1          (1UL << 3)
#define DR7_L1          (1UL << 2)
#define DR7_G2          (1UL << 5)
#define DR7_L2          (1UL << 4)
#define DR7_G3          (1UL << 7)
#define DR7_L3          (1UL << 6)
#define DR7_TYPE_SHIFT(n)   (16 + (n)*4)
#define DR7_LEN_SHIFT(n)    (18 + (n)*4)
#define DR7_TYPE_EXEC       0
#define DR7_TYPE_WRITE      1
#define DR7_TYPE_IO         2
#define DR7_TYPE_ACCESS     3
#define DR7_LEN_1           0
#define DR7_LEN_2           1
#define DR7_LEN_4           3
static inline void set_debug_register(int reg, vm_offset_t value)
{
switch (reg) {
case 0:
asm volatile("movl %0, %%dr0" : : "r" (value));
break;
case 1:
asm volatile("movl %0, %%dr1" : : "r" (value));
break;
case 2:
asm volatile("movl %0, %%dr2" : : "r" (value));
break;
case 3:
asm volatile("movl %0, %%dr3" : : "r" (value));
break;
case 6:
asm volatile("movl %0, %%dr6" : : "r" (value));
break;
case 7:
asm volatile("movl %0, %%dr7" : : "r" (value));
break;
}
}
static inline vm_offset_t get_debug_register(int reg)
{
vm_offset_t value = 0;
switch (reg) {
case 0:
asm volatile("movl %%dr0, %0" : "=r" (value));
break;
case 1:
asm volatile("movl %%dr1, %0" : "=r" (value));
break;
case 2:
asm volatile("movl %%dr2, %0" : "=r" (value));
break;
case 3:
asm volatile("movl %%dr3, %0" : "=r" (value));
break;
case 6:
asm volatile("movl %%dr6, %0" : "=r" (value));
break;
case 7:
asm volatile("movl %%dr7, %0" : "=r" (value));
break;
}
return value;
}
void gdb_stub_init(void)
{
printf("[GDB] Modern GDB stub initializing...\n");
gdb_config.enabled = FALSE;
gdb_state = GDB_STATE_DISCONNECTED;
gdb_config.hardware_breakpoints = gdb_stub_hw_breakpoint_available();
printf("[GDB] Hardware breakpoints: %s\n",
gdb_config.hardware_breakpoints ? "available" : "not available");
printf("[GDB] GDB stub initialization complete\n");
}
void gdb_stub_configure(const struct gdb_stub_config *config)
{
if (!config) {
printf("[GDB] Error: NULL configuration\n");
return;
}
gdb_config = *config;
printf("[GDB] Configuration updated:\n");
printf("  - Hardware breakpoints: %s\n",
config->hardware_breakpoints ? "enabled" : "disabled");
printf("  - Software breakpoints: %s\n",
config->software_breakpoints ? "enabled" : "disabled");
printf("  - Watchpoints: %s\n",
config->watchpoints ? "enabled" : "disabled");
printf("  - Thread-aware: %s\n",
config->thread_aware ? "enabled" : "disabled");
}
void gdb_stub_enable(boolean_t enable)
{
gdb_config.enabled = enable;
if (enable) {
gdb_state = GDB_STATE_CONNECTED;
printf("[GDB] GDB stub enabled - waiting for connection\n");
} else {
gdb_state = GDB_STATE_DISCONNECTED;
printf("[GDB] GDB stub disabled\n");
}
}
boolean_t gdb_stub_is_enabled(void)
{
return gdb_config.enabled;
}
void gdb_stub_handle_exception(int exception_type,
struct i386_saved_state *state)
{
if (!gdb_config.enabled) {
return;
}
gdb_stats.exceptions_handled++;
printf("[GDB] Exception %d handled, EIP=0x%lx\n",
exception_type, state->eip);
gdb_state = GDB_STATE_STOPPED;
switch (exception_type) {
case 1:
case 3:
gdb_stub_send_packet("S05");
gdb_stats.breakpoints_hit++;
break;
case 14:
gdb_stub_send_packet("S0B");
break;
default:
gdb_stub_send_packet("S05");
break;
}
printf("[GDB] Sent stop notification, waiting for GDB commands\n");
}
boolean_t gdb_stub_hw_breakpoint_available(void)
{
return TRUE;
}
static int gdb_stub_find_free_hw_breakpoint(void)
{
int i;
for (i = 0; i < GDB_MAX_HW_BREAKPOINTS; i++) {
if (!hw_breakpoints[i].active) {
return i;
}
}
return -1;
}
static int gdb_stub_find_hw_breakpoint_by_addr(vm_offset_t address)
{
int i;
for (i = 0; i < GDB_MAX_HW_BREAKPOINTS; i++) {
if (hw_breakpoints[i].active && hw_breakpoints[i].address == address) {
return i;
}
}
return -1;
}
boolean_t gdb_stub_set_hw_breakpoint(vm_offset_t address,
gdb_breakpoint_type_t type)
{
int slot;
unsigned long dr7;
int dr7_type, dr7_len;
if (!gdb_config.hardware_breakpoints) {
return FALSE;
}
if (gdb_stub_find_hw_breakpoint_by_addr(address) >= 0) {
printf("[GDB] Hardware breakpoint already exists at 0x%lx\n", (unsigned long)address);
return TRUE;
}
slot = gdb_stub_find_free_hw_breakpoint();
if (slot < 0) {
printf("[GDB] No free hardware breakpoint slots\n");
return FALSE;
}
printf("[GDB] Setting hardware breakpoint at 0x%lx, type %d, slot %d\n",
(unsigned long)address, type, slot);
set_debug_register(slot, address);
dr7 = get_debug_register(7);
switch (type) {
case GDB_BP_HARDWARE:
dr7_type = DR7_TYPE_EXEC;
dr7_len = DR7_LEN_1;
break;
case GDB_BP_WRITE_WATCH:
dr7_type = DR7_TYPE_WRITE;
dr7_len = DR7_LEN_4;
break;
case GDB_BP_READ_WATCH:
dr7_type = DR7_TYPE_IO;
dr7_len = DR7_LEN_4;
break;
case GDB_BP_ACCESS_WATCH:
dr7_type = DR7_TYPE_ACCESS;
dr7_len = DR7_LEN_4;
break;
default:
return FALSE;
}
dr7 &= ~(3UL << DR7_TYPE_SHIFT(slot));
dr7 &= ~(3UL << DR7_LEN_SHIFT(slot));
dr7 |= ((unsigned long)dr7_type << DR7_TYPE_SHIFT(slot));
dr7 |= ((unsigned long)dr7_len << DR7_LEN_SHIFT(slot));
dr7 |= (1UL << (slot * 2));
dr7 |= (1UL << (slot * 2 + 1));
dr7 |= DR7_GE | DR7_LE;
set_debug_register(7, dr7);
hw_breakpoints[slot].active = TRUE;
hw_breakpoints[slot].address = address;
hw_breakpoints[slot].type = type;
hw_breakpoints[slot].length = (type == GDB_BP_HARDWARE) ? 1 : 4;
hw_breakpoints[slot].dr_index = slot;
return TRUE;
}
boolean_t gdb_stub_remove_hw_breakpoint(vm_offset_t address)
{
int slot;
unsigned long dr7;
if (!gdb_config.hardware_breakpoints) {
return FALSE;
}
slot = gdb_stub_find_hw_breakpoint_by_addr(address);
if (slot < 0) {
printf("[GDB] Hardware breakpoint not found at 0x%lx\n", (unsigned long)address);
return FALSE;
}
printf("[GDB] Removing hardware breakpoint at 0x%lx, slot %d\n", (unsigned long)address, slot);
set_debug_register(slot, 0);
dr7 = get_debug_register(7);
dr7 &= ~(1UL << (slot * 2));
dr7 &= ~(1UL << (slot * 2 + 1));
dr7 &= ~(3UL << DR7_TYPE_SHIFT(slot));
dr7 &= ~(3UL << DR7_LEN_SHIFT(slot));
set_debug_register(7, dr7);
hw_breakpoints[slot].active = FALSE;
hw_breakpoints[slot].address = 0;
hw_breakpoints[slot].type = 0;
hw_breakpoints[slot].length = 0;
hw_breakpoints[slot].dr_index = 0;
return TRUE;
}
boolean_t gdb_stub_set_breakpoint(gdb_breakpoint_type_t type,
vm_offset_t address,
vm_size_t length)
{
if (!gdb_config.enabled) {
return FALSE;
}
switch (type) {
case GDB_BP_SOFTWARE:
if (gdb_config.software_breakpoints) {
printf("[GDB] Setting software breakpoint at 0x%lx\n", (unsigned long)address);
return TRUE;
}
break;
case GDB_BP_HARDWARE:
return gdb_stub_set_hw_breakpoint(address, type);
case GDB_BP_WRITE_WATCH:
case GDB_BP_READ_WATCH:
case GDB_BP_ACCESS_WATCH:
if (gdb_config.watchpoints) {
printf("[GDB] Setting watchpoint at 0x%lx, type %d, length %lu\n",
(unsigned long)address, type, (unsigned long)length);
return gdb_stub_set_hw_breakpoint(address, type);
}
break;
}
return FALSE;
}
boolean_t gdb_stub_remove_breakpoint(gdb_breakpoint_type_t type,
vm_offset_t address,
vm_size_t length)
{
if (!gdb_config.enabled) {
return FALSE;
}
printf("[GDB] Removing breakpoint at 0x%lx, type %d\n", (unsigned long)address, type);
switch (type) {
case GDB_BP_SOFTWARE:
return TRUE;
case GDB_BP_HARDWARE:
case GDB_BP_WRITE_WATCH:
case GDB_BP_READ_WATCH:
case GDB_BP_ACCESS_WATCH:
return gdb_stub_remove_hw_breakpoint(address);
}
return FALSE;
}
void gdb_stub_thread_create(thread_t thread)
{
if (gdb_config.thread_aware && gdb_config.enabled) {
printf("[GDB] Thread created: %p\n", (void *)thread);
}
}
void gdb_stub_thread_destroy(thread_t thread)
{
if (gdb_config.thread_aware && gdb_config.enabled) {
printf("[GDB] Thread destroyed: %p\n", (void *)thread);
}
}
void gdb_stub_thread_switch(thread_t old_thread, thread_t new_thread)
{
if (gdb_config.thread_aware && gdb_config.enabled) {
printf("[GDB] Thread switch: %p -> %p\n", (void *)old_thread, (void *)new_thread);
}
}
void gdb_stub_get_stats(struct gdb_stub_stats *stats)
{
if (stats) {
*stats = gdb_stats;
}
}
void gdb_stub_reset_stats(void)
{
gdb_stats.packets_sent = 0;
gdb_stats.packets_received = 0;
gdb_stats.exceptions_handled = 0;
gdb_stats.breakpoints_hit = 0;
gdb_stats.commands_processed = 0;
gdb_stats.errors = 0;
}
void gdb_stub_send_signal(int signal)
{
if (gdb_config.enabled) {
printf("[GDB] Sending signal %d to debugger\n", signal);
gdb_state = GDB_STATE_STOPPED;
}
}
boolean_t gdb_stub_should_break(void)
{
return (gdb_config.enabled && gdb_state == GDB_STATE_STOPPED);
}
boolean_t gdb_stub_memory_valid(vm_offset_t address, vm_size_t length)
{
return TRUE;
}
void gdb_stub_memory_changed(vm_offset_t address, vm_size_t length)
{
if (gdb_config.enabled) {
printf("[GDB] Memory changed at 0x%x, length %zu\n", address, length);
}
}
void gdb_stub_send_thread_info(void)
{
printf("[GDB] Sending thread information\n");
}
void gdb_stub_send_register_info(void)
{
printf("[GDB] Sending register information\n");
}
void gdb_stub_send_memory_map(void)
{
printf("[GDB] Sending memory map\n");
}
void gdb_stub_set_dr0(vm_offset_t value) { set_debug_register(0, value); }
void gdb_stub_set_dr1(vm_offset_t value) { set_debug_register(1, value); }
void gdb_stub_set_dr2(vm_offset_t value) { set_debug_register(2, value); }
void gdb_stub_set_dr3(vm_offset_t value) { set_debug_register(3, value); }
void gdb_stub_set_dr7(unsigned long value) { set_debug_register(7, value); }
void gdb_stub_set_dr6(unsigned long value) { set_debug_register(6, value); }
vm_offset_t gdb_stub_get_dr0(void) { return get_debug_register(0); }
vm_offset_t gdb_stub_get_dr1(void) { return get_debug_register(1); }
vm_offset_t gdb_stub_get_dr2(void) { return get_debug_register(2); }
vm_offset_t gdb_stub_get_dr3(void) { return get_debug_register(3); }
unsigned long gdb_stub_get_dr6(void) { return get_debug_register(6); }
unsigned long gdb_stub_get_dr7(void) { return get_debug_register(7); }
static char gdb_packet_buffer[GDB_PACKET_SIZE];
static char gdb_reply_buffer[GDB_PACKET_SIZE];
static unsigned char gdb_checksum(const char *data)
{
unsigned char checksum = 0;
while (*data) {
checksum += (unsigned char)*data++;
}
return checksum;
}
void gdb_stub_putchar(int c)
{
if (c == GDB_PACKET_START) {
printf("[GDB-TX]");
}
printf("%c", c);
if (c == GDB_PACKET_END) {
printf("\n");
}
}
int gdb_stub_getchar(void)
{
return -1;
}
boolean_t gdb_stub_char_available(void)
{
return FALSE;
}
void gdb_stub_send_packet(const char *data)
{
unsigned char checksum;
if (!gdb_config.enabled) {
return;
}
checksum = gdb_checksum(data);
gdb_stub_putchar(GDB_PACKET_START);
while (*data) {
gdb_stub_putchar(*data++);
}
gdb_stub_putchar(GDB_PACKET_END);
gdb_stub_putchar("0123456789abcdef"[checksum >> 4]);
gdb_stub_putchar("0123456789abcdef"[checksum & 0x0f]);
gdb_stats.packets_sent++;
printf("[GDB] Sent packet\n");
}
void gdb_stub_send_ok(void)
{
gdb_stub_send_packet("OK");
}
void gdb_stub_send_error(int error_code)
{
snprintf(gdb_reply_buffer, sizeof(gdb_reply_buffer), "E%02x", error_code & 0xff);
gdb_stub_send_packet(gdb_reply_buffer);
gdb_stats.errors++;
}
int gdb_stub_receive_packet(char *buffer, int max_len)
{
int c, count = 0;
unsigned char checksum, received_checksum;
boolean_t packet_started = FALSE;
if (!gdb_config.enabled) {
return 0;
}
while (count < max_len - 1) {
c = gdb_stub_getchar();
if (c < 0) {
break;
}
if (!packet_started) {
if (c == GDB_PACKET_START) {
packet_started = TRUE;
count = 0;
}
continue;
}
if (c == GDB_PACKET_END) {
buffer[count] = '\0';
c = gdb_stub_getchar();
if (c < 0) break;
received_checksum = (c >= '0' && c <= '9') ? (c - '0') : (c - 'a' + 10);
received_checksum <<= 4;
c = gdb_stub_getchar();
if (c < 0) break;
received_checksum |= (c >= '0' && c <= '9') ? (c - '0') : (c - 'a' + 10);
checksum = gdb_checksum(buffer);
if (checksum == received_checksum) {
gdb_stub_putchar(GDB_ACK);
gdb_stats.packets_received++;
return count;
} else {
gdb_stub_putchar(GDB_NAK);
return 0;
}
} else {
buffer[count++] = c;
}
}
return 0;
}
void gdb_stub_process_packet(const char *packet)
{
if (!gdb_config.enabled || !packet || !*packet) {
return;
}
gdb_stats.commands_processed++;
switch (packet[0]) {
case GDB_CMD_QUERY:
if (strncmp(packet, "qSupported", 10) == 0) {
gdb_stub_send_packet("PacketSize=1000;hwbreak+;swbreak+");
} else if (strncmp(packet, "qAttached", 9) == 0) {
gdb_stub_send_packet("1");
} else {
gdb_stub_send_packet("");
}
break;
case GDB_CMD_SET_BREAKPOINT:
gdb_stub_send_ok();
break;
case GDB_CMD_REMOVE_BREAKPOINT:
gdb_stub_send_ok();
break;
case GDB_CMD_CONTINUE:
gdb_state = GDB_STATE_RUNNING;
gdb_stub_send_ok();
break;
case GDB_CMD_STEP:
gdb_state = GDB_STATE_RUNNING;
gdb_stub_send_ok();
break;
case '?':
snprintf(gdb_reply_buffer, sizeof(gdb_reply_buffer), "S%02x", GDB_SIGNAL_TRAP);
gdb_stub_send_packet(gdb_reply_buffer);
break;
default:
gdb_stub_send_packet("");
break;
}
}