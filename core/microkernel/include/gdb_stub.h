#ifndef _GDB_STUB_H_
#define _GDB_STUB_H_
#include <mach/boolean.h>
#include <mach/mach_types.h>
#include <machine/thread.h>
#define GDB_PACKET_SIZE     1024
#define GDB_BUFFER_MAX      2048
typedef enum {
GDB_STATE_DISCONNECTED,
GDB_STATE_CONNECTED,
GDB_STATE_RUNNING,
GDB_STATE_STOPPED
} gdb_state_t;
#define GDB_CMD_READ_REGISTERS      'g'
#define GDB_CMD_WRITE_REGISTERS     'G'
#define GDB_CMD_READ_MEMORY         'm'
#define GDB_CMD_WRITE_MEMORY        'M'
#define GDB_CMD_CONTINUE            'c'
#define GDB_CMD_STEP                's'
#define GDB_CMD_SET_BREAKPOINT      'Z'
#define GDB_CMD_REMOVE_BREAKPOINT   'z'
#define GDB_CMD_QUERY               'q'
#define GDB_CMD_KILL                'k'
#define GDB_CMD_VCONT              'v'
#define GDB_CMD_THREAD_INFO        'T'
#define GDB_CMD_EXTENDED           '!'
#define GDB_PACKET_START           '$'
#define GDB_PACKET_END             '#'
#define GDB_ACK                    '+'
#define GDB_NAK                    '-'
#define GDB_SIGNAL_TRAP            5
#define GDB_SIGNAL_SEGV            11
#define GDB_SIGNAL_TERM            15
typedef enum {
GDB_BP_SOFTWARE    = 0,
GDB_BP_HARDWARE    = 1,
GDB_BP_WRITE_WATCH = 2,
GDB_BP_READ_WATCH  = 3,
GDB_BP_ACCESS_WATCH = 4
} gdb_breakpoint_type_t;
struct gdb_stub_config {
boolean_t enabled;
boolean_t hardware_breakpoints;
boolean_t software_breakpoints;
boolean_t watchpoints;
boolean_t multiprocess;
boolean_t thread_aware;
int max_breakpoints;
int max_watchpoints;
};
extern void gdb_stub_init(void);
extern void gdb_stub_configure(const struct gdb_stub_config *config);
extern void gdb_stub_enable(boolean_t enable);
extern boolean_t gdb_stub_is_enabled(void);
extern void gdb_stub_handle_exception(int exception_type,
struct i386_saved_state *state);
extern boolean_t gdb_stub_set_breakpoint(gdb_breakpoint_type_t type,
vm_offset_t address,
vm_size_t length);
extern boolean_t gdb_stub_remove_breakpoint(gdb_breakpoint_type_t type,
vm_offset_t address,
vm_size_t length);
extern boolean_t gdb_stub_should_break(void);
extern void gdb_stub_send_signal(int signal);
extern boolean_t gdb_stub_hw_breakpoint_available(void);
extern boolean_t gdb_stub_set_hw_breakpoint(vm_offset_t address,
gdb_breakpoint_type_t type);
extern boolean_t gdb_stub_remove_hw_breakpoint(vm_offset_t address);
extern void gdb_stub_thread_create(thread_t thread);
extern void gdb_stub_thread_destroy(thread_t thread);
extern void gdb_stub_thread_switch(thread_t old_thread, thread_t new_thread);
extern boolean_t gdb_stub_memory_valid(vm_offset_t address, vm_size_t length);
extern void gdb_stub_memory_changed(vm_offset_t address, vm_size_t length);
#define GDB_STUB_DEFAULT_CONFIG {       \
.enabled = FALSE,                   \
.hardware_breakpoints = TRUE,       \
.software_breakpoints = TRUE,       \
.watchpoints = TRUE,                \
.multiprocess = FALSE,              \
.thread_aware = TRUE,               \
.max_breakpoints = 4,               \
.max_watchpoints = 4                \
}
#define GDB_MAX_HW_BREAKPOINTS 4
#define GDB_MAX_HW_WATCHPOINTS 4
struct gdb_hw_breakpoint {
boolean_t active;
vm_offset_t address;
gdb_breakpoint_type_t type;
vm_size_t length;
int dr_index;
};
struct gdb_stub_stats {
unsigned int packets_sent;
unsigned int packets_received;
unsigned int exceptions_handled;
unsigned int breakpoints_hit;
unsigned int commands_processed;
unsigned int errors;
};
extern void gdb_stub_set_dr0(vm_offset_t value);
extern void gdb_stub_set_dr1(vm_offset_t value);
extern void gdb_stub_set_dr2(vm_offset_t value);
extern void gdb_stub_set_dr3(vm_offset_t value);
extern void gdb_stub_set_dr7(unsigned long value);
extern vm_offset_t gdb_stub_get_dr0(void);
extern vm_offset_t gdb_stub_get_dr1(void);
extern vm_offset_t gdb_stub_get_dr2(void);
extern vm_offset_t gdb_stub_get_dr3(void);
extern unsigned long gdb_stub_get_dr6(void);
extern unsigned long gdb_stub_get_dr7(void);
extern void gdb_stub_set_dr6(unsigned long value);
extern void gdb_stub_get_stats(struct gdb_stub_stats *stats);
extern void gdb_stub_reset_stats(void);
extern void gdb_stub_send_thread_info(void);
extern void gdb_stub_send_register_info(void);
extern void gdb_stub_send_memory_map(void);
extern void gdb_stub_send_packet(const char *data);
extern void gdb_stub_send_ok(void);
extern void gdb_stub_send_error(int error_code);
extern int gdb_stub_receive_packet(char *buffer, int max_len);
extern void gdb_stub_process_packet(const char *packet);
extern void gdb_stub_putchar(int c);
extern int gdb_stub_getchar(void);
extern boolean_t gdb_stub_char_available(void);
#endif