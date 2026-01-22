#include <assert-backtrace.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <hurd/trivfs.h>
#include <hurd/ports.h>
#include <hurd/lookup.h>
#include <pthread.h>
#include <elf.h>
#include <link.h>
#include <fcntl.h>
#include "exec_S.h"
#ifndef exec_priv_h
#define exec_priv_h
struct bootinfo
{
struct port_info pi;
vm_address_t stack_base;
vm_size_t stack_size;
int flags;
char *argv, *envp;
size_t argvlen, envplen, dtablesize, nports, nints;
mach_port_t *dtable, *portarray;
int *intarray;
vm_address_t phdr_addr, user_entry;
vm_size_t phdr_size;
};
typedef struct bootinfo *bootinfo_t;
extern struct port_bucket *port_bucket;
extern struct port_class *execboot_portclass;
extern mach_port_t procserver;
typedef void asection;
struct execdata
{
error_t error;
vm_address_t entry;
file_t file;
vm_address_t start_code;
vm_address_t end_code;
char *map_buffer;
size_t map_vsize;
size_t map_fsize;
off_t map_filepos;
#define map_buffer(e)	((e)->map_buffer)
#define map_fsize(e)	((e)->map_fsize)
#define map_vsize(e)	((e)->map_vsize)
#define map_filepos(e)	((e)->map_filepos)
#define map_set_fsize(e, fsize) ((e)->map_fsize = (fsize))
union
{
asection *section;
const ElfW(Phdr) *phdr;
} interp;
memory_object_t filemap, cntlmap;
struct shared_io *cntl;
char *file_data;
off_t file_size;
size_t optimal_block;
task_t task;
union
{
struct
{
ElfW(Phdr) *phdr;
ElfW(Addr) phdr_addr;
ElfW(Word) phnum;
int anywhere;
vm_address_t loadbase;
int execstack;
} elf;
} info;
};
error_t elf_machine_matches_host (ElfW(Half) e_machine);
void finish (struct execdata *, int dealloc_file_port);
void *map (struct execdata *e, off_t posn, size_t len);
void check_hashbang (struct execdata *e,
file_t file,
task_t oldtask,
int flags,
const char *filename,
char *argv, mach_msg_type_number_t argvlen, boolean_t argv_copy,
char *envp, mach_msg_type_number_t envplen, boolean_t envp_copy,
mach_port_t *dtable, mach_msg_type_number_t dtablesize,
boolean_t dtable_copy,
mach_port_t *portarray, mach_msg_type_number_t nports,
boolean_t portarray_copy,
int *intarray, mach_msg_type_number_t nints, boolean_t intarray_copy,
const mach_port_t *deallocnames, mach_msg_type_number_t ndeallocnames,
const mach_port_t *destroynames, mach_msg_type_number_t ndestroynames);
extern mach_port_t *std_ports;
extern int *std_ints;
extern size_t std_nports, std_nints;
extern pthread_rwlock_t std_lock;
#endif