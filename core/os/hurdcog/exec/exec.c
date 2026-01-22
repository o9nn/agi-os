#include "priv.h"
#include <mach/gnumach.h>
#include <mach/vm_param.h>
#include <hurd.h>
#include <hurd/exec.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <unistd.h>
#include <stdbool.h>
mach_port_t procserver;
mach_port_t *std_ports;
int *std_ints;
size_t std_nports, std_nints;
pthread_rwlock_t std_lock = PTHREAD_RWLOCK_INITIALIZER;
#define	b2he()	a2he (errno)
#include <hurd/sigpreempt.h>
static void
load_section (void *section, struct execdata *u)
{
vm_address_t addr = 0;
vm_offset_t filepos = 0;
vm_size_t filesz = 0, memsz = 0;
vm_prot_t vm_prot;
vm_address_t mask = 0;
const ElfW(Phdr) *const ph = section;
if (u->error)
return;
vm_prot = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE;
addr = ph->p_vaddr & ~(ph->p_align - 1);
memsz = ph->p_vaddr + ph->p_memsz - addr;
filepos = ph->p_offset & ~(ph->p_align - 1);
filesz = ph->p_offset + ph->p_filesz - filepos;
if ((ph->p_flags & PF_R) == 0)
vm_prot &= ~VM_PROT_READ;
if ((ph->p_flags & PF_W) == 0)
vm_prot &= ~VM_PROT_WRITE;
if ((ph->p_flags & PF_X) == 0)
vm_prot &= ~VM_PROT_EXECUTE;
assert_backtrace (!u->info.elf.anywhere);
addr += u->info.elf.loadbase;
if (memsz == 0)
return;
if (filesz != 0)
{
vm_address_t mapstart = round_page (addr);
void write_to_task (vm_address_t * mapstart, vm_size_t size,
vm_prot_t vm_prot, vm_address_t contents)
{
vm_size_t off = size % vm_page_size;
u->error = vm_map (u->task,
mapstart, size, mask, 0,
MACH_PORT_NULL, 0, 1,
vm_prot|VM_PROT_WRITE,
VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE,
VM_INHERIT_COPY);
if (! u->error && size >= vm_page_size)
u->error = vm_write (u->task, *mapstart, contents, size - off);
if (! u->error && off != 0)
{
vm_address_t page = 0;
page = (vm_address_t) mmap (0, vm_page_size,
PROT_READ|PROT_WRITE, MAP_ANON,
0, 0);
u->error = (page == -1) ? errno : 0;
if (! u->error)
{
u->error = hurd_safe_copyin ((void *) page,
(void *) (contents + (size - off)),
off);
if (! u->error)
u->error = vm_write (u->task, *mapstart + (size - off),
page, vm_page_size);
munmap ((caddr_t) page, vm_page_size);
}
}
if (! u->error && (vm_prot & VM_PROT_WRITE) == 0)
u->error = vm_protect (u->task, *mapstart, size, 0, vm_prot);
}
if (mapstart - addr < filesz)
{
#define SECTION_IN_MEMORY_P	(u->file_data != NULL)
#define SECTION_CONTENTS	(u->file_data + filepos)
if (SECTION_IN_MEMORY_P)
write_to_task (&mapstart, filesz - (mapstart - addr), vm_prot,
(vm_address_t) SECTION_CONTENTS
+ (mapstart - addr));
else if (u->filemap != MACH_PORT_NULL)
u->error = vm_map (u->task,
&mapstart, filesz - (mapstart - addr),
mask, 0,
u->filemap, filepos + (mapstart - addr), 1,
vm_prot,
VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE,
VM_INHERIT_COPY);
else
{
const vm_size_t size = filesz - (mapstart - addr);
void *buf = map (u, filepos + (mapstart - addr), size);
if (buf)
write_to_task (&mapstart, size, vm_prot, (vm_address_t) buf);
}
if (u->error)
return;
}
if (vm_prot & VM_PROT_EXECUTE)
{
if (u->start_code == 0 || u->start_code > addr)
u->start_code = addr;
if (u->end_code < addr + memsz)
u->end_code = addr + memsz;
}
if (mapstart > addr)
{
vm_address_t overlap_page = trunc_page (addr);
vm_address_t ourpage = 0;
mach_msg_type_number_t size = 0;
void *readaddr;
size_t readsize;
u->error = vm_read (u->task, overlap_page, vm_page_size,
&ourpage, &size);
if (u->error)
{
if (u->error == KERN_INVALID_ADDRESS)
{
u->error = vm_allocate (u->task,
&overlap_page, vm_page_size, 0);
size = vm_page_size;
if (!u->error)
{
ourpage = (vm_address_t) mmap (0, vm_page_size,
PROT_READ|PROT_WRITE,
MAP_ANON, 0, 0);
u->error = (ourpage == -1) ? errno : 0;
}
}
if (u->error)
{
maplose:
vm_deallocate (u->task, mapstart, filesz);
return;
}
}
readaddr = (void *) (ourpage + (addr - overlap_page));
readsize = size - (addr - overlap_page);
if (readsize > filesz)
readsize = filesz;
if (SECTION_IN_MEMORY_P)
memcpy (readaddr, SECTION_CONTENTS, readsize);
else
{
const void *contents = map (u, filepos, readsize);
if (!contents)
goto maplose;
u->error = hurd_safe_copyin (readaddr, contents,
readsize);
if (u->error)
goto maplose;
}
u->error = vm_write (u->task, overlap_page, ourpage, size);
if (u->error == KERN_PROTECTION_FAILURE)
{
u->error = vm_protect (u->task, overlap_page, size,
0, vm_prot | VM_PROT_WRITE);
if (!u->error)
u->error = vm_write (u->task, overlap_page, ourpage, size);
if (!u->error && !(vm_prot & VM_PROT_WRITE))
u->error = vm_protect (u->task, overlap_page, size,
0, vm_prot);
}
munmap ((caddr_t) ourpage, size);
if (u->error)
goto maplose;
}
if (u->cntl)
u->cntl->accessed = 1;
addr += filesz;
memsz -= filesz;
}
if (memsz != 0)
{
vm_address_t mapstart = round_page (addr);
if (mapstart - addr < memsz)
{
u->error = vm_map (u->task, &mapstart, memsz - (mapstart - addr),
mask, 0, MACH_PORT_NULL, 0, 1,
vm_prot, VM_PROT_ALL, VM_INHERIT_COPY);
if (u->error)
return;
}
if (mapstart > addr)
{
vm_address_t overlap_page = trunc_page (addr);
vm_address_t ourpage = 0;
mach_msg_type_number_t size = 0;
u->error = vm_read (u->task, overlap_page, vm_page_size,
&ourpage, &size);
if (u->error)
{
vm_deallocate (u->task, mapstart, memsz);
return;
}
u->error = hurd_safe_memset (
(void *) (ourpage + (addr - overlap_page)),
0,
size - (addr - overlap_page));
if (! u->error && !(vm_prot & VM_PROT_WRITE))
u->error = vm_protect (u->task, overlap_page, size,
0, VM_PROT_WRITE);
if (! u->error)
u->error = vm_write (u->task, overlap_page, ourpage, size);
if (! u->error && !(vm_prot & VM_PROT_WRITE))
u->error = vm_protect (u->task, overlap_page, size, 0, vm_prot);
munmap ((caddr_t) ourpage, size);
}
}
return;
}
void *
map (struct execdata *e, off_t posn, size_t len)
{
const size_t size = e->file_size;
size_t offset;
if ((map_filepos (e) & ~(map_vsize (e) - 1)) == (posn & ~(map_vsize (e) - 1))
&& posn + len - map_filepos (e) <= map_fsize (e))
offset = posn & (map_vsize (e) - 1);
else if (posn + len > size)
return NULL;
else if (e->file_data != NULL) {
return e->file_data + posn;
} else if (e->filemap == MACH_PORT_NULL)
{
char *buffer = map_buffer (e);
mach_msg_type_number_t nread = map_vsize (e);
assert_backtrace (e->file_data == NULL);
e->error = io_read (e->file, &buffer, &nread, posn, round_page (len));
if (e->error)
return NULL;
if (buffer != map_buffer (e))
{
if (map_vsize (e) != 0)
munmap (map_buffer (e), map_vsize (e));
map_buffer (e) = buffer;
map_vsize (e) = round_page (nread);
}
map_filepos (e) = posn;
map_set_fsize (e, nread);
offset = 0;
}
else
{
if (map_buffer (e) != NULL)
munmap (map_buffer (e), map_vsize (e));
map_buffer (e) = NULL;
offset = posn & (vm_page_size - 1);
map_filepos (e) = trunc_page (posn);
map_vsize (e) = round_page (posn + len) - map_filepos (e);
if (vm_map (mach_task_self (),
(vm_address_t *) &map_buffer (e), map_vsize (e), 0, 1,
e->filemap, map_filepos (e), 1, VM_PROT_READ, VM_PROT_READ,
VM_INHERIT_NONE))
{
e->error = EIO;
return NULL;
}
if (e->cntl)
e->cntl->accessed = 1;
map_set_fsize (e, MIN (map_vsize (e), size - map_filepos (e)));
}
return map_buffer (e) + offset;
}
static void
prepare_stream (struct execdata *e)
{
e->map_buffer = NULL;
e->map_vsize = e->map_fsize = 0;
e->map_filepos = 0;
}
static void
prepare (file_t file, struct execdata *e)
{
memory_object_t rd, wr;
e->file = file;
e->file_data = NULL;
e->cntl = NULL;
e->filemap = MACH_PORT_NULL;
e->cntlmap = MACH_PORT_NULL;
e->interp.section = NULL;
e->start_code = 0;
e->end_code = 0;
prepare_stream (e);
e->error = io_map (file, &rd, &wr);
if (! e->error)
{
if (wr != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), wr);
if (rd == MACH_PORT_NULL)
{
e->error = EBADF;
return;
}
e->filemap = rd;
e->error =  EOPNOTSUPP;
if (!e->error)
e->error = vm_map (mach_task_self (), (vm_address_t *) &e->cntl,
vm_page_size, 0, 1, e->cntlmap, 0, 0,
VM_PROT_READ|VM_PROT_WRITE,
VM_PROT_READ|VM_PROT_WRITE, VM_INHERIT_NONE);
if (e->cntl)
while (1)
{
pthread_spin_lock (&e->cntl->lock);
switch (e->cntl->conch_status)
{
case USER_COULD_HAVE_CONCH:
e->cntl->conch_status = USER_HAS_CONCH;
case USER_HAS_CONCH:
pthread_spin_unlock (&e->cntl->lock);
break;
case USER_RELEASE_CONCH:
case USER_HAS_NOT_CONCH:
default:
pthread_spin_unlock (&e->cntl->lock);
e->error = io_get_conch (e->file);
if (e->error)
return;
continue;
}
e->file_size = 0;
if (e->cntl->use_file_size)
e->file_size = e->cntl->file_size;
if (e->cntl->use_read_size && e->cntl->read_size > e->file_size)
e->file_size = e->cntl->read_size;
break;
}
}
if (!e->cntl && (!e->error || e->error == EOPNOTSUPP))
{
struct stat st;
e->error = io_stat (file, &st);
if (e->error)
return;
e->file_size = st.st_size;
e->optimal_block = st.st_blksize;
}
}
#include <endian.h>
#if BYTE_ORDER == BIG_ENDIAN
#define host_ELFDATA ELFDATA2MSB
#endif
#if BYTE_ORDER == LITTLE_ENDIAN
#define host_ELFDATA ELFDATA2LSB
#endif
#ifdef __LP64__
#define host_ELFCLASS ELFCLASS64
#else
#define host_ELFCLASS ELFCLASS32
#endif
static void
check_elf (struct execdata *e)
{
ElfW(Ehdr) *ehdr = map (e, 0, sizeof (ElfW(Ehdr)));
ElfW(Phdr) *phdr;
if (! ehdr)
{
if (!e->error)
e->error = ENOEXEC;
return;
}
if (*(ElfW(Word) *) ehdr != ((union { ElfW(Word) word;
unsigned char string[SELFMAG]; })
{ string: ELFMAG }).word)
{
e->error = ENOEXEC;
return;
}
if (ehdr->e_ident[EI_CLASS] != host_ELFCLASS ||
ehdr->e_ident[EI_DATA] != host_ELFDATA ||
ehdr->e_ident[EI_VERSION] != EV_CURRENT ||
ehdr->e_version != EV_CURRENT ||
ehdr->e_ehsize < sizeof *ehdr ||
ehdr->e_phentsize != sizeof (ElfW(Phdr)))
{
e->error = ENOEXEC;
return;
}
e->error = elf_machine_matches_host (ehdr->e_machine);
if (e->error)
return;
e->entry = ehdr->e_entry;
#ifdef ELIBEXEC
if (e->entry == 0)
{
e->error = ELIBEXEC;
return;
}
#endif
e->info.elf.anywhere = (ehdr->e_type == ET_DYN ||
ehdr->e_type == ET_REL);
e->info.elf.loadbase = 0;
e->info.elf.phnum = ehdr->e_phnum;
phdr = map (e, ehdr->e_phoff, ehdr->e_phnum * sizeof (ElfW(Phdr)));
if (! phdr)
{
if (!e->error)
e->error = ENOEXEC;
return;
}
e->info.elf.phdr = phdr;
e->info.elf.phdr_addr = ehdr->e_phoff;
}
static void
check_elf_phdr (struct execdata *e, const ElfW(Phdr) *mapped_phdr)
{
const ElfW(Phdr) *phdr;
bool seen_phdr = false;
memcpy (e->info.elf.phdr, mapped_phdr,
e->info.elf.phnum * sizeof (ElfW(Phdr)));
e->info.elf.execstack = 1;
for (phdr = e->info.elf.phdr;
phdr < &e->info.elf.phdr[e->info.elf.phnum];
++phdr)
switch (phdr->p_type)
{
case PT_INTERP:
e->interp.phdr = phdr;
break;
case PT_LOAD:
if (e->file_size <= (off_t) (phdr->p_offset +
phdr->p_filesz))
{
e->error = ENOEXEC;
return;
}
if (!seen_phdr
&& (phdr->p_offset & -phdr->p_align) == 0
&& phdr->p_offset <= e->info.elf.phdr_addr
&& e->info.elf.phdr_addr - phdr->p_offset < phdr->p_filesz)
{
e->info.elf.phdr_addr += phdr->p_vaddr - phdr->p_offset;
seen_phdr = true;
}
break;
case PT_GNU_STACK:
e->info.elf.execstack = phdr->p_flags & PF_X;
break;
}
if (!seen_phdr)
e->info.elf.phdr_addr = 0;
}
static void
check (struct execdata *e)
{
check_elf (e);
}
static void
finish_mapping (struct execdata *e)
{
if (e->cntl != NULL)
{
pthread_spin_lock (&e->cntl->lock);
if (e->cntl->conch_status == USER_RELEASE_CONCH)
{
pthread_spin_unlock (&e->cntl->lock);
io_release_conch (e->file);
}
else
{
e->cntl->conch_status = USER_HAS_NOT_CONCH;
pthread_spin_unlock (&e->cntl->lock);
}
munmap (e->cntl, vm_page_size);
e->cntl = NULL;
}
if (e->filemap != MACH_PORT_NULL)
{
mach_port_deallocate (mach_task_self (), e->filemap);
e->filemap = MACH_PORT_NULL;
}
if (e->cntlmap != MACH_PORT_NULL)
{
mach_port_deallocate (mach_task_self (), e->cntlmap);
e->cntlmap = MACH_PORT_NULL;
}
}
void
finish (struct execdata *e, int dealloc_file)
{
finish_mapping (e);
{
if (e->file_data != NULL) {
free (e->file_data);
e->file_data = NULL;
} else if (map_buffer (e) != NULL) {
munmap (map_buffer (e), map_vsize (e));
map_buffer (e) = NULL;
}
}
if (dealloc_file && e->file != MACH_PORT_NULL)
{
mach_port_deallocate (mach_task_self (), e->file);
e->file = MACH_PORT_NULL;
}
}
static void
set_name (task_t task, const char *exec_name, pid_t pid)
{
char *name;
int size;
if (pid)
size = asprintf (&name, "%s(%d)", exec_name, pid);
else
size = asprintf (&name, "%s", exec_name);
if (size == 0)
return;
#define TASK_NAME_SIZE	32
if (size < TASK_NAME_SIZE)
task_set_name (task, name);
else
{
char *abbr = name + size - TASK_NAME_SIZE + 1;
abbr[0] = abbr[1] = abbr[2] = '.';
task_set_name (task, abbr);
}
#undef TASK_NAME_SIZE
free (name);
}
static void
load (task_t usertask, struct execdata *e)
{
ElfW(Word) i;
e->task = usertask;
if (e->error)
goto out;
if (e->info.elf.anywhere)
{
vm_address_t mapping_size = 0;
vm_address_t anywhere_start = 0;
for (i = 0; i < e->info.elf.phnum; ++i)
{
ElfW(Phdr) *phdr = &e->info.elf.phdr[i];
if (phdr->p_type == PT_LOAD)
mapping_size = phdr->p_vaddr + phdr->p_memsz;
}
e->error = vm_allocate (usertask, &anywhere_start, mapping_size, 1);
if (e->error)
goto out;
e->info.elf.loadbase = anywhere_start;
e->info.elf.anywhere = 0;
e->error = vm_deallocate (usertask, anywhere_start, mapping_size);
if (e->error)
goto out;
}
for (i = 0; i < e->info.elf.phnum; ++i)
if (e->info.elf.phdr[i].p_type == PT_LOAD)
load_section (&e->info.elf.phdr[i], e);
e->entry += e->info.elf.loadbase;
out:
finish_mapping (e);
}
static inline void *
servercopy (void *arg, mach_msg_type_number_t argsize, boolean_t argcopy,
error_t *errorp)
{
if (! argcopy)
return arg;
if (! argsize)
return NULL;
void *copy;
copy = mmap (0, argsize, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (copy == MAP_FAILED)
{
*errorp = errno;
return NULL;
}
memcpy (copy, arg, argsize);
return copy;
}
static error_t
do_exec (file_t file,
task_t oldtask,
int flags,
const_string_t path,
const_string_t abspath,
char *argv, mach_msg_type_number_t argvlen, boolean_t argv_copy,
char *envp, mach_msg_type_number_t envplen, boolean_t envp_copy,
mach_port_t *dtable, mach_msg_type_number_t dtablesize,
boolean_t dtable_copy,
mach_port_t *portarray, mach_msg_type_number_t nports,
boolean_t portarray_copy,
int *intarray, mach_msg_type_number_t nints, boolean_t intarray_copy,
const mach_port_t *deallocnames, mach_msg_type_number_t ndeallocnames,
const mach_port_t *destroynames, mach_msg_type_number_t ndestroynames)
{
struct execdata e, interp;
task_t newtask = MACH_PORT_NULL;
thread_t thread = MACH_PORT_NULL;
struct bootinfo *boot = 0;
int *ports_replaced;
int secure, defaults;
mach_msg_type_number_t i;
int intarray_dealloc = 0;
int oldtask_trashed = 0;
void prepare_and_check (file_t file, struct execdata *e)
{
prepare (file, e);
if (e->error)
return;
check (e);
}
interp.file = MACH_PORT_NULL;
if ((!std_ports || !std_ints) && (flags & (EXEC_SECURE|EXEC_DEFAULTS)))
return EIEIO;
if (oldtask != MACH_PORT_NULL && (e.error = task_suspend (oldtask)))
return e.error;
prepare_and_check (file, &e);
if (e.error == ENOEXEC)
{
check_hashbang (&e,
file, oldtask, flags, path,
argv, argvlen, argv_copy,
envp, envplen, envp_copy,
dtable, dtablesize, dtable_copy,
portarray, nports, portarray_copy,
intarray, nints, intarray_copy,
deallocnames, ndeallocnames,
destroynames, ndestroynames);
if (! e.error)
return 0;
}
if (e.error)
goto out;
const ElfW(Phdr) *phdr = e.info.elf.phdr;
e.info.elf.phdr = alloca (e.info.elf.phnum * sizeof (ElfW(Phdr)));
check_elf_phdr (&e, phdr);
if (oldtask == MACH_PORT_NULL)
flags |= EXEC_NEWTASK;
if (flags & (EXEC_NEWTASK|EXEC_SECURE))
{
e.error = task_create (((flags & EXEC_SECURE) ||
oldtask == MACH_PORT_NULL) ?
mach_task_self () : oldtask,
#ifdef KERN_INVALID_LEDGER
NULL, 0,
#endif
0, &newtask);
if (e.error)
goto out;
}
else
newtask = oldtask;
pthread_rwlock_rdlock (&std_lock);
{
#define use(idx, new, reauth, consume) \
do { use1 (idx, new, reauth, consume); \
if (e.error) goto stdout; } while (0)
void use1 (unsigned int idx, mach_port_t new,
int reauth, int consume)
{
if (new != MACH_PORT_NULL && reauth)
{
mach_port_t ref = mach_reply_port (), authed;
e.error = io_reauthenticate (new, ref, MACH_MSG_TYPE_MAKE_SEND);
if (! e.error)
e.error = auth_user_authenticate
(boot->portarray[INIT_PORT_AUTH],
ref, MACH_MSG_TYPE_MAKE_SEND, &authed);
mach_port_destroy (mach_task_self (), ref);
if (e.error)
return;
new = authed;
}
else
{
if (!consume && new != MACH_PORT_NULL)
mach_port_mod_refs (mach_task_self (),
new, MACH_PORT_RIGHT_SEND, 1);
}
boot->portarray[idx] = new;
ports_replaced[idx] = 1;
}
e.error = ports_create_port (execboot_portclass, port_bucket,
sizeof *boot, &boot);
if (boot == NULL)
{
stdout:
pthread_rwlock_unlock (&std_lock);
goto out;
}
memset (&boot->pi + 1, 0, (char *) &boot[1] - (char *) (&boot->pi + 1));
secure = (flags & EXEC_SECURE);
defaults = (flags & EXEC_DEFAULTS);
boot->flags = flags;
argv = servercopy (argv, argvlen, argv_copy, &e.error);
if (e.error)
goto stdout;
boot->argv = argv;
boot->argvlen = argvlen;
if (abspath && abspath[0] == '/')
{
const char *end = strrchr (abspath, '/');
size_t pathlen;
const char ld_origin_s[] = "\0LD_ORIGIN_PATH=";
const char *existing;
size_t existing_len = 0;
size_t new_envplen;
char *new_envp;
while (end > abspath && end[-1] == '/')
end--;
if (end == abspath)
end++;
pathlen = end - abspath;
if (memcmp (envp, ld_origin_s + 1, sizeof (ld_origin_s) - 2) == 0)
existing = envp - 1;
else
existing = memmem (envp, envplen, ld_origin_s, sizeof (ld_origin_s) - 1);
if (existing)
{
existing += sizeof (ld_origin_s) - 1;
existing_len = strnlen (existing, envplen - (existing - envp));
new_envplen = envplen - existing_len + pathlen;
new_envp = mmap (0, new_envplen,
PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (new_envp == MAP_FAILED)
{
e.error = errno;
goto stdout;
}
memcpy (new_envp, envp, existing - envp);
memcpy (new_envp + (existing - envp), abspath, pathlen);
memcpy (new_envp + (existing - envp) + pathlen,
existing + existing_len,
envplen - ((existing - envp) + existing_len));
}
else
{
new_envplen = sizeof (ld_origin_s) - 1 + pathlen + envplen;
new_envp = mmap (0, new_envplen,
PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
memcpy (new_envp, ld_origin_s + 1, sizeof (ld_origin_s) - 2);
memcpy (new_envp + sizeof (ld_origin_s) - 2, abspath, pathlen);
new_envp [sizeof (ld_origin_s) - 2 + pathlen] = 0;
memcpy (new_envp + sizeof (ld_origin_s) - 2 + pathlen + 1, envp, envplen);
}
if (! envp_copy)
munmap (envp, envplen);
envp = new_envp;
envplen = new_envplen;
}
else
{
envp = servercopy (envp, envplen, envp_copy, &e.error);
if (e.error)
goto stdout;
}
boot->envp = envp;
boot->envplen = envplen;
dtable = servercopy (dtable, dtablesize * sizeof (mach_port_t),
dtable_copy, &e.error);
if (e.error)
goto stdout;
boot->dtable = dtable;
boot->dtablesize = dtablesize;
if ((secure || defaults) && nints < INIT_INT_MAX)
{
if (intarray_copy || (round_page (nints * sizeof (int)) <
round_page (INIT_INT_MAX * sizeof (int))))
{
boot->intarray = mmap (0, INIT_INT_MAX * sizeof (int),
PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
memcpy (boot->intarray, intarray, nints * sizeof (int));
intarray_dealloc = !intarray_copy;
}
else
boot->intarray = intarray;
boot->nints = INIT_INT_MAX;
}
else
{
intarray = servercopy (intarray, nints * sizeof (int), intarray_copy,
&e.error);
if (e.error)
goto stdout;
boot->intarray = intarray;
boot->nints = nints;
}
if (secure)
boot->intarray[INIT_UMASK] = std_ints ? std_ints[INIT_UMASK] : CMASK;
boot->nports = nports < INIT_PORT_MAX ? INIT_PORT_MAX : nports;
boot->portarray = mmap (0, boot->nports * sizeof (mach_port_t),
PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
for (i = 0; i < nports; ++i)
boot->portarray[i] = portarray[i];
if (MACH_PORT_NULL != 0)
for (; i < boot->nports; ++i)
boot->portarray[i] = MACH_PORT_NULL;
ports_replaced = alloca (boot->nports * sizeof *ports_replaced);
memset (ports_replaced, 0, boot->nports * sizeof *ports_replaced);
if (portarray[INIT_PORT_BOOTSTRAP] == MACH_PORT_NULL &&
oldtask != MACH_PORT_NULL)
{
if (! task_get_bootstrap_port (oldtask,
&boot->portarray[INIT_PORT_BOOTSTRAP]))
ports_replaced[INIT_PORT_BOOTSTRAP] = 1;
}
if ((secure || defaults)
&& boot->portarray[INIT_PORT_AUTH] == MACH_PORT_NULL)
use (INIT_PORT_AUTH, std_ports[INIT_PORT_AUTH], 0, 0);
if (secure || (defaults
&& boot->portarray[INIT_PORT_PROC] == MACH_PORT_NULL))
{
mach_port_t new;
e.error = proc_task2proc (procserver, newtask, &new);
if (e.error)
goto stdout;
use (INIT_PORT_PROC, new, 0, 1);
}
else if (oldtask != newtask && oldtask != MACH_PORT_NULL
&& boot->portarray[INIT_PORT_PROC] != MACH_PORT_NULL)
{
mach_port_t new;
e.error = proc_task2proc (boot->portarray[INIT_PORT_PROC],
newtask, &new);
if (e.error)
goto stdout;
use (INIT_PORT_PROC, new, 0, 1);
}
if (secure || (defaults
&& boot->portarray[INIT_PORT_CRDIR] == MACH_PORT_NULL))
use (INIT_PORT_CRDIR, std_ports[INIT_PORT_CRDIR], 1, 0);
if ((secure || defaults)
&& boot->portarray[INIT_PORT_CWDIR] == MACH_PORT_NULL)
use (INIT_PORT_CWDIR, std_ports[INIT_PORT_CWDIR], 1, 0);
}
pthread_rwlock_unlock (&std_lock);
if (! e.error && e.interp.section)
{
char *name = map (&e, (e.interp.phdr->p_offset
& ~(e.interp.phdr->p_align - 1)),
e.interp.phdr->p_filesz);
if (! name && ! e.error)
e.error = ENOEXEC;
if (! name)
e.interp.section = NULL;
else
{
error_t user_port (int which, error_t (*operate) (mach_port_t))
{
return (*operate) (boot->nports > which ?
boot->portarray[which] :
MACH_PORT_NULL);
}
file_t user_fd (int fd)
{
if (fd < 0 || fd >= boot->dtablesize ||
boot->dtable[fd] == MACH_PORT_NULL)
{
errno = EBADF;
return MACH_PORT_NULL;
}
mach_port_mod_refs (mach_task_self (), boot->dtable[fd],
MACH_PORT_RIGHT_SEND, +1);
return boot->dtable[fd];
}
e.error = hurd_file_name_lookup (&user_port, &user_fd, 0,
name, O_READ, 0, &interp.file);
}
}
if (interp.file != MACH_PORT_NULL)
{
prepare_and_check (interp.file, &interp);
if (! interp.error)
{
const ElfW(Phdr) *phdr = interp.info.elf.phdr;
interp.info.elf.phdr = alloca (interp.info.elf.phnum *
sizeof (ElfW(Phdr)));
check_elf_phdr (&interp, phdr);
}
e.error = interp.error;
}
if (e.error)
goto out;
if (newtask == oldtask)
{
thread_t *threads;
mach_msg_type_number_t nthreads, i;
e.error = task_threads (oldtask, &threads, &nthreads);
if (e.error)
goto out;
for (i = 0; i < nthreads; ++i)
{
thread_terminate (threads[i]);
mach_port_deallocate (mach_task_self (), threads[i]);
}
munmap ((caddr_t) threads, nthreads * sizeof (thread_t));
vm_deallocate (oldtask,
VM_MIN_ADDRESS, VM_MAX_ADDRESS - VM_MIN_ADDRESS);
oldtask_trashed = 1;
for (i = 0; i < ndeallocnames; ++i)
mach_port_deallocate (oldtask, deallocnames[i]);
for (i = 0; i < ndestroynames; ++i)
mach_port_destroy (oldtask, destroynames[i]);
}
{
vm_address_t addr = 0;
vm_size_t size = vm_page_size;
#ifdef __LP64__
if (e.info.elf.anywhere && (interp.file == MACH_PORT_NULL
|| interp.info.elf.anywhere))
size = (vm_size_t) 1 << 32;
#endif
e.error = vm_map (newtask,
&addr, size, 0, 0, MACH_PORT_NULL, 0, 1,
VM_PROT_NONE, VM_PROT_NONE, VM_INHERIT_COPY);
if (e.error)
goto out;
}
load (newtask, &e);
if (e.error)
goto out;
if (interp.file != MACH_PORT_NULL)
{
load (newtask, &interp);
if (interp.error)
{
e.error = interp.error;
goto out;
}
finish (&interp, 1);
}
finish (&e, 0);
if (e.info.elf.phdr_addr != 0)
{
e.info.elf.phdr_addr += e.info.elf.loadbase;
boot->phdr_addr = e.info.elf.phdr_addr;
boot->phdr_size = e.info.elf.phnum * sizeof (ElfW(Phdr));
}
boot->user_entry = e.entry;
if (boot->portarray[INIT_PORT_PROC] != MACH_PORT_NULL)
{
e.error = proc_set_code (boot->portarray[INIT_PORT_PROC],
e.start_code, e.end_code);
if (e.error)
goto out;
pid_t pid;
e.error = proc_task2pid (boot->portarray[INIT_PORT_PROC],
newtask, &pid);
if (e.error)
goto out;
if (abspath)
proc_set_exe (boot->portarray[INIT_PORT_PROC], abspath);
set_name (newtask, argv, pid);
e.error = proc_set_entry (boot->portarray[INIT_PORT_PROC],
e.entry);
if (e.error)
goto out;
}
else
set_name (newtask, argv, 0);
e.error = thread_create (newtask, &thread);
if (e.error)
goto out;
boot->stack_base = 0, boot->stack_size = 0;
e.error = mach_setup_thread (newtask, thread,
(void *) (e.interp.section ? interp.entry :
e.entry),
&boot->stack_base, &boot->stack_size);
if (e.error)
goto out;
if (e.info.elf.execstack || (e.interp.section && interp.info.elf.execstack))
e.error = vm_protect (newtask, boot->stack_base, boot->stack_size,
0, VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE);
if (oldtask != newtask && oldtask != MACH_PORT_NULL)
{
process_t proc;
process_t newproc;
process_t psrv;
mach_port_t rendezvous;
if (secure
|| boot->nports <= INIT_PORT_PROC
|| boot->portarray[INIT_PORT_PROC] == MACH_PORT_NULL)
psrv = procserver;
else
psrv = boot->portarray[INIT_PORT_PROC];
e.error = proc_task2proc (psrv, oldtask, &proc);
if (e.error)
goto out;
rendezvous = mach_reply_port ();
e.error = proc_reauthenticate_reassign (proc,
rendezvous,
MACH_MSG_TYPE_MAKE_SEND,
newtask);
if (e.error)
{
mach_port_mod_refs (mach_task_self (), rendezvous,
MACH_PORT_RIGHT_RECEIVE, -1);
mach_port_deallocate (mach_task_self (), proc);
goto out;
}
e.error = auth_user_authenticate (boot->portarray[INIT_PORT_AUTH],
rendezvous, MACH_MSG_TYPE_MAKE_SEND,
&newproc);
mach_port_mod_refs (mach_task_self (), rendezvous,
MACH_PORT_RIGHT_RECEIVE, -1);
mach_port_deallocate (mach_task_self (), proc);
if (e.error)
goto out;
e.error = proc_reauthenticate_complete (newproc);
if (e.error)
{
mach_port_deallocate (mach_task_self (), newproc);
goto out;
}
assert_backtrace (ports_replaced[INIT_PORT_PROC]);
mach_port_deallocate (mach_task_self (),
boot->portarray[INIT_PORT_PROC]);
boot->portarray[INIT_PORT_PROC] = newproc;
}
{
mach_port_t btport = ports_get_send_right (boot);
e.error = task_set_bootstrap_port (newtask, btport);
mach_port_deallocate (mach_task_self (), btport);
}
out:
if (interp.file != MACH_PORT_NULL)
finish (&interp, 1);
finish (&e, !e.error);
if (!e.error && (flags & EXEC_SIGTRAP))
{
mach_port_t proc;
if (boot->nports > INIT_PORT_PROC)
proc = boot->portarray[INIT_PORT_PROC];
else
e.error = proc_task2proc (procserver, newtask, &proc);
if (!e.error)
proc_mark_stop (proc, SIGTRAP, 0);
}
if (boot)
{
if (e.error)
memset (&boot->pi + 1, 0,
(char *) &boot[1] - (char *) (&boot->pi + 1));
else
if (boot->nports > INIT_PORT_PROC)
proc_mark_exec (boot->portarray[INIT_PORT_PROC]);
ports_port_deref (boot);
}
if (thread != MACH_PORT_NULL)
{
if (!e.error && !(flags & EXEC_SIGTRAP))
thread_resume (thread);
mach_port_deallocate (mach_task_self (), thread);
}
if (e.error)
{
if (oldtask != newtask)
{
task_terminate (newtask);
mach_port_deallocate (mach_task_self (), newtask);
}
if (oldtask_trashed)
task_terminate (oldtask);
else
task_resume (oldtask);
}
else
{
if (oldtask != newtask)
{
task_terminate (oldtask);
mach_port_deallocate (mach_task_self (), oldtask);
}
else
task_resume (oldtask);
mach_port_deallocate (mach_task_self (), newtask);
for (i = 0; i < nports; ++i)
if (ports_replaced[i] && portarray[i] != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), portarray[i]);
if (intarray_dealloc)
munmap (intarray, nints * sizeof intarray[0]);
if (!portarray_copy)
munmap (portarray, nports * sizeof portarray[0]);
}
return e.error;
}
kern_return_t
S_exec_exec (struct trivfs_protid *protid,
file_t file,
task_t oldtask,
int flags,
const_data_t argv, mach_msg_type_number_t argvlen, boolean_t argv_copy,
const_data_t envp, mach_msg_type_number_t envplen, boolean_t envp_copy,
const mach_port_t *dtable, mach_msg_type_number_t dtablesize,
boolean_t dtable_copy,
const mach_port_t *portarray, mach_msg_type_number_t nports,
boolean_t portarray_copy,
const int *intarray, mach_msg_type_number_t nints,
boolean_t intarray_copy,
const mach_port_t *deallocnames, mach_msg_type_number_t ndeallocnames,
const mach_port_t *destroynames, mach_msg_type_number_t ndestroynames)
{
return S_exec_exec_paths (protid,
file,
oldtask,
flags,
"",
"",
argv, argvlen, argv_copy,
envp, envplen, envp_copy,
dtable, dtablesize,
dtable_copy,
portarray, nports,
portarray_copy,
intarray, nints,
intarray_copy,
deallocnames, ndeallocnames,
destroynames, ndestroynames);
}
kern_return_t
S_exec_exec_paths (struct trivfs_protid *protid,
file_t file,
task_t oldtask,
int flags,
const_string_t path,
const_string_t abspath,
const char *argv, mach_msg_type_number_t argvlen,
boolean_t argv_copy,
const char *envp, mach_msg_type_number_t envplen,
boolean_t envp_copy,
const mach_port_t *dtable, mach_msg_type_number_t dtablesize,
boolean_t dtable_copy,
const mach_port_t *portarray, mach_msg_type_number_t nports,
boolean_t portarray_copy,
const int *intarray, mach_msg_type_number_t nints,
boolean_t intarray_copy,
const mach_port_t *deallocnames,
mach_msg_type_number_t ndeallocnames,
const mach_port_t *destroynames,
mach_msg_type_number_t ndestroynames)
{
if (! protid)
return EOPNOTSUPP;
return do_exec (file, oldtask, flags, path, abspath,
(char*) argv, argvlen, argv_copy,
(char*) envp, envplen, envp_copy,
(mach_port_t*) dtable, dtablesize, dtable_copy,
(mach_port_t*) portarray, nports, portarray_copy,
(int*) intarray, nints, intarray_copy,
deallocnames, ndeallocnames,
destroynames, ndestroynames);
}
kern_return_t
S_exec_setexecdata (struct trivfs_protid *protid,
const mach_port_t *ports, mach_msg_type_number_t nports, int ports_copy,
const int *ints, mach_msg_type_number_t nints, int ints_copy)
{
error_t err;
if (! protid || (protid->realnode != MACH_PORT_NULL && ! protid->isroot))
return EPERM;
if (nports < INIT_PORT_MAX || nints < INIT_INT_MAX)
return EINVAL;
err = 0;
ports = servercopy ((mach_port_t*) ports, nports * sizeof (mach_port_t), ports_copy, &err);
if (err)
return err;
ints = servercopy ((int*) ints, nints * sizeof (int), ints_copy, &err);
if (err)
{
munmap ((void*) ports, nports * sizeof (mach_port_t));
return err;
}
pthread_rwlock_wrlock (&std_lock);
if (std_ports)
{
mach_msg_type_number_t i;
for (i = 0; i < std_nports; ++i)
mach_port_deallocate (mach_task_self (), std_ports[i]);
munmap (std_ports, std_nports * sizeof (mach_port_t));
}
std_ports = (mach_port_t*) ports;
std_nports = nports;
if (std_ints)
munmap (std_ints, std_nints * sizeof (int));
std_ints = (int*) ints;
std_nints = nints;
pthread_rwlock_unlock (&std_lock);
return 0;
}
#include "exec_startup_S.h"
kern_return_t
S_exec_startup_get_info (struct bootinfo *boot,
vm_address_t *user_entry,
vm_address_t *phdr_data, vm_size_t *phdr_size,
vm_address_t *stack_base, vm_size_t *stack_size,
int *flags,
data_t *argvp, mach_msg_type_number_t *argvlen,
data_t *envpp, mach_msg_type_number_t *envplen,
mach_port_t **dtable,
mach_msg_type_name_t *dtablepoly,
mach_msg_type_number_t *dtablesize,
mach_port_t **portarray,
mach_msg_type_name_t *portpoly,
mach_msg_type_number_t *nports,
int **intarray, mach_msg_type_number_t *nints)
{
if (! boot)
return EOPNOTSUPP;
*user_entry = boot->user_entry;
*phdr_data = boot->phdr_addr;
*phdr_size = boot->phdr_size;
*stack_base = boot->stack_base;
*stack_size = boot->stack_size;
*argvp = boot->argv;
*argvlen = boot->argvlen;
boot->argvlen = 0;
*envpp = boot->envp;
*envplen = boot->envplen;
boot->envplen = 0;
*dtable = boot->dtable;
*dtablesize = boot->dtablesize;
*dtablepoly = MACH_MSG_TYPE_MOVE_SEND;
boot->dtablesize = 0;
*intarray = boot->intarray;
*nints = boot->nints;
boot->nints = 0;
*portarray = boot->portarray;
*nports = boot->nports;
*portpoly = MACH_MSG_TYPE_MOVE_SEND;
boot->nports = 0;
*flags = boot->flags;
return 0;
}