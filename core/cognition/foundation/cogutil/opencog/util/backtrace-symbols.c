#if defined(HAVE_BFD) && defined(HAVE_IBERTY)
#include "backtrace-symbols.h"
#define fatal(a, b) exit(1)
#define bfd_fatal(a) exit(1)
#define bfd_nonfatal(a) exit(1)
#define list_matching_formats(a) exit(1)
#define PTRSTR_LEN (sizeof(void *) * 2 + 3)
#define true 1
#define false 0
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef CYGWIN
#include <execinfo.h>
#endif
#include <libiberty.h>
#include <bfd.h>
#include <dlfcn.h>
#include <link.h>
#include <pthread.h>
#define OPTION_DEMANGLER (150)
#ifndef PTR
#define PTR void*
#endif
static void slurp_symtab(bfd *abfd, asymbol **syms)
{
long symcount;
unsigned int size;
if ((bfd_get_file_flags(abfd) & HAS_SYMS) == 0) return;
symcount = bfd_read_minisymbols(abfd, false, (PTR)&syms, &size);
if (symcount == 0) symcount = bfd_read_minisymbols(abfd, true , (PTR)&syms, &size);
if (symcount < 0) bfd_fatal(bfd_get_filename(abfd));
}
struct spotty {
asymbol **syms;
bfd_vma pc;
const char *filename;
const char *functionname;
unsigned int line;
int found;
};
static void find_address_in_section(bfd *abfd, asection *section, void *data)
{
struct spotty *spot = (struct spotty *)data;
bfd_vma vma;
bfd_size_type size;
if (spot->found) return;
#ifdef HAVE_DECL_BFD_SECTION_FLAGS
if ((bfd_section_flags(section) & SEC_ALLOC) == 0) return;
#elif defined HAVE_DECL_BFD_GET_SECTION_FLAGS
if ((bfd_get_section_flags(abfd, section) & SEC_ALLOC) == 0) return;
#else
#error "Unsupported BFD API"
#endif
#ifdef HAVE_DECL_BFD_SECTION_VMA
#ifdef HAVE_1_ARG_BFD_SECTION_VMA
vma = bfd_section_vma(section);
#elif defined HAVE_2_ARG_BFD_SECTION_VMA
vma = bfd_section_vma(abfd, section);
#else
#error "Unsupported BFD API"
#endif
#elif defined HAVE_DECL_BFD_GET_SECTION_VMA
vma = bfd_get_section_vma(abfd, section);
#else
#error "Unsupported BFD API"
#endif
if (spot->pc < vma) return;
#ifdef HAVE_1_ARG_BFD_SECTION_SIZE
size = bfd_section_size(section);
#elif defined HAVE_2_ARG_BFD_SECTION_SIZE
size = bfd_section_size(abfd, section);
#else
#error "Unsupported BFD API"
#endif
if (spot->pc >= vma + size) return;
spot->found = bfd_find_nearest_line(abfd, section, spot->syms, spot->pc - vma,
&(spot->filename), &(spot->functionname), &(spot->line));
}
static char **translate_addresses_buf(bfd *abfd, bfd_vma *addr, int naddr, asymbol **syms)
{
int naddr_orig = naddr;
char b;
int total = 0;
enum { Count, Print } state;
char *buf = &b;
int len = 0;
char **ret_buf = NULL;
struct spotty spot;
spot.syms = syms;
for (state = Count; state <= Print; state++) {
if (state == Print) {
ret_buf = malloc(total + sizeof(char *) * naddr);
buf = (char *)(ret_buf + naddr);
len = total;
}
while (naddr) {
if (state == Print) ret_buf[naddr - 1] = buf;
spot.pc = addr[naddr - 1];
spot.found = false;
bfd_map_over_sections(abfd, find_address_in_section, (PTR)&spot);
if (!spot.found) {
total += snprintf(buf, len, "[0x%llx] \?\?() \?\?:0",
(long long unsigned int)addr[naddr - 1]) +
1;
} else {
const char *name;
name = spot.functionname;
if (name == NULL || *name == '\0') name = "??";
if (spot.filename != NULL) {
char *h;
h = strrchr(spot.filename, '/');
if (h != NULL) spot.filename = h + 1;
}
total += snprintf(buf, len, "%s:%u\t%s()", spot.filename ? spot.filename : "??",
spot.line, name) +
1;
}
if (state == Print) {
buf = buf + total + 1;
}
naddr--;
}
naddr = naddr_orig;
}
return ret_buf;
}
static char **process_file(const char *file_name, bfd_vma *addr, int naddr)
{
bfd *abfd = NULL;
char **matching = NULL;
char **ret_buf = NULL;
asymbol **syms = NULL;
abfd = bfd_openr(file_name, NULL);
if (abfd == NULL) bfd_fatal(file_name);
if (bfd_check_format(abfd, bfd_archive))
fatal("%s: can not get addresses from archive", file_name);
if (!bfd_check_format_matches(abfd, bfd_object, &matching)) {
bfd_nonfatal(bfd_get_filename(abfd));
if (bfd_get_error() == bfd_error_file_ambiguously_recognized) {
list_matching_formats(matching);
free(matching);
}
xexit(1);
}
slurp_symtab(abfd, syms);
ret_buf = translate_addresses_buf(abfd, addr, naddr, syms);
if (syms != NULL) {
free(syms);
syms = NULL;
}
bfd_close(abfd);
return ret_buf;
}
#define MAX_DEPTH 16
struct file_match {
const char *file;
void *address;
void *base;
void *hdr;
};
static int find_matching_file(struct dl_phdr_info *info, size_t size, void *data)
{
struct file_match *match = data;
long n;
const ElfW(Phdr) * phdr;
ElfW(Addr) load_base = info->dlpi_addr;
phdr = info->dlpi_phdr;
for (n = info->dlpi_phnum; --n >= 0; phdr++) {
if (phdr->p_type == PT_LOAD) {
ElfW(Addr) vaddr = phdr->p_vaddr + load_base;
ElfW(Addr) match_address = (ElfW(Addr))match->address;
if (match_address >= vaddr && match_address < vaddr + phdr->p_memsz) {
match->file = info->dlpi_name;
match->base = (void *)info->dlpi_addr;
}
}
}
return 0;
}
char **oc_backtrace_symbols(void *const *buffer, int size)
{
static pthread_mutex_t bfd_mutex = PTHREAD_MUTEX_INITIALIZER;
int stack_depth = size - 1;
int x, y;
int total = 0;
char ***locations;
char **final;
char *f_strings;
locations = malloc(sizeof(char **) * (stack_depth + 1));
pthread_mutex_lock(&bfd_mutex);
bfd_init();
for (x = stack_depth, y = 0; x >= 0; x--, y++) {
struct file_match match = {.address = buffer[x]};
char **ret_buf;
bfd_vma addr;
dl_iterate_phdr(find_matching_file, &match);
addr = buffer[x] - match.base;
if (match.file && strlen(match.file))
ret_buf = process_file(match.file, &addr, 1);
else
ret_buf = process_file("/proc/self/exe", &addr, 1);
locations[x] = ret_buf;
total += strlen(ret_buf[0]) + 1;
}
pthread_mutex_unlock(&bfd_mutex);
final = malloc(total + (stack_depth + 1) * sizeof(char *));
f_strings = (char *)(final + stack_depth + 1);
for (x = stack_depth; x >= 0; x--) {
strcpy(f_strings, locations[x][0]);
free(locations[x]);
final[x] = f_strings;
f_strings += strlen(f_strings) + 1;
}
free(locations);
return final;
}
void oc_backtrace_symbols_fd(void *const *buffer, int size, int fd)
{
int j;
char **strings;
strings = backtrace_symbols(buffer, size);
if (strings == NULL) {
perror("backtrace_symbols");
exit(EXIT_FAILURE);
}
for (j = 0; j < size; j++)
dprintf(fd, "%s\n", strings[j]);
free(strings);
}
#endif