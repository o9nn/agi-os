#include "priv.h"
#include <mach.h>
#include <hurd/hurd_types.h>
#include <errno.h>
#include <elf.h>
error_t
elf_machine_matches_host (ElfW(Half) e_machine)
{
static void *host_type;
struct host_basic_info hostinfo;
if (host_type)
goto *host_type;
else
{
error_t err;
mach_msg_type_number_t hostinfocnt = HOST_BASIC_INFO_COUNT;
err = host_info (mach_host_self (), HOST_BASIC_INFO,
(host_info_t) &hostinfo, &hostinfocnt);
if (err)
return err;
assert_backtrace (hostinfocnt == HOST_BASIC_INFO_COUNT);
}
#define CACHE(test) ({ __label__ here; host_type = &&here; \
here: return (test) ? 0 : ENOEXEC; })
switch (hostinfo.cpu_type)
{
case CPU_TYPE_MC68020:
case CPU_TYPE_MC68030:
case CPU_TYPE_MC68040:
CACHE (e_machine == EM_68K);
case CPU_TYPE_I860:
CACHE (e_machine == EM_860);
case CPU_TYPE_MIPS:
CACHE (e_machine == EM_MIPS);
case CPU_TYPE_MC88000:
CACHE (e_machine == EM_88K);
case CPU_TYPE_SPARC:
CACHE (e_machine == EM_SPARC);
case CPU_TYPE_I386:
case CPU_TYPE_I486:
case CPU_TYPE_PENTIUM:
case CPU_TYPE_PENTIUMPRO:
CACHE (e_machine == EM_386);
#if defined (CPU_TYPE_X86_64) || defined (__x86_64__)
case CPU_TYPE_X86_64:
CACHE (e_machine == EM_X86_64);
#endif
case CPU_TYPE_POWERPC:
CACHE (e_machine == EM_PPC);
case CPU_TYPE_ALPHA:
CACHE (e_machine == EM_ALPHA);
case CPU_TYPE_HPPA:
CACHE (e_machine == EM_PARISC);
#if defined (CPU_TYPE_ARM64) || defined(__aarch64__)
case CPU_TYPE_ARM64:
CACHE (e_machine == EM_AARCH64);
#endif
default:
return EGRATUITOUS;
}
return 0;
}