#define __KERNEL_SYSCALLS__
#include <stdarg.h>
#include <asm/system.h>
#include <asm/io.h>
#include <linux/types.h>
#include <linux/fcntl.h>
#include <linux/config.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/tty.h>
#include <linux/head.h>
#include <linux/unistd.h>
#include <linux/string.h>
#include <linux/timer.h>
#include <linux/fs.h>
#include <linux/ctype.h>
#include <linux/delay.h>
#include <linux/utsname.h>
#include <linux/ioport.h>
#include <linux/hdreg.h>
#include <linux/mm.h>
#include <linux/major.h>
#include <linux/blk.h>
#ifdef CONFIG_ROOT_NFS
#include <linux/nfs_fs.h>
#endif
#ifdef CONFIG_MTRR
#include <asm/mtrr.h>
#endif
#include <asm/bugs.h>
#include <linux/dev/glue/glue.h>
#if __GNUC__ < 2 || (__GNUC__ == 2 && __GNUC_MINOR__ < 6)
#error sorry, your GCC is too old. It builds incorrect kernels.
#endif
extern char _stext, _etext;
extern const char *linux_banner;
static char printbuf[1024];
extern int console_loglevel;
static int init(void *);
extern int bdflush(void *);
extern int kswapd(void *);
extern void kswapd_setup(void);
extern void init_modules(void);
extern long console_init(long, long);
extern long kmalloc_init(long,long);
extern void sock_init(void);
extern unsigned long pci_init(unsigned long, unsigned long);
extern void sysctl_init(void);
extern void no_scroll(char *str, int *ints);
extern void swap_setup(char *str, int *ints);
extern void buff_setup(char *str, int *ints);
extern void panic_setup(char *str, int *ints);
extern void bmouse_setup(char *str, int *ints);
extern void msmouse_setup(char *str, int *ints);
extern void lp_setup(char *str, int *ints);
extern void eth_setup(char *str, int *ints);
extern void xd_setup(char *str, int *ints);
extern void xd_manual_geo_init(char *str, int *ints);
extern void floppy_setup(char *str, int *ints);
extern void st_setup(char *str, int *ints);
extern void st0x_setup(char *str, int *ints);
extern void advansys_setup(char *str, int *ints);
extern void tmc8xx_setup(char *str, int *ints);
extern void t128_setup(char *str, int *ints);
extern void pas16_setup(char *str, int *ints);
extern void generic_NCR5380_setup(char *str, int *intr);
extern void generic_NCR53C400_setup(char *str, int *intr);
extern void aha152x_setup(char *str, int *ints);
extern void aha1542_setup(char *str, int *ints);
extern void gdth_setup(char *str, int *ints);
extern void aic7xxx_setup(char *str, int *ints);
extern void AM53C974_setup(char *str, int *ints);
extern void BusLogic_Setup(char *str, int *ints);
extern void ncr53c8xx_setup(char *str, int *ints);
extern void eata2x_setup(char *str, int *ints);
extern void u14_34f_setup(char *str, int *ints);
extern void fdomain_setup(char *str, int *ints);
extern void in2000_setup(char *str, int *ints);
extern void NCR53c406a_setup(char *str, int *ints);
extern void wd7000_setup(char *str, int *ints);
extern void ppa_setup(char *str, int *ints);
extern void scsi_luns_setup(char *str, int *ints);
extern void sound_setup(char *str, int *ints);
extern void apm_setup(char *str, int *ints);
extern void reboot_setup(char *str, int *ints);
#ifdef CONFIG_CDU31A
extern void cdu31a_setup(char *str, int *ints);
#endif CONFIG_CDU31A
#ifdef CONFIG_MCD
extern void mcd_setup(char *str, int *ints);
#endif CONFIG_MCD
#ifdef CONFIG_MCDX
extern void mcdx_setup(char *str, int *ints);
#endif CONFIG_MCDX
#ifdef CONFIG_SBPCD
extern void sbpcd_setup(char *str, int *ints);
#endif CONFIG_SBPCD
#ifdef CONFIG_AZTCD
extern void aztcd_setup(char *str, int *ints);
#endif CONFIG_AZTCD
#ifdef CONFIG_CDU535
extern void sonycd535_setup(char *str, int *ints);
#endif CONFIG_CDU535
#ifdef CONFIG_GSCD
extern void gscd_setup(char *str, int *ints);
#endif CONFIG_GSCD
#ifdef CONFIG_CM206
extern void cm206_setup(char *str, int *ints);
#endif CONFIG_CM206
#ifdef CONFIG_OPTCD
extern void optcd_setup(char *str, int *ints);
#endif CONFIG_OPTCD
#ifdef CONFIG_SJCD
extern void sjcd_setup(char *str, int *ints);
#endif CONFIG_SJCD
#ifdef CONFIG_ISP16_CDI
extern void isp16_setup(char *str, int *ints);
#endif CONFIG_ISP16_CDI
#ifdef CONFIG_BLK_DEV_RAM
static void ramdisk_start_setup(char *str, int *ints);
static void load_ramdisk(char *str, int *ints);
static void prompt_ramdisk(char *str, int *ints);
static void ramdisk_size(char *str, int *ints);
#ifdef CONFIG_BLK_DEV_INITRD
static void no_initrd(char *s,int *ints);
#endif
#endif CONFIG_BLK_DEV_RAM
#ifdef CONFIG_ISDN_DRV_ICN
extern void icn_setup(char *str, int *ints);
#endif
#ifdef CONFIG_ISDN_DRV_HISAX
extern void HiSax_setup(char *str, int *ints);
#endif
#ifdef CONFIG_ISDN_DRV_PCBIT
extern void pcbit_setup(char *str, int *ints);
#endif
#ifdef CONFIG_ATARIMOUSE
extern void atari_mouse_setup (char *str, int *ints);
#endif
#ifdef CONFIG_DMASOUND
extern void dmasound_setup (char *str, int *ints);
#endif
#ifdef CONFIG_ATARI_SCSI
extern void atari_scsi_setup (char *str, int *ints);
#endif
extern void wd33c93_setup (char *str, int *ints);
extern void gvp11_setup (char *str, int *ints);
#ifdef CONFIG_CYCLADES
extern void cy_setup(char *str, int *ints);
#endif
#ifdef CONFIG_DIGI
extern void pcxx_setup(char *str, int *ints);
#endif
#ifdef CONFIG_RISCOM8
extern void riscom8_setup(char *str, int *ints);
#endif
#ifdef CONFIG_SPECIALIX
extern void specialix_setup(char *str, int *ints);
#endif
#ifdef CONFIG_BAYCOM
extern void baycom_setup(char *str, int *ints);
#endif
#ifdef CONFIG_PARIDE_PD
extern void pd_setup(char *str, int *ints);
#endif
#ifdef CONFIG_PARIDE_PF
extern void pf_setup(char *str, int *ints);
#endif
#ifdef CONFIG_PARIDE_PT
extern void pt_setup(char *str, int *ints);
#endif
#ifdef CONFIG_PARIDE_PG
extern void pg_setup(char *str, int *ints);
#endif
#ifdef CONFIG_PARIDE_PCD
extern void pcd_setup(char *str, int *ints);
#endif
#if defined(CONFIG_SYSVIPC) || defined(CONFIG_KERNELD)
extern void ipc_init(void);
#endif
#define MAX_INIT_ARGS 8
#define MAX_INIT_ENVS 8
extern void time_init(void);
static unsigned long memory_start = 0;
static unsigned long memory_end = 0;
int rows, cols;
#ifdef CONFIG_BLK_DEV_RAM
extern int rd_doload;
extern int rd_prompt;
extern int rd_size;
extern int rd_image_start;
#ifdef CONFIG_BLK_DEV_INITRD
kdev_t real_root_dev;
#endif
#endif
int root_mountflags = MS_RDONLY;
char *execute_command = 0;
#ifdef CONFIG_ROOT_NFS
char nfs_root_name[NFS_ROOT_NAME_LEN] = { "default" };
char nfs_root_addrs[NFS_ROOT_ADDRS_LEN] = { "" };
#endif
extern void dquot_init(void);
static char * argv_init[MAX_INIT_ARGS+2] = { "init", NULL, };
static char * envp_init[MAX_INIT_ENVS+2] = { "HOME=/", "TERM=linux", NULL, };
static char * argv_rc[] = { "/bin/sh", NULL };
static char * envp_rc[] = { "HOME=/", "TERM=linux", NULL };
static char * argv[] = { "-/bin/sh",NULL };
static char * envp[] = { "HOME=/usr/root", "TERM=linux", NULL };
char *get_options(char *str, int *ints)
{
char *cur = str;
int i=1;
while (cur && isdigit(*cur) && i <= 10) {
ints[i++] = simple_strtoul(cur,NULL,0);
if ((cur = strchr(cur,',')) != NULL)
cur++;
}
ints[0] = i-1;
return(cur);
}
static void profile_setup(char *str, int *ints)
{
if (ints[0] > 0)
prof_shift = (unsigned long) ints[1];
else
#ifdef CONFIG_PROFILE_SHIFT
prof_shift = CONFIG_PROFILE_SHIFT;
#else
prof_shift = 2;
#endif
}
struct kernel_param {
const char *str;
void (*setup_func)(char *, int *);
} ;
struct kernel_param bootsetups[] = {
{ "reserve=", reserve_setup },
{ "profile=", profile_setup },
#ifdef CONFIG_BLK_DEV_RAM
{ "ramdisk_start=", ramdisk_start_setup },
{ "load_ramdisk=", load_ramdisk },
{ "prompt_ramdisk=", prompt_ramdisk },
{ "ramdisk=", ramdisk_size },
{ "ramdisk_size=", ramdisk_size },
#ifdef CONFIG_BLK_DEV_INITRD
{ "noinitrd", no_initrd },
#endif
#endif
{ "swap=", swap_setup },
{ "buff=", buff_setup },
{ "panic=", panic_setup },
{ "no-scroll", no_scroll },
#ifdef CONFIG_BUGi386
{ "no-hlt", no_halt },
{ "no387", no_387 },
{ "reboot=", reboot_setup },
#endif
#ifdef CONFIG_INET
{ "ether=", eth_setup },
#endif
#ifdef CONFIG_PRINTER
{ "lp=", lp_setup },
#endif
#ifdef CONFIG_SCSI
{ "max_scsi_luns=", scsi_luns_setup },
#endif
#ifdef CONFIG_SCSI_ADVANSYS
{ "advansys=", advansys_setup },
#endif
#if defined(CONFIG_BLK_DEV_HD)
{ "hd=", hd_setup },
#endif
#ifdef CONFIG_CHR_DEV_ST
{ "st=", st_setup },
#endif
#ifdef CONFIG_BUSMOUSE
{ "bmouse=", bmouse_setup },
#endif
#ifdef CONFIG_MS_BUSMOUSE
{ "msmouse=", msmouse_setup },
#endif
#ifdef CONFIG_SCSI_SEAGATE
{ "st0x=", st0x_setup },
{ "tmc8xx=", tmc8xx_setup },
#endif
#ifdef CONFIG_SCSI_T128
{ "t128=", t128_setup },
#endif
#ifdef CONFIG_SCSI_PAS16
{ "pas16=", pas16_setup },
#endif
#ifdef CONFIG_SCSI_GENERIC_NCR5380
{ "ncr5380=", generic_NCR5380_setup },
{ "ncr53c400=", generic_NCR53C400_setup },
#endif
#ifdef CONFIG_SCSI_AHA152X
{ "aha152x=", aha152x_setup},
#endif
#ifdef CONFIG_SCSI_AHA1542
{ "aha1542=", aha1542_setup},
#endif
#ifdef CONFIG_SCSI_GDTH
{ "gdth=", gdth_setup},
#endif
#ifdef CONFIG_SCSI_AIC7XXX
{ "aic7xxx=", aic7xxx_setup},
#endif
#ifdef CONFIG_SCSI_BUSLOGIC
{ "BusLogic=", BusLogic_Setup},
#endif
#ifdef CONFIG_SCSI_NCR53C8XX
{ "ncr53c8xx=", ncr53c8xx_setup},
#endif
#ifdef CONFIG_SCSI_EATA
{ "eata=", eata2x_setup},
#endif
#ifdef CONFIG_SCSI_U14_34F
{ "u14-34f=", u14_34f_setup},
#endif
#ifdef CONFIG_SCSI_AM53C974
{ "AM53C974=", AM53C974_setup},
#endif
#ifdef CONFIG_SCSI_NCR53C406A
{ "ncr53c406a=", NCR53c406a_setup},
#endif
#ifdef CONFIG_SCSI_FUTURE_DOMAIN
{ "fdomain=", fdomain_setup},
#endif
#ifdef CONFIG_SCSI_IN2000
{ "in2000=", in2000_setup},
#endif
#ifdef CONFIG_SCSI_7000FASST
{ "wd7000=", wd7000_setup},
#endif
#ifdef CONFIG_SCSI_PPA
{ "ppa=", ppa_setup },
#endif
#ifdef CONFIG_BLK_DEV_XD
{ "xd=", xd_setup },
{ "xd_geo=", xd_manual_geo_init },
#endif
#ifdef CONFIG_BLK_DEV_FD
{ "floppy=", floppy_setup },
#endif
#ifdef CONFIG_CDU31A
{ "cdu31a=", cdu31a_setup },
#endif CONFIG_CDU31A
#ifdef CONFIG_MCD
{ "mcd=", mcd_setup },
#endif CONFIG_MCD
#ifdef CONFIG_MCDX
{ "mcdx=", mcdx_setup },
#endif CONFIG_MCDX
#ifdef CONFIG_SBPCD
{ "sbpcd=", sbpcd_setup },
#endif CONFIG_SBPCD
#ifdef CONFIG_AZTCD
{ "aztcd=", aztcd_setup },
#endif CONFIG_AZTCD
#ifdef CONFIG_CDU535
{ "sonycd535=", sonycd535_setup },
#endif CONFIG_CDU535
#ifdef CONFIG_GSCD
{ "gscd=", gscd_setup },
#endif CONFIG_GSCD
#ifdef CONFIG_CM206
{ "cm206=", cm206_setup },
#endif CONFIG_CM206
#ifdef CONFIG_OPTCD
{ "optcd=", optcd_setup },
#endif CONFIG_OPTCD
#ifdef CONFIG_SJCD
{ "sjcd=", sjcd_setup },
#endif CONFIG_SJCD
#ifdef CONFIG_ISP16_CDI
{ "isp16=", isp16_setup },
#endif CONFIG_ISP16_CDI
#ifdef CONFIG_SOUND
{ "sound=", sound_setup },
#endif
#ifdef CONFIG_ISDN_DRV_ICN
{ "icn=", icn_setup },
#endif
#ifdef CONFIG_ISDN_DRV_HISAX
{ "hisax=", HiSax_setup },
{ "HiSax=", HiSax_setup },
#endif
#ifdef CONFIG_ISDN_DRV_PCBIT
{ "pcbit=", pcbit_setup },
#endif
#ifdef CONFIG_ATARIMOUSE
{ "atamouse=", atari_mouse_setup },
#endif
#ifdef CONFIG_DMASOUND
{ "dmasound=", dmasound_setup },
#endif
#ifdef CONFIG_ATARI_SCSI
{ "atascsi=", atari_scsi_setup },
#endif
#if defined(CONFIG_A3000_SCSI) || defined(CONFIG_A2091_SCSI) \
|| defined(CONFIG_GVP11_SCSI)
{ "wd33c93=", wd33c93_setup },
#endif
#if defined(CONFIG_GVP11_SCSI)
{ "gvp11=", gvp11_setup },
#endif
#ifdef CONFIG_CYCLADES
{ "cyclades=", cy_setup },
#endif
#ifdef CONFIG_DIGI
{ "digi=", pcxx_setup },
#endif
#ifdef CONFIG_RISCOM8
{ "riscom8=", riscom8_setup },
#endif
#ifdef CONFIG_SPECIALIX
{ "specialix=", specialix_setup },
#endif
#ifdef CONFIG_BAYCOM
{ "baycom=", baycom_setup },
#endif
#ifdef CONFIG_APM
{ "apm=", apm_setup },
#endif
{ 0, 0 }
};
static struct kernel_param raw_params[] = {
#ifdef CONFIG_PARIDE_PD
{ "pd.", pd_setup },
#endif
#ifdef CONFIG_PARIDE_PCD
{ "pcd.", pcd_setup },
#endif
#ifdef CONFIG_PARIDE_PF
{ "pf.", pf_setup },
#endif
#ifdef CONFIG_PARIDE_PT
{ "pt.", pt_setup },
#endif
#ifdef CONFIG_PARIDE_PG
{ "pg.", pg_setup },
#endif
{ 0, 0 }
} ;
#ifdef CONFIG_BLK_DEV_RAM
static void ramdisk_start_setup(char *str, int *ints)
{
if (ints[0] > 0 && ints[1] >= 0)
rd_image_start = ints[1];
}
static void load_ramdisk(char *str, int *ints)
{
if (ints[0] > 0 && ints[1] >= 0)
rd_doload = ints[1] & 3;
}
static void prompt_ramdisk(char *str, int *ints)
{
if (ints[0] > 0 && ints[1] >= 0)
rd_prompt = ints[1] & 1;
}
static void ramdisk_size(char *str, int *ints)
{
if (ints[0] > 0 && ints[1] >= 0)
rd_size = ints[1];
}
#endif
static int checksetup(char *line)
{
int i = 0;
int ints[11];
#ifdef CONFIG_BLK_DEV_IDE
if (!strncmp(line,"ide",3) || (!strncmp(line,"hd",2) && line[2] != '=')) {
ide_setup(line);
return 1;
}
#endif
while (bootsetups[i].str) {
int n = strlen(bootsetups[i].str);
if (!strncmp(line,bootsetups[i].str,n)) {
bootsetups[i].setup_func(get_options(line+n,ints), ints);
return 1;
}
i++;
}
for (i=0; raw_params[i].str; i++) {
int n = strlen(raw_params[i].str);
if (!strncmp(line,raw_params[i].str,n)) {
raw_params[i].setup_func(line+n, NULL);
return 1;
}
}
return 0;
}
unsigned long loops_per_sec = (1<<12);
#if defined(__SMP__) && defined(__i386__)
unsigned long smp_loops_per_tick = 1000000;
#endif
#define LPS_PREC 8
void calibrate_delay(void)
{
int ticks;
int loopbit;
int lps_precision = LPS_PREC;
loops_per_sec = (1<<12);
printk("Calibrating delay loop.. ");
while (loops_per_sec <<= 1) {
ticks = jiffies;
while (ticks == jiffies)
;
ticks = jiffies;
__delay(loops_per_sec);
ticks = jiffies - ticks;
if (ticks)
break;
}
loops_per_sec >>= 1;
loopbit = loops_per_sec;
while ( lps_precision-- && (loopbit >>= 1) ) {
loops_per_sec |= loopbit;
ticks = jiffies;
while (ticks == jiffies);
ticks = jiffies;
__delay(loops_per_sec);
if (jiffies != ticks)
loops_per_sec &= ~loopbit;
}
loops_per_sec *= HZ;
printk("ok - %lu.%02lu BogoMIPS\n",
(loops_per_sec+2500)/500000,
((loops_per_sec+2500)/5000) % 100);
#if defined(__SMP__) && defined(__i386__)
smp_loops_per_tick = loops_per_sec / 400;
#endif
}
static void parse_root_dev(char * line)
{
int base = 0;
static struct dev_name_struct {
const char *name;
const int num;
} devices[] = {
{ "nfs", 0x00ff },
{ "loop", 0x0700 },
{ "hda", 0x0300 },
{ "hdb", 0x0340 },
{ "hdc", 0x1600 },
{ "hdd", 0x1640 },
{ "hde", 0x2100 },
{ "hdf", 0x2140 },
{ "hdg", 0x2200 },
{ "hdh", 0x2240 },
{ "sda", 0x0800 },
{ "sdb", 0x0810 },
{ "sdc", 0x0820 },
{ "sdd", 0x0830 },
{ "sde", 0x0840 },
{ "sdf", 0x0850 },
{ "sdg", 0x0860 },
{ "sdh", 0x0870 },
{ "sdi", 0x0880 },
{ "sdj", 0x0890 },
{ "sdk", 0x08a0 },
{ "sdl", 0x08b0 },
{ "sdm", 0x08c0 },
{ "sdn", 0x08d0 },
{ "sdo", 0x08e0 },
{ "sdp", 0x08f0 },
{ "fd", 0x0200 },
{ "xda", 0x0d00 },
{ "xdb", 0x0d40 },
{ "ram", 0x0100 },
{ "scd", 0x0b00 },
{ "mcd", 0x1700 },
{ "cdu535", 0x1800 },
{ "aztcd", 0x1d00 },
{ "cm206cd", 0x2000 },
{ "gscd", 0x1000 },
{ "sbpcd", 0x1900 },
{ "sonycd", 0x1800 },
#ifdef CONFIG_PARIDE_PD
{ "pda", 0x2d00 },
{ "pdb", 0x2d10 },
{ "pdc", 0x2d20 },
{ "pdd", 0x2d30 },
#endif
#ifdef CONFIG_PARIDE_PCD
{ "pcd", 0x2e00 },
#endif
#ifdef CONFIG_PARIDE_PF
{ "pf", 0x2f00 },
#endif
{ NULL, 0 }
};
if (strncmp(line,"/dev/",5) == 0) {
struct dev_name_struct *dev = devices;
line += 5;
do {
int len = strlen(dev->name);
if (strncmp(line,dev->name,len) == 0) {
line += len;
base = dev->num;
break;
}
dev++;
} while (dev->name);
}
ROOT_DEV = to_kdev_t(base + simple_strtoul(line,NULL,base?10:16));
}
static void parse_options(char *line)
{
char *next;
int args, envs;
if (!*line)
return;
args = 0;
envs = 1;
next = line;
while ((line = next) != NULL) {
if ((next = strchr(line,' ')) != NULL)
*next++ = 0;
if (!strncmp(line,"root=",5)) {
parse_root_dev(line+5);
continue;
}
#ifdef CONFIG_ROOT_NFS
if (!strncmp(line, "nfsroot=", 8)) {
int n;
line += 8;
ROOT_DEV = MKDEV(UNNAMED_MAJOR, 255);
if (line[0] == '/' || line[0] == ',' || (line[0] >= '0' && line[0] <= '9')) {
strncpy(nfs_root_name, line, sizeof(nfs_root_name));
nfs_root_name[sizeof(nfs_root_name)-1] = '\0';
continue;
}
n = strlen(line) + strlen(NFS_ROOT);
if (n >= sizeof(nfs_root_name))
line[sizeof(nfs_root_name) - strlen(NFS_ROOT) - 1] = '\0';
sprintf(nfs_root_name, NFS_ROOT, line);
continue;
}
if (!strncmp(line, "nfsaddrs=", 9)) {
line += 9;
strncpy(nfs_root_addrs, line, sizeof(nfs_root_addrs));
nfs_root_addrs[sizeof(nfs_root_addrs)-1] = '\0';
continue;
}
#endif
if (!strcmp(line,"ro")) {
root_mountflags |= MS_RDONLY;
continue;
}
if (!strcmp(line,"rw")) {
root_mountflags &= ~MS_RDONLY;
continue;
}
if (!strcmp(line,"debug")) {
console_loglevel = 10;
continue;
}
if (!strncmp(line,"init=",5)) {
line += 5;
execute_command = line;
continue;
}
if (checksetup(line))
continue;
if (strchr(line,'=')) {
if (envs >= MAX_INIT_ENVS)
break;
envp_init[++envs] = line;
} else {
if (args >= MAX_INIT_ARGS)
break;
argv_init[++args] = line;
}
}
argv_init[args+1] = NULL;
envp_init[envs+1] = NULL;
}
extern void setup_arch(char **, unsigned long *, unsigned long *);
extern void arch_syms_export(void);
#ifndef __SMP__
int cpu_idle(void *unused)
{
for(;;)
idle();
}
#else
extern int cpu_idle(void * unused);
asmlinkage void start_secondary(void)
{
trap_init();
init_IRQ();
smp_callin();
cpu_idle(NULL);
}
static void smp_init(void)
{
int i, j;
smp_boot_cpus();
for (i=1; i<smp_num_cpus; i++)
{
struct task_struct *n, *p;
j = cpu_logical_map[i];
kernel_thread(cpu_idle, NULL, CLONE_PID);
current_set[j]=task[i];
current_set[j]->processor=j;
cli();
n = task[i]->next_run;
p = task[i]->prev_run;
nr_running--;
n->prev_run = p;
p->next_run = n;
task[i]->next_run = task[i]->prev_run = task[i];
sti();
}
}
static void smp_begin(void)
{
smp_threads_ready=1;
smp_commence();
}
#endif
asmlinkage void start_kernel(void)
{
char * command_line;
#ifdef __SMP__
static int first_cpu=1;
if(!first_cpu)
start_secondary();
first_cpu=0;
#endif
setup_arch(&command_line, &memory_start, &memory_end);
memory_start = paging_init(memory_start,memory_end);
trap_init();
init_IRQ();
sched_init();
time_init();
parse_options(command_line);
#ifdef CONFIG_MODULES
init_modules();
#endif
#ifdef CONFIG_PROFILE
if (!prof_shift)
#ifdef CONFIG_PROFILE_SHIFT
prof_shift = CONFIG_PROFILE_SHIFT;
#else
prof_shift = 2;
#endif
#endif
if (prof_shift) {
prof_buffer = (unsigned int *) memory_start;
prof_len = (unsigned long) &_etext - (unsigned long) &_stext;
prof_len >>= prof_shift;
memory_start += prof_len * sizeof(unsigned int);
memset(prof_buffer, 0, prof_len * sizeof(unsigned int));
}
memory_start = console_init(memory_start,memory_end);
#ifdef CONFIG_PCI
memory_start = pci_init(memory_start,memory_end);
#endif
memory_start = kmalloc_init(memory_start,memory_end);
sti();
calibrate_delay();
memory_start = inode_init(memory_start,memory_end);
memory_start = file_table_init(memory_start,memory_end);
memory_start = name_cache_init(memory_start,memory_end);
#ifdef CONFIG_BLK_DEV_INITRD
if (initrd_start && initrd_start < memory_start) {
printk(KERN_CRIT "initrd overwritten (0x%08lx < 0x%08lx) - "
"disabling it.\n",initrd_start,memory_start);
initrd_start = 0;
}
#endif
mem_init(memory_start,memory_end);
buffer_init();
sock_init();
#if defined(CONFIG_SYSVIPC) || defined(CONFIG_KERNELD)
ipc_init();
#endif
dquot_init();
arch_syms_export();
sti();
check_bugs();
#if defined(CONFIG_MTRR) && defined(__SMP__)
init_mtrr_config();
#endif
printk(linux_banner);
#ifdef __SMP__
smp_init();
#endif
sysctl_init();
kernel_thread(init, NULL, 0);
cpu_idle(NULL);
}
static int printf(const char *fmt, ...)
{
va_list args;
int i;
va_start(args, fmt);
write(1,printbuf,i=vsprintf(printbuf, fmt, args));
va_end(args);
return i;
}
static int do_rc(void * rc)
{
close(0);
if (open(rc,O_RDONLY,0))
return -1;
return execve("/bin/sh", argv_rc, envp_rc);
}
static int do_shell(void * shell)
{
close(0);close(1);close(2);
setsid();
(void) open("/dev/tty1",O_RDWR,0);
(void) dup(0);
(void) dup(0);
return execve(shell, argv, envp);
}
#ifdef CONFIG_BLK_DEV_INITRD
static int do_linuxrc(void * shell)
{
static char *argv[] = { "linuxrc", NULL, };
close(0);close(1);close(2);
setsid();
(void) open("/dev/tty1",O_RDWR,0);
(void) dup(0);
(void) dup(0);
return execve(shell, argv, envp_init);
}
static void no_initrd(char *s,int *ints)
{
mount_initrd = 0;
}
#endif
static int init(void * unused)
{
int pid,i;
#ifdef CONFIG_BLK_DEV_INITRD
int real_root_mountflags;
#endif
kernel_thread(bdflush, NULL, 0);
kswapd_setup();
kernel_thread(kswapd, NULL, 0);
#ifdef CONFIG_BLK_DEV_INITRD
real_root_dev = ROOT_DEV;
real_root_mountflags = root_mountflags;
if (initrd_start && mount_initrd) root_mountflags &= ~MS_RDONLY;
else mount_initrd =0;
#endif
setup();
#ifdef __SMP__
smp_begin();
#endif
#ifdef CONFIG_UMSDOS_FS
{
extern struct inode *pseudo_root;
if (pseudo_root != NULL){
current->fs->root = pseudo_root;
current->fs->pwd = pseudo_root;
}
}
#endif
#ifdef CONFIG_BLK_DEV_INITRD
root_mountflags = real_root_mountflags;
if (mount_initrd && ROOT_DEV != real_root_dev && ROOT_DEV == MKDEV(RAMDISK_MAJOR,0)) {
int error;
pid = kernel_thread(do_linuxrc, "/linuxrc", SIGCHLD);
if (pid>0)
while (pid != wait(&i));
if (real_root_dev != MKDEV(RAMDISK_MAJOR, 0)) {
error = change_root(real_root_dev,"/initrd");
if (error)
printk(KERN_ERR "Change root to /initrd: "
"error %d\n",error);
}
}
#endif
if ((open("/dev/tty1",O_RDWR,0) < 0) &&
(open("/dev/ttyS0",O_RDWR,0) < 0))
printk("Unable to open an initial console.\n");
(void) dup(0);
(void) dup(0);
if (!execute_command) {
execve("/etc/init",argv_init,envp_init);
execve("/bin/init",argv_init,envp_init);
execve("/sbin/init",argv_init,envp_init);
pid = kernel_thread(do_rc, "/etc/rc", SIGCHLD);
if (pid>0)
while (pid != wait(&i))
;
}
while (1) {
pid = kernel_thread(do_shell,
execute_command ? execute_command : "/bin/sh",
SIGCHLD);
if (pid < 0) {
printf("Fork failed in init\n\r");
continue;
}
while (1)
if (pid == wait(&i))
break;
printf("\n\rchild %d died with code %04x\n\r",pid,i);
sync();
}
return -1;
}