#include <linux/config.h>
#include <linux/fs.h>
#include <linux/genhd.h>
#include <linux/kernel.h>
#include <linux/major.h>
#include <linux/string.h>
#ifdef CONFIG_BLK_DEV_INITRD
#include <linux/blk.h>
#endif
#include <asm/system.h>
#include <asm/unaligned.h>
#define SYS_IND(p)	get_unaligned(&p->sys_ind)
#define NR_SECTS(p)	get_unaligned(&p->nr_sects)
#define START_SECT(p)	get_unaligned(&p->start_sect)
struct gendisk *gendisk_head = NULL;
static int current_minor = 0;
extern int *blk_size[];
extern void rd_load(void);
extern void initrd_load(void);
extern int chr_dev_init(void);
extern int blk_dev_init(void);
extern int scsi_dev_init(void);
extern int net_dev_init(void);
char *disk_name (struct gendisk *hd, int minor, char *buf)
{
unsigned int part;
const char *maj = hd->major_name;
char unit = (minor >> hd->minor_shift) + 'a';
#ifdef CONFIG_BLK_DEV_IDE
switch (hd->major) {
case IDE3_MAJOR:
unit += 2;
case IDE2_MAJOR:
unit += 2;
case IDE1_MAJOR:
unit += 2;
case IDE0_MAJOR:
maj = "hd";
}
#endif
part = minor & ((1 << hd->minor_shift) - 1);
if (part)
sprintf(buf, "%s%c%d", maj, unit, part);
else
sprintf(buf, "%s%c", maj, unit);
return buf;
}
static void add_partition (struct gendisk *hd, int minor, int start, int size)
{
char buf[8];
hd->part[minor].start_sect = start;
hd->part[minor].nr_sects   = size;
printk(" %s", disk_name(hd, minor, buf));
}
static inline int is_extended_partition(struct partition *p)
{
return (SYS_IND(p) == DOS_EXTENDED_PARTITION ||
SYS_IND(p) == WIN98_EXTENDED_PARTITION ||
SYS_IND(p) == LINUX_EXTENDED_PARTITION);
}
#ifdef CONFIG_MSDOS_PARTITION
static void extended_partition(struct gendisk *hd, kdev_t dev)
{
struct buffer_head *bh;
struct partition *p;
unsigned long first_sector, first_size, this_sector, this_size;
int mask = (1 << hd->minor_shift) - 1;
int i;
first_sector = hd->part[MINOR(dev)].start_sect;
first_size = hd->part[MINOR(dev)].nr_sects;
this_sector = first_sector;
while (1) {
if ((current_minor & mask) == 0)
return;
if (!(bh = bread(dev,0,1024)))
return;
bh->b_state = 0;
if (*(unsigned short *) (bh->b_data+510) != 0xAA55)
goto done;
p = (struct partition *) (0x1BE + bh->b_data);
this_size = hd->part[MINOR(dev)].nr_sects;
for (i=0; i<4; i++, p++) {
if (!NR_SECTS(p) || is_extended_partition(p))
continue;
if (i >= 2
&& START_SECT(p) + NR_SECTS(p) > this_size
&& (this_sector + START_SECT(p) < first_sector ||
this_sector + START_SECT(p) + NR_SECTS(p) >
first_sector + first_size))
continue;
add_partition(hd, current_minor, this_sector+START_SECT(p), NR_SECTS(p));
current_minor++;
if ((current_minor & mask) == 0)
goto done;
}
p -= 4;
for (i=0; i<4; i++, p++)
if(NR_SECTS(p) && is_extended_partition(p))
break;
if (i == 4)
goto done;
hd->part[current_minor].nr_sects = NR_SECTS(p);
hd->part[current_minor].start_sect = first_sector + START_SECT(p);
this_sector = first_sector + START_SECT(p);
dev = MKDEV(hd->major, current_minor);
brelse(bh);
}
done:
brelse(bh);
}
#ifdef CONFIG_BSD_DISKLABEL
static void bsd_disklabel_partition(struct gendisk *hd, kdev_t dev)
{
struct buffer_head *bh;
struct bsd_disklabel *l;
struct bsd_partition *p;
int mask = (1 << hd->minor_shift) - 1;
if (!(bh = bread(dev,0,1024)))
return;
bh->b_state = 0;
l = (struct bsd_disklabel *) (bh->b_data+512);
if (l->d_magic != BSD_DISKMAGIC) {
brelse(bh);
return;
}
p = &l->d_partitions[0];
while (p - &l->d_partitions[0] <= BSD_MAXPARTITIONS) {
if ((current_minor & mask) >= (4 + hd->max_p))
break;
if (p->p_fstype != BSD_FS_UNUSED) {
add_partition(hd, current_minor, p->p_offset, p->p_size);
current_minor++;
}
p++;
}
brelse(bh);
}
#endif
static int msdos_partition(struct gendisk *hd, kdev_t dev, unsigned long first_sector)
{
int i, minor = current_minor;
struct buffer_head *bh;
struct partition *p;
unsigned char *data;
int mask = (1 << hd->minor_shift) - 1;
#ifdef CONFIG_BLK_DEV_IDE
int tested_for_xlate = 0;
read_mbr:
#endif
if (!(bh = bread(dev,0,1024))) {
printk(" unable to read partition table\n");
return -1;
}
data = bh->b_data;
bh->b_state = 0;
#ifdef CONFIG_BLK_DEV_IDE
check_table:
#endif
if (*(unsigned short *)  (0x1fe + data) != 0xAA55) {
brelse(bh);
return 0;
}
p = (struct partition *) (0x1be + data);
#ifdef CONFIG_BLK_DEV_IDE
if (!tested_for_xlate++) {
extern int ide_xlate_1024(kdev_t, int, const char *);
unsigned int sig = *(unsigned short *)(data + 2);
if (SYS_IND(p) == EZD_PARTITION) {
if (ide_xlate_1024(dev, -1, " [EZD]")) {
data += 512;
goto check_table;
}
} else if (SYS_IND(p) == DM6_PARTITION) {
if (ide_xlate_1024(dev, 1, " [DM6:DDO]")) {
brelse(bh);
goto read_mbr;
}
} else if (sig <= 0x1ae && *(unsigned short *)(data + sig) == 0x55AA
&& (1 & *(unsigned char *)(data + sig + 2)) )
{
(void) ide_xlate_1024 (dev, 0, " [DM6:MBR]");
} else if (SYS_IND(p) == DM6_AUX1PARTITION || SYS_IND(p) == DM6_AUX3PARTITION) {
(void) ide_xlate_1024(dev, 0, " [DM6:AUX]");
} else {
for (i = 0; i < 4 ; i++) {
struct partition *q = &p[i];
if (NR_SECTS(q)
&& (q->sector & 63) == 1
&& (q->end_sector & 63) == 63) {
unsigned int heads = q->end_head + 1;
if (heads == 32 || heads == 64 || heads == 128 || heads == 255) {
(void) ide_xlate_1024(dev, heads, " [PTBL]");
break;
}
}
}
}
}
#endif
current_minor += 4;
for (i=1 ; i<=4 ; minor++,i++,p++) {
if (!NR_SECTS(p))
continue;
add_partition(hd, minor, first_sector+START_SECT(p), NR_SECTS(p));
if (is_extended_partition(p)) {
printk(" <");
hd->sizes[minor] = hd->part[minor].nr_sects
>> (BLOCK_SIZE_BITS - 9);
extended_partition(hd, MKDEV(hd->major, minor));
printk(" >");
if (hd->part[minor].nr_sects > 2)
hd->part[minor].nr_sects = 2;
}
#ifdef CONFIG_BSD_DISKLABEL
if (SYS_IND(p) == BSD_PARTITION) {
printk(" <");
bsd_disklabel_partition(hd, MKDEV(hd->major, minor));
printk(" >");
}
#endif
}
if (*(unsigned short *) (data+0xfc) == 0x55AA) {
p = (struct partition *) (0x1be + data);
for (i = 4 ; i < 16 ; i++, current_minor++) {
p--;
if ((current_minor & mask) == 0)
break;
if (!(START_SECT(p) && NR_SECTS(p)))
continue;
add_partition(hd, current_minor, START_SECT(p), NR_SECTS(p));
}
}
printk("\n");
brelse(bh);
return 1;
}
#endif
#ifdef CONFIG_OSF_PARTITION
static int osf_partition(struct gendisk *hd, unsigned int dev, unsigned long first_sector)
{
int i;
int mask = (1 << hd->minor_shift) - 1;
struct buffer_head *bh;
struct disklabel {
u32 d_magic;
u16 d_type,d_subtype;
u8 d_typename[16];
u8 d_packname[16];
u32 d_secsize;
u32 d_nsectors;
u32 d_ntracks;
u32 d_ncylinders;
u32 d_secpercyl;
u32 d_secprtunit;
u16 d_sparespertrack;
u16 d_sparespercyl;
u32 d_acylinders;
u16 d_rpm, d_interleave, d_trackskew, d_cylskew;
u32 d_headswitch, d_trkseek, d_flags;
u32 d_drivedata[5];
u32 d_spare[5];
u32 d_magic2;
u16 d_checksum;
u16 d_npartitions;
u32 d_bbsize, d_sbsize;
struct d_partition {
u32 p_size;
u32 p_offset;
u32 p_fsize;
u8  p_fstype;
u8  p_frag;
u16 p_cpg;
} d_partitions[8];
} * label;
struct d_partition * partition;
#define DISKLABELMAGIC (0x82564557UL)
if (!(bh = bread(dev,0,1024))) {
printk("unable to read partition table\n");
return -1;
}
label = (struct disklabel *) (bh->b_data+64);
partition = label->d_partitions;
if (label->d_magic != DISKLABELMAGIC) {
printk("magic: %08x\n", label->d_magic);
brelse(bh);
return 0;
}
if (label->d_magic2 != DISKLABELMAGIC) {
printk("magic2: %08x\n", label->d_magic2);
brelse(bh);
return 0;
}
for (i = 0 ; i < label->d_npartitions; i++, partition++) {
if ((current_minor & mask) == 0)
break;
if (partition->p_size)
add_partition(hd, current_minor,
first_sector+partition->p_offset,
partition->p_size);
current_minor++;
}
printk("\n");
brelse(bh);
return 1;
}
#endif
#ifdef CONFIG_SUN_PARTITION
static int sun_partition(struct gendisk *hd, kdev_t dev, unsigned long first_sector)
{
int i, csum;
unsigned short *ush;
struct buffer_head *bh;
struct sun_disklabel {
unsigned char info[128];
unsigned char spare[292];
unsigned short rspeed;
unsigned short pcylcount;
unsigned short sparecyl;
unsigned char spare2[4];
unsigned short ilfact;
unsigned short ncyl;
unsigned short nacyl;
unsigned short ntrks;
unsigned short nsect;
unsigned char spare3[4];
struct sun_partition {
__u32 start_cylinder;
__u32 num_sectors;
} partitions[8];
unsigned short magic;
unsigned short csum;
} * label;
struct sun_partition *p;
int other_endian;
unsigned long spc;
#define SUN_LABEL_MAGIC          0xDABE
#define SUN_LABEL_MAGIC_SWAPPED  0xBEDA
#define SWAP16(x)  (other_endian ? (((__u16)(x) & 0xFF) << 8) \
| (((__u16)(x) & 0xFF00) >> 8) \
: (__u16)(x))
#define SWAP32(x)  (other_endian ? (((__u32)(x) & 0xFF) << 24) \
| (((__u32)(x) & 0xFF00) << 8) \
| (((__u32)(x) & 0xFF0000) >> 8) \
| (((__u32)(x) & 0xFF000000) >> 24) \
: (__u32)(x))
if(!(bh = bread(dev, 0, 1024))) {
printk("Dev %s: unable to read partition table\n",
kdevname(dev));
return -1;
}
label = (struct sun_disklabel *) bh->b_data;
p = label->partitions;
if (label->magic != SUN_LABEL_MAGIC && label->magic != SUN_LABEL_MAGIC_SWAPPED) {
printk("Dev %s Sun disklabel: bad magic %04x\n",
kdevname(dev), label->magic);
brelse(bh);
return 0;
}
other_endian = (label->magic == SUN_LABEL_MAGIC_SWAPPED);
ush = ((unsigned short *) (label+1)) - 1;
for(csum = 0; ush >= ((unsigned short *) label);)
csum ^= *ush--;
if(csum) {
printk("Dev %s Sun disklabel: Csum bad, label corrupted\n",
kdevname(dev));
brelse(bh);
return 0;
}
spc = SWAP16(label->ntrks) * SWAP16(label->nsect);
for(i=0; i < 8; i++, p++) {
unsigned long st_sector;
st_sector = first_sector + SWAP32(p->start_cylinder) * spc;
add_partition(hd, current_minor, st_sector, SWAP32(p->num_sectors));
current_minor++;
}
printk("\n");
brelse(bh);
return 1;
#undef SWAP16
#undef SWAP32
}
#endif
#ifdef CONFIG_AMIGA_PARTITION
#include <asm/byteorder.h>
#include <linux/affs_hardblocks.h>
static __inline__ __u32
checksum_block(__u32 *m, int size)
{
__u32 sum = 0;
while (size--)
sum += htonl(*m++);
return sum;
}
static int
amiga_partition(struct gendisk *hd, unsigned int dev, unsigned long first_sector)
{
struct buffer_head	*bh;
struct RigidDiskBlock	*rdb;
struct PartitionBlock	*pb;
int			 start_sect;
int			 nr_sects;
int			 blk;
int			 part, res;
set_blocksize(dev,512);
res = 0;
for (blk = 0; blk < RDB_ALLOCATION_LIMIT; blk++) {
if(!(bh = bread(dev,blk,512))) {
printk("Dev %d: unable to read RDB block %d\n",dev,blk);
goto rdb_done;
}
if (*(__u32 *)bh->b_data == htonl(IDNAME_RIGIDDISK)) {
rdb = (struct RigidDiskBlock *)bh->b_data;
if (checksum_block((__u32 *)bh->b_data,htonl(rdb->rdb_SummedLongs) & 0x7F)) {
printk("Dev %d: RDB in block %d has bad checksum\n",dev,blk);
brelse(bh);
continue;
}
printk(" RDSK");
blk = htonl(rdb->rdb_PartitionList);
brelse(bh);
for (part = 1; blk > 0 && part <= 16; part++) {
if (!(bh = bread(dev,blk,512))) {
printk("Dev %d: unable to read partition block %d\n",
dev,blk);
goto rdb_done;
}
pb  = (struct PartitionBlock *)bh->b_data;
blk = htonl(pb->pb_Next);
if (pb->pb_ID == htonl(IDNAME_PARTITION) && checksum_block(
(__u32 *)pb,htonl(pb->pb_SummedLongs) & 0x7F) == 0 ) {
if (!(nr_sects = (htonl(pb->pb_Environment[10]) + 1 -
htonl(pb->pb_Environment[9])) *
htonl(pb->pb_Environment[3]) *
htonl(pb->pb_Environment[5]))) {
continue;
}
start_sect = htonl(pb->pb_Environment[9]) *
htonl(pb->pb_Environment[3]) *
htonl(pb->pb_Environment[5]);
add_partition(hd,current_minor,start_sect,nr_sects);
current_minor++;
res = 1;
}
brelse(bh);
}
printk("\n");
break;
}
}
rdb_done:
set_blocksize(dev,BLOCK_SIZE);
return res;
}
#endif
static void check_partition(struct gendisk *hd, kdev_t dev)
{
static int first_time = 1;
unsigned long first_sector;
char buf[8];
if (first_time)
printk("Partition check:\n");
first_time = 0;
first_sector = hd->part[MINOR(dev)].start_sect;
if ((int)first_sector == -1) {
hd->part[MINOR(dev)].start_sect = 0;
return;
}
printk(" %s:", disk_name(hd, MINOR(dev), buf));
#ifdef CONFIG_MSDOS_PARTITION
if (msdos_partition(hd, dev, first_sector))
return;
#endif
#ifdef CONFIG_OSF_PARTITION
if (osf_partition(hd, dev, first_sector))
return;
#endif
#ifdef CONFIG_SUN_PARTITION
if(sun_partition(hd, dev, first_sector))
return;
#endif
#ifdef CONFIG_AMIGA_PARTITION
if(amiga_partition(hd, dev, first_sector))
return;
#endif
printk(" unknown partition table\n");
}
void resetup_one_dev(struct gendisk *dev, int drive)
{
int i;
int first_minor	= drive << dev->minor_shift;
int end_minor	= first_minor + dev->max_p;
blk_size[dev->major] = NULL;
current_minor = 1 + first_minor;
check_partition(dev, MKDEV(dev->major, first_minor));
if (dev->sizes != NULL) {
for (i = first_minor; i < end_minor; i++)
dev->sizes[i] = dev->part[i].nr_sects >> (BLOCK_SIZE_BITS - 9);
blk_size[dev->major] = dev->sizes;
}
}
static void setup_dev(struct gendisk *dev)
{
int i, drive;
int end_minor	= dev->max_nr * dev->max_p;
blk_size[dev->major] = NULL;
for (i = 0 ; i < end_minor; i++) {
dev->part[i].start_sect = 0;
dev->part[i].nr_sects = 0;
}
dev->init(dev);
for (drive = 0 ; drive < dev->nr_real ; drive++) {
int first_minor	= drive << dev->minor_shift;
current_minor = 1 + first_minor;
check_partition(dev, MKDEV(dev->major, first_minor));
}
if (dev->sizes != NULL) {
for (i = 0; i < end_minor; i++)
dev->sizes[i] = dev->part[i].nr_sects >> (BLOCK_SIZE_BITS - 9);
blk_size[dev->major] = dev->sizes;
}
}
void device_setup(void)
{
extern void console_map_init(void);
struct gendisk *p;
int nr=0;
chr_dev_init();
blk_dev_init();
sti();
#ifdef CONFIG_SCSI
scsi_dev_init();
#endif
#ifdef CONFIG_INET
net_dev_init();
#endif
console_map_init();
for (p = gendisk_head ; p ; p=p->next) {
setup_dev(p);
nr += p->nr_real;
}
#ifdef CONFIG_BLK_DEV_RAM
#ifdef CONFIG_BLK_DEV_INITRD
if (initrd_start && mount_initrd) initrd_load();
else
#endif
rd_load();
#endif
}