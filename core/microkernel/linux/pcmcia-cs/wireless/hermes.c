#include <linux/config.h>
#include <linux/module.h>
#include <linux/types.h>
#include <linux/threads.h>
#include <linux/smp.h>
#include <asm/io.h>
#include <linux/delay.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <asm/errno.h>
#include "hermes.h"
MODULE_DESCRIPTION("Low-level driver helper for Lucent Hermes chipset and Prism II HFA384x wireless MAC controller");
MODULE_AUTHOR("David Gibson <hermes@gibson.dropbear.id.au>");
#ifdef MODULE_LICENSE
MODULE_LICENSE("Dual MPL/GPL");
#endif
#define CMD_BUSY_TIMEOUT (100)
#define CMD_INIT_TIMEOUT (50000)
#define CMD_COMPL_TIMEOUT (20000)
#define ALLOC_COMPL_TIMEOUT (1000)
#define IO_TYPE(hw)	((hw)->io_space ? "IO " : "MEM ")
#define DMSG(stuff...) do {printk(KERN_DEBUG "hermes @ %s0x%x: " , IO_TYPE(hw), hw->iobase); \
printk(stuff);} while (0)
#undef HERMES_DEBUG
#ifdef HERMES_DEBUG
#include <stdarg.h>
#define DEBUG(lvl, stuff...) if ( (lvl) <= HERMES_DEBUG) DMSG(stuff)
#else
#define DEBUG(lvl, stuff...) do { } while (0)
#endif
static int hermes_issue_cmd(hermes_t *hw, u16 cmd, u16 param0)
{
int k = CMD_BUSY_TIMEOUT;
u16 reg;
reg = hermes_read_regn(hw, CMD);
while ( (reg & HERMES_CMD_BUSY) && k ) {
k--;
udelay(1);
reg = hermes_read_regn(hw, CMD);
}
if (reg & HERMES_CMD_BUSY) {
return -EBUSY;
}
hermes_write_regn(hw, PARAM2, 0);
hermes_write_regn(hw, PARAM1, 0);
hermes_write_regn(hw, PARAM0, param0);
hermes_write_regn(hw, CMD, cmd);
return 0;
}
void hermes_struct_init(hermes_t *hw, ulong address,
int io_space, int reg_spacing)
{
hw->iobase = address;
hw->io_space = io_space;
hw->reg_spacing = reg_spacing;
hw->inten = 0x0;
#ifdef HERMES_DEBUG_BUFFER
hw->dbufp = 0;
memset(&hw->dbuf, 0xff, sizeof(hw->dbuf));
memset(&hw->profile, 0, sizeof(hw->profile));
#endif
}
int hermes_init(hermes_t *hw)
{
u16 status, reg;
int err = 0;
int k;
hw->inten = 0x0;
hermes_write_regn(hw, INTEN, 0);
hermes_write_regn(hw, EVACK, 0xffff);
k = CMD_BUSY_TIMEOUT;
reg = hermes_read_regn(hw, CMD);
while (k && (reg & HERMES_CMD_BUSY)) {
if (reg == 0xffff)
return -ENODEV;
k--;
udelay(1);
reg = hermes_read_regn(hw, CMD);
}
reg = hermes_read_regn(hw, EVSTAT);
hermes_write_regn(hw, EVACK, reg);
err = hermes_issue_cmd(hw, HERMES_CMD_INIT, 0);
if (err)
return err;
reg = hermes_read_regn(hw, EVSTAT);
k = CMD_INIT_TIMEOUT;
while ( (! (reg & HERMES_EV_CMD)) && k) {
k--;
udelay(10);
reg = hermes_read_regn(hw, EVSTAT);
}
hermes_write_regn(hw, SWSUPPORT0, HERMES_MAGIC);
if (! hermes_present(hw)) {
DEBUG(0, "hermes @ 0x%x: Card removed during reset.\n",
hw->iobase);
err = -ENODEV;
goto out;
}
if (! (reg & HERMES_EV_CMD)) {
printk(KERN_ERR "hermes @ %s0x%lx: "
"Timeout waiting for card to reset (reg=0x%04x)!\n",
IO_TYPE(hw), hw->iobase, reg);
err = -ETIMEDOUT;
goto out;
}
status = hermes_read_regn(hw, STATUS);
hermes_write_regn(hw, EVACK, HERMES_EV_CMD);
if (status & HERMES_STATUS_RESULT)
err = -EIO;
out:
return err;
}
int hermes_docmd_wait(hermes_t *hw, u16 cmd, u16 parm0,
hermes_response_t *resp)
{
int err;
int k;
u16 reg;
u16 status;
err = hermes_issue_cmd(hw, cmd, parm0);
if (err) {
if (! hermes_present(hw)) {
printk(KERN_WARNING "hermes @ %s0x%lx: "
"Card removed while issuing command.\n",
IO_TYPE(hw), hw->iobase);
err = -ENODEV;
} else
printk(KERN_ERR "hermes @ %s0x%lx: Error %d issuing command.\n",
IO_TYPE(hw), hw->iobase, err);
goto out;
}
reg = hermes_read_regn(hw, EVSTAT);
k = CMD_COMPL_TIMEOUT;
while ( (! (reg & HERMES_EV_CMD)) && k) {
k--;
udelay(10);
reg = hermes_read_regn(hw, EVSTAT);
}
if (! hermes_present(hw)) {
printk(KERN_WARNING "hermes @ %s0x%lx: "
"Card removed while waiting for command completion.\n",
IO_TYPE(hw), hw->iobase);
err = -ENODEV;
goto out;
}
if (! (reg & HERMES_EV_CMD)) {
printk(KERN_ERR "hermes @ %s0x%lx: "
"Timeout waiting for command completion.\n",
IO_TYPE(hw), hw->iobase);
err = -ETIMEDOUT;
goto out;
}
status = hermes_read_regn(hw, STATUS);
if (resp) {
resp->status = status;
resp->resp0 = hermes_read_regn(hw, RESP0);
resp->resp1 = hermes_read_regn(hw, RESP1);
resp->resp2 = hermes_read_regn(hw, RESP2);
}
hermes_write_regn(hw, EVACK, HERMES_EV_CMD);
if (status & HERMES_STATUS_RESULT)
err = -EIO;
out:
return err;
}
int hermes_allocate(hermes_t *hw, u16 size, u16 *fid)
{
int err = 0;
int k;
u16 reg;
if ( (size < HERMES_ALLOC_LEN_MIN) || (size > HERMES_ALLOC_LEN_MAX) )
return -EINVAL;
err = hermes_docmd_wait(hw, HERMES_CMD_ALLOC, size, NULL);
if (err) {
return err;
}
reg = hermes_read_regn(hw, EVSTAT);
k = ALLOC_COMPL_TIMEOUT;
while ( (! (reg & HERMES_EV_ALLOC)) && k) {
k--;
udelay(10);
reg = hermes_read_regn(hw, EVSTAT);
}
if (! hermes_present(hw)) {
printk(KERN_WARNING "hermes @ %s0x%lx: "
"Card removed waiting for frame allocation.\n",
IO_TYPE(hw), hw->iobase);
return -ENODEV;
}
if (! (reg & HERMES_EV_ALLOC)) {
printk(KERN_ERR "hermes @ %s0x%lx: "
"Timeout waiting for frame allocation\n",
IO_TYPE(hw), hw->iobase);
return -ETIMEDOUT;
}
*fid = hermes_read_regn(hw, ALLOCFID);
hermes_write_regn(hw, EVACK, HERMES_EV_ALLOC);
return 0;
}
static int hermes_bap_seek(hermes_t *hw, int bap, u16 id, u16 offset)
{
int sreg = bap ? HERMES_SELECT1 : HERMES_SELECT0;
int oreg = bap ? HERMES_OFFSET1 : HERMES_OFFSET0;
int k;
u16 reg;
if ( (offset > HERMES_BAP_OFFSET_MAX) || (offset % 2) )
return -EINVAL;
k = HERMES_BAP_BUSY_TIMEOUT;
reg = hermes_read_reg(hw, oreg);
while ((reg & HERMES_OFFSET_BUSY) && k) {
k--;
udelay(1);
reg = hermes_read_reg(hw, oreg);
}
#ifdef HERMES_DEBUG_BUFFER
hw->profile[HERMES_BAP_BUSY_TIMEOUT - k]++;
if (k < HERMES_BAP_BUSY_TIMEOUT) {
struct hermes_debug_entry *e =
&hw->dbuf[(hw->dbufp++) % HERMES_DEBUG_BUFSIZE];
e->bap = bap;
e->id = id;
e->offset = offset;
e->cycles = HERMES_BAP_BUSY_TIMEOUT - k;
}
#endif
if (reg & HERMES_OFFSET_BUSY)
return -ETIMEDOUT;
hermes_write_reg(hw, sreg, id);
hermes_write_reg(hw, oreg, offset);
k = HERMES_BAP_BUSY_TIMEOUT;
reg = hermes_read_reg(hw, oreg);
while ( (reg & (HERMES_OFFSET_BUSY | HERMES_OFFSET_ERR)) && k) {
k--;
udelay(1);
reg = hermes_read_reg(hw, oreg);
}
if (reg & HERMES_OFFSET_BUSY) {
return -ETIMEDOUT;
}
if (reg & HERMES_OFFSET_ERR) {
return -EIO;
}
return 0;
}
int hermes_bap_pread(hermes_t *hw, int bap, void *buf, unsigned len,
u16 id, u16 offset)
{
int dreg = bap ? HERMES_DATA1 : HERMES_DATA0;
int err = 0;
if ( (len < 0) || (len % 2) )
return -EINVAL;
err = hermes_bap_seek(hw, bap, id, offset);
if (err)
goto out;
hermes_read_words(hw, dreg, buf, len/2);
out:
return err;
}
int hermes_bap_pwrite(hermes_t *hw, int bap, const void *buf, unsigned len,
u16 id, u16 offset)
{
int dreg = bap ? HERMES_DATA1 : HERMES_DATA0;
int err = 0;
if ( (len < 0) || (len % 2) )
return -EINVAL;
err = hermes_bap_seek(hw, bap, id, offset);
if (err)
goto out;
hermes_write_words(hw, dreg, buf, len/2);
out:
return err;
}
int hermes_read_ltv(hermes_t *hw, int bap, u16 rid, unsigned bufsize,
u16 *length, void *buf)
{
int err = 0;
int dreg = bap ? HERMES_DATA1 : HERMES_DATA0;
u16 rlength, rtype;
unsigned nwords;
if ( (bufsize < 0) || (bufsize % 2) )
return -EINVAL;
err = hermes_docmd_wait(hw, HERMES_CMD_ACCESS, rid, NULL);
if (err)
return err;
err = hermes_bap_seek(hw, bap, rid, 0);
if (err)
return err;
rlength = hermes_read_reg(hw, dreg);
if (! rlength)
return -ENOENT;
rtype = hermes_read_reg(hw, dreg);
if (length)
*length = rlength;
if (rtype != rid)
printk(KERN_WARNING "hermes @ %s0x%lx: "
"hermes_read_ltv(): rid  (0x%04x) does not match type (0x%04x)\n",
IO_TYPE(hw), hw->iobase, rid, rtype);
if (HERMES_RECLEN_TO_BYTES(rlength) > bufsize)
printk(KERN_WARNING "hermes @ %s0x%lx: "
"Truncating LTV record from %d to %d bytes. "
"(rid=0x%04x, len=0x%04x)\n",
IO_TYPE(hw), hw->iobase,
HERMES_RECLEN_TO_BYTES(rlength), bufsize, rid, rlength);
nwords = min((unsigned)rlength - 1, bufsize / 2);
hermes_read_words(hw, dreg, buf, nwords);
return 0;
}
int hermes_write_ltv(hermes_t *hw, int bap, u16 rid,
u16 length, const void *value)
{
int dreg = bap ? HERMES_DATA1 : HERMES_DATA0;
int err = 0;
unsigned count;
if (length == 0)
return -EINVAL;
err = hermes_bap_seek(hw, bap, rid, 0);
if (err)
return err;
hermes_write_reg(hw, dreg, length);
hermes_write_reg(hw, dreg, rid);
count = length - 1;
hermes_write_words(hw, dreg, value, count);
err = hermes_docmd_wait(hw, HERMES_CMD_ACCESS | HERMES_CMD_WRITE,
rid, NULL);
return err;
}
EXPORT_SYMBOL(hermes_struct_init);
EXPORT_SYMBOL(hermes_init);
EXPORT_SYMBOL(hermes_docmd_wait);
EXPORT_SYMBOL(hermes_allocate);
EXPORT_SYMBOL(hermes_bap_pread);
EXPORT_SYMBOL(hermes_bap_pwrite);
EXPORT_SYMBOL(hermes_read_ltv);
EXPORT_SYMBOL(hermes_write_ltv);
static int __init init_hermes(void)
{
return 0;
}
static void __exit exit_hermes(void)
{
}
module_init(init_hermes);
module_exit(exit_hermes);