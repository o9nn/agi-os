#ifndef _LINUX_MCA_H
#define _LINUX_MCA_H
extern int MCA_bus;
#define MCA_MAX_SLOT_NR 8
#define MCA_NOTFOUND (-1)
#define MCA_INTEGSCSI (MCA_MAX_SLOT_NR)
#define MCA_INTEGVIDEO (MCA_MAX_SLOT_NR+1)
#define MCA_NUMADAPTERS (MCA_MAX_SLOT_NR+2)
extern int mca_find_adapter(int id, int start);
extern int mca_find_unused_adapter(int id, int start);
extern int mca_isadapter(int slot);
extern int mca_isenabled(int slot);
extern int mca_is_adapter_used(int slot);
extern int mca_mark_as_used(int slot);
extern void mca_mark_as_unused(int slot);
extern unsigned char mca_read_stored_pos(int slot, int reg);
extern void mca_set_adapter_name(int slot, char* name);
extern char* mca_get_adapter_name(int slot);
typedef int (*MCA_ProcFn)(char* buf, int slot, void* dev);
extern void mca_set_adapter_procfn(int slot, MCA_ProcFn, void* dev);
extern unsigned char mca_read_pos(int slot, int reg);
extern void mca_write_pos(int slot, int reg, unsigned char byte);
extern void mca_handle_nmi(void);
#endif