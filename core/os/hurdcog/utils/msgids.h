#ifndef _HURD_MSGIDS_H_
#define _HURD_MSGIDS_H_
struct msgid_info
{
char *name;
char *subsystem;
};
error_t msgids_scan_std (void);
const struct msgid_info *msgid_info (mach_msg_id_t msgid);
extern const struct argp msgid_argp;
#endif