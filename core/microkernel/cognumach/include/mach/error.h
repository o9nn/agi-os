#ifndef _MACH_ERROR_H_
#define _MACH_ERROR_H_
#include <mach/kern_return.h>
#define err_none (mach_error_t)0
#define ERR_SUCCESS (mach_error_t)0
#define err_system(x) (((x)&0x3f)<<26)
#define err_sub(x) (((x)&0xfff)<<14)
#define err_get_system(err) (((err)>>26)&0x3f)
#define err_get_sub(err) (((err)>>14)&0xfff)
#define err_get_code(err) ((err)&0x3fff)
#define system_emask (err_system(0x3f))
#define sub_emask (err_sub(0xfff))
#define code_emask (0x3fff)
#define err_kern err_system(0x0)
#define err_us err_system(0x1)
#define err_server err_system(0x2)
#define err_ipc err_system(0x3)
#define err_mach_ipc err_system(0x4)
#define err_bootstrap err_system(0x5)
#define err_hurd err_system(0x10)
#define err_local err_system(0x3e)
#define err_ipc_compat err_system(0x3f)
#define err_max_system 0x3f
#define err_mig -300
#define err_exec 6000
#define err_unix (err_kern|err_sub(3))
#define unix_err(errno) (err_kern|err_sub(3)|errno)
#define err_dos (err_kern|err_sub(0xd05))
#define err_fluke err_system(0x20)
#ifndef __ASSEMBLER__
typedef kern_return_t mach_error_t;
#endif
#endif