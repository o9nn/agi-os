#ifndef	_GLOBAL_H
#define	_GLOBAL_H
#include <stdbool.h>
#include <sys/types.h>
#include "mig_string.h"
extern bool DefaultFiles;
extern bool BeQuiet;
extern bool BeVerbose;
extern bool GenSymTab;
extern bool IsKernelUser;
extern bool IsKernelServer;
extern const_string_t RCSId;
extern const_string_t SubsystemName;
extern u_int SubsystemBase;
extern const_string_t MsgOption;
extern const_string_t WaitTime;
extern const_string_t ServerPrefix;
extern const_string_t UserPrefix;
extern const_string_t ServerDemux;
extern const_string_t SubrPrefix;
extern const_string_t RoutinePrefix;
extern const_string_t OOLPostfix;
extern int yylineno;
extern string_t yyinname;
extern void init_global(void);
extern string_t UserFilePrefix;
extern string_t UserHeaderFileName;
extern string_t ServerHeaderFileName;
extern string_t InternalHeaderFileName;
extern string_t UserFileName;
extern string_t ServerFileName;
extern size_t port_size;
extern size_t port_size_in_bits;
extern size_t complex_alignof;
#define IS_64BIT_ABI (desired_complex_alignof == 8)
extern void more_global(void);
#ifndef NULL
#define NULL 0
#endif
#endif