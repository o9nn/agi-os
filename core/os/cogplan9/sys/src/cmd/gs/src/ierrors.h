#ifndef ierrors_INCLUDED
# define ierrors_INCLUDED
extern const char *const gs_error_names[];
#define e_unknownerror (-1)
#define e_dictfull (-2)
#define e_dictstackoverflow (-3)
#define e_dictstackunderflow (-4)
#define e_execstackoverflow (-5)
#define e_interrupt (-6)
#undef gs_error_interrupt
#define gs_error_interrupt e_interrupt
#define e_invalidaccess (-7)
#define e_invalidexit (-8)
#define e_invalidfileaccess (-9)
#define e_invalidfont (-10)
#define e_invalidrestore (-11)
#define e_ioerror (-12)
#define e_limitcheck (-13)
#define e_nocurrentpoint (-14)
#define e_rangecheck (-15)
#define e_stackoverflow (-16)
#define e_stackunderflow (-17)
#define e_syntaxerror (-18)
#define e_timeout (-19)
#define e_typecheck (-20)
#define e_undefined (-21)
#define e_undefinedfilename (-22)
#define e_undefinedresult (-23)
#define e_unmatchedmark (-24)
#define e_VMerror (-25)
#define LEVEL1_ERROR_NAMES\
"unknownerror", "dictfull", "dictstackoverflow", "dictstackunderflow",\
"execstackoverflow", "interrupt", "invalidaccess", "invalidexit",\
"invalidfileaccess", "invalidfont", "invalidrestore", "ioerror",\
"limitcheck", "nocurrentpoint", "rangecheck", "stackoverflow",\
"stackunderflow", "syntaxerror", "timeout", "typecheck", "undefined",\
"undefinedfilename", "undefinedresult", "unmatchedmark", "VMerror"
#define e_configurationerror (-26)
#define e_invalidcontext (-27)
#define e_undefinedresource (-28)
#define e_unregistered (-29)
#define e_invalidid (-30)
#define LEVEL2_ERROR_NAMES\
"configurationerror", "invalidcontext", "undefinedresource",\
"unregistered", "invalidid"
#define ERROR_NAMES LEVEL1_ERROR_NAMES, LEVEL2_ERROR_NAMES
#define e_Fatal (-100)
#define e_Quit (-101)
#define e_InterpreterExit (-102)
#define e_RemapColor (-103)
#define e_ExecStackUnderflow (-104)
#define e_VMreclaim (-105)
#define e_NeedInput (-106)
#define e_NeedStdin (-107)
#define e_NeedStdout (-108)
#define e_NeedStderr (-109)
#define e_Info (-110)
#define ERROR_IS_INTERRUPT(ecode)\
((ecode) == e_interrupt || (ecode) == e_timeout)
#endif