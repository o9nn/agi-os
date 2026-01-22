#ifndef gdebug_INCLUDED
# define gdebug_INCLUDED
extern char gs_debug[128];
bool gs_debug_c(int );
#define gs_log_errors gs_debug['#']
extern FILE *gs_debug_out;
#ifdef DEBUG
#undef dstderr
#define dstderr gs_debug_out
#undef estderr
#define estderr gs_debug_out
#endif
#ifdef DEBUG
# define if_debug0(c,s)\
BEGIN if (gs_debug_c(c)) dlprintf(s); END
# define if_debug1(c,s,a1)\
BEGIN if (gs_debug_c(c)) dlprintf1(s,a1); END
# define if_debug2(c,s,a1,a2)\
BEGIN if (gs_debug_c(c)) dlprintf2(s,a1,a2); END
# define if_debug3(c,s,a1,a2,a3)\
BEGIN if (gs_debug_c(c)) dlprintf3(s,a1,a2,a3); END
# define if_debug4(c,s,a1,a2,a3,a4)\
BEGIN if (gs_debug_c(c)) dlprintf4(s,a1,a2,a3,a4); END
# define if_debug5(c,s,a1,a2,a3,a4,a5)\
BEGIN if (gs_debug_c(c)) dlprintf5(s,a1,a2,a3,a4,a5); END
# define if_debug6(c,s,a1,a2,a3,a4,a5,a6)\
BEGIN if (gs_debug_c(c)) dlprintf6(s,a1,a2,a3,a4,a5,a6); END
# define if_debug7(c,s,a1,a2,a3,a4,a5,a6,a7)\
BEGIN if (gs_debug_c(c)) dlprintf7(s,a1,a2,a3,a4,a5,a6,a7); END
# define if_debug8(c,s,a1,a2,a3,a4,a5,a6,a7,a8)\
BEGIN if (gs_debug_c(c)) dlprintf8(s,a1,a2,a3,a4,a5,a6,a7,a8); END
# define if_debug9(c,s,a1,a2,a3,a4,a5,a6,a7,a8,a9)\
BEGIN if (gs_debug_c(c)) dlprintf9(s,a1,a2,a3,a4,a5,a6,a7,a8,a9); END
# define if_debug10(c,s,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)\
BEGIN if (gs_debug_c(c)) dlprintf10(s,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10); END
# define if_debug11(c,s,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11)\
BEGIN if (gs_debug_c(c)) dlprintf11(s,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11); END
# define if_debug12(c,s,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)\
BEGIN if (gs_debug_c(c)) dlprintf12(s,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12); END
#else
# define if_debug0(c,s) DO_NOTHING
# define if_debug1(c,s,a1) DO_NOTHING
# define if_debug2(c,s,a1,a2) DO_NOTHING
# define if_debug3(c,s,a1,a2,a3) DO_NOTHING
# define if_debug4(c,s,a1,a2,a3,a4) DO_NOTHING
# define if_debug5(c,s,a1,a2,a3,a4,a5) DO_NOTHING
# define if_debug6(c,s,a1,a2,a3,a4,a5,a6) DO_NOTHING
# define if_debug7(c,s,a1,a2,a3,a4,a5,a6,a7) DO_NOTHING
# define if_debug8(c,s,a1,a2,a3,a4,a5,a6,a7,a8) DO_NOTHING
# define if_debug9(c,s,a1,a2,a3,a4,a5,a6,a7,a8,a9) DO_NOTHING
# define if_debug10(c,s,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) DO_NOTHING
# define if_debug11(c,s,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) DO_NOTHING
# define if_debug12(c,s,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) DO_NOTHING
#endif
void debug_dump_bytes(const byte * from, const byte * to,
const char *msg);
void debug_dump_bitmap(const byte * from, uint raster, uint height,
const char *msg);
void debug_print_string(const byte * str, uint len);
void debug_print_string_hex(const byte * str, uint len);
#endif