#ifndef TTTYPES_H
#define TTTYPES_H
#include "ttconfig.h"
#include "tttype.h"
#ifdef DEBUG
#ifndef ARM_1212
#include <stdlib.h>
#include <stdio.h>
#else
#include "std.h"
#endif
#endif
typedef unsigned char   Byte;
typedef unsigned short  UShort;
typedef signed   short  Short;
typedef unsigned long   ULong;
typedef signed   long   Long;
#if SIZEOF_INT == 4
typedef signed int      Fixed;
#elif SIZEOF_LONG == 4
typedef signed long     Fixed;
#else
#error "no 32bit type found"
#endif
typedef int             Int;
typedef long            Integer;
typedef Byte*    PByte;
typedef UShort*  PUShort;
typedef Short*   PShort;
typedef ULong*   PULong;
typedef Long*    PLong;
typedef Fixed*   PFixed;
typedef Int*     PInt;
typedef void*    Pointer;
typedef TT_F26Dot6*     PCoordinates;
typedef unsigned char*  PTouchTable;
#ifndef Bool
typedef int  Bool;
#endif
#ifndef TRUE
#define TRUE  1
#endif
#ifndef FALSE
#define FALSE  0
#endif
#ifndef NULL
#define NULL  (void*)0
#endif
#ifdef Plan9
#ifdef Tamd64
typedef unsigned long long* PStorage;
#else
typedef unsigned int* PStorage;
#endif
#elif   ARCH_SIZEOF_PTR == SIZEOF_LONG
typedef long*  PStorage;
#elif ARCH_SIZEOF_PTR == SIZEOF_INT
typedef int*   PStorage;
#else
#error "Size of pointer type is not equal to either long or int"
#endif
#define TT_Round_Off             5
#define TT_Round_To_Half_Grid    0
#define TT_Round_To_Grid         1
#define TT_Round_To_Double_Grid  2
#define TT_Round_Up_To_Grid      4
#define TT_Round_Down_To_Grid    3
#define TT_Round_Super           6
#define TT_Round_Super_45        7
#define TT_Flag_On_Curve      1
#define TT_Flag_Touched_X     2
#define TT_Flag_Touched_Y     4
#define TT_Flag_Touched_Both  6
#define SUCCESS  0
#define FAILURE  -1
#ifndef MIN
#define MIN( a, b )  ( (a) < (b) ? (a) : (b) )
#endif
#ifndef MAX
#define MAX( a, b )  ( (a) > (b) ? (a) : (b) )
#endif
#ifndef ABS
#define ABS( a )     ( (a) < 0 ? -(a) : (a) )
#endif
#define HANDLE_Val( handle )       ((handle).z)
#define HANDLE_Engine( handle )    ((PEngine_Instance)HANDLE_Val( handle ))
#define HANDLE_Face( handle )      ((PFace)HANDLE_Val( handle ))
#define HANDLE_Instance( handle )  ((PInstance)HANDLE_Val( handle ))
#define HANDLE_Glyph( handle )     ((PGlyph)HANDLE_Val( handle ))
#define HANDLE_CharMap( handle )   ((PCMapTable)HANDLE_Val( handle ))
#define HANDLE_Set( handle, val )  ((handle).z = (void*)(val))
#endif