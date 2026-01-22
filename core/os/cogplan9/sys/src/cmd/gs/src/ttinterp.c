#include "ttmisc.h"
#include "ttfoutl.h"
#include "tttypes.h"
#include "ttcalc.h"
#include "ttinterp.h"
#include "ttfinp.h"
#ifdef DEBUG
# define DBG_PAINT CUR.current_face->font->DebugRepaint(CUR.current_face->font);
# define DBG_PRT_FUN CUR.current_face->font->DebugPrint
# define DBG_PRT (void)(!DBG_PRT_FUN ? 0 : DBG_PRT_FUN(CUR.current_face->font
# define DBG_PRINT(fmt) DBG_PRT, fmt))
# define DBG_PRINT1(fmt, a) DBG_PRT, fmt, a))
# define DBG_PRINT3(fmt, a, b, c) DBG_PRT, fmt, a, b, c))
# define DBG_PRINT4(fmt, a, b, c, d) DBG_PRT, fmt, a, b, c, d))
#else
# define DBG_PRT_FUN NULL
# define DBG_PAINT
# define DBG_PRINT(fmt)
# define DBG_PRINT1(fmt, a)
# define DBG_PRINT3(fmt, a, b, c)
# define DBG_PRINT4(fmt, a, b, c, d)
#endif
static int nInstrCount=0;
#ifndef TT_STATIC_INTERPRETER
#define CUR (*exc)
#else
#define CUR cur
static TExecution_Context cur;
#endif
#define INS_ARG EXEC_OPS PStorage args
#define SKIP_Code() SkipCode( EXEC_ARG )
#define GET_ShortIns() GetShortIns( EXEC_ARG )
#define COMPUTE_Funcs() Compute_Funcs( EXEC_ARG )
#define NORMalize( x, y, v ) Normalize( EXEC_ARGS x, y, v )
#define SET_SuperRound( scale, flags ) \
SetSuperRound( EXEC_ARGS scale, flags )
#define INS_Goto_CodeRange( range, ip ) \
Ins_Goto_CodeRange( EXEC_ARGS range, ip )
#define CUR_Func_project( x, y ) CUR.func_project( EXEC_ARGS x, y )
#define CUR_Func_move( z, p, d ) CUR.func_move( EXEC_ARGS z, p, d )
#define CUR_Func_dualproj( x, y ) CUR.func_dualproj( EXEC_ARGS x, y )
#define CUR_Func_freeProj( x, y ) CUR.func_freeProj( EXEC_ARGS x, y )
#define CUR_Func_round( d, c ) CUR.func_round( EXEC_ARGS d, c )
#define CUR_Func_read_cvt( index ) \
CUR.func_read_cvt( EXEC_ARGS index )
#define CUR_Func_write_cvt( index, val ) \
CUR.func_write_cvt( EXEC_ARGS index, val )
#define CUR_Func_move_cvt( index, val ) \
CUR.func_move_cvt( EXEC_ARGS index, val )
#define CURRENT_Ratio() Current_Ratio( EXEC_ARG )
#define CURRENT_Ppem() Current_Ppem( EXEC_ARG )
#define CALC_Length() Calc_Length( EXEC_ARG )
#define INS_SxVTL( a, b, c, d ) Ins_SxVTL( EXEC_ARGS a, b, c, d )
#define COMPUTE_Point_Displacement( a, b, c, d ) \
Compute_Point_Displacement( EXEC_ARGS a, b, c, d )
#define MOVE_Zp2_Point( a, b, c, t ) Move_Zp2_Point( EXEC_ARGS a, b, c, t )
#define CUR_Ppem() Cur_PPEM( EXEC_ARG )
typedef void (*TInstruction_Function)( INS_ARG );
#define BOUNDS(x,n) ( x < 0 || x >= n )
#ifndef ABS
#define ABS(x) ( (x) < 0 ? -(x) : (x) )
#endif
#define THROW_PATENTED longjmp(CUR.trap, TT_Err_Invalid_Engine)
static unsigned char Pop_Push_Count[512] =
{
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
0, 2,
0, 2,
0, 0,
5, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
0, 0,
0, 0,
1, 0,
0, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 2,
1, 0,
0, 0,
2, 2,
0, 1,
1, 1,
1, 0,
2, 0,
0, 0,
1, 0,
2, 0,
1, 0,
1, 0,
0, 0,
1, 0,
1, 0,
0, 0,
0, 0,
0, 0,
0, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
0, 0,
2, 0,
2, 0,
0, 0,
0, 0,
2, 0,
2, 0,
0, 0,
0, 0,
2, 0,
1, 1,
2, 0,
1, 1,
1, 1,
1, 1,
2, 0,
2, 1,
2, 1,
0, 1,
0, 1,
0, 0,
0, 0,
1, 0,
2, 1,
2, 1,
2, 1,
2, 1,
2, 1,
2, 1,
1, 1,
1, 1,
1, 0,
0, 0,
2, 1,
2, 1,
1, 1,
1, 0,
1, 0,
1, 0,
2, 1,
2, 1,
2, 1,
2, 1,
1, 1,
1, 1,
1, 1,
1, 1,
1, 1,
1, 1,
1, 1,
1, 1,
1, 1,
1, 1,
1, 1,
1, 1,
2, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
2, 0,
2, 0,
0, 0,
0, 0,
0, 0,
0, 0,
1, 0,
1, 0,
0, 0,
2, 0,
2, 0,
0, 0,
0, 0,
1, 0,
2, 0,
2, 0,
1, 1,
1, 0,
3, 3,
2, 1,
2, 1,
1, 0,
2, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 0,
0, 1,
0, 2,
0, 3,
0, 4,
0, 5,
0, 6,
0, 7,
0, 8,
0, 1,
0, 2,
0, 3,
0, 4,
0, 5,
0, 6,
0, 7,
0, 8,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
1, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0,
2, 0
};
static TT_F26Dot6 Norm( TT_F26Dot6 X, TT_F26Dot6 Y )
{
Int64 T1, T2;
MUL_64( X, X, T1 );
MUL_64( Y, Y, T2 );
ADD_64( T1, T2, T1 );
return (TT_F26Dot6)SQRT_64( T1 );
}
static TT_F26Dot6 FUnits_To_Pixels( EXEC_OPS Int distance )
{
return MulDiv_Round( distance,
CUR.metrics.scale1,
CUR.metrics.scale2 );
}
static Long Current_Ratio( EXEC_OP )
{
if ( CUR.metrics.ratio )
return CUR.metrics.ratio;
if ( CUR.GS.projVector.y == 0 )
CUR.metrics.ratio = CUR.metrics.x_ratio;
else if ( CUR.GS.projVector.x == 0 )
CUR.metrics.ratio = CUR.metrics.y_ratio;
else
{
Long x, y;
x = MulDiv_Round( CUR.GS.projVector.x, CUR.metrics.x_ratio, 0x4000 );
y = MulDiv_Round( CUR.GS.projVector.y, CUR.metrics.y_ratio, 0x4000 );
CUR.metrics.ratio = Norm( x, y );
}
return CUR.metrics.ratio;
}
static Int Current_Ppem( EXEC_OP )
{
return MulDiv_Round( CUR.metrics.ppem, CURRENT_Ratio(), 0x10000 );
}
static TT_F26Dot6 Read_CVT( EXEC_OPS Int index )
{
return CUR.cvt[index];
}
static TT_F26Dot6 Read_CVT_Stretched( EXEC_OPS Int index )
{
return MulDiv_Round( CUR.cvt[index], CURRENT_Ratio(), 0x10000 );
}
static void Write_CVT( EXEC_OPS Int index, TT_F26Dot6 value )
{
int ov=CUR.cvt[index];
CUR.cvt[index] = value;
DBG_PRINT3(" cvt[%d]%d:=%d", index, ov, CUR.cvt[index]);
}
static void Write_CVT_Stretched( EXEC_OPS Int index, TT_F26Dot6 value )
{
int ov=CUR.cvt[index];
CUR.cvt[index] = MulDiv_Round( value, 0x10000, CURRENT_Ratio() );
DBG_PRINT3(" cvt[%d]%d:=%d", index, ov, CUR.cvt[index]);
}
static void Move_CVT( EXEC_OPS Int index, TT_F26Dot6 value )
{
int ov=CUR.cvt[index];
CUR.cvt[index] += value;
DBG_PRINT3(" cvt[%d]%d:=%d", index, ov, CUR.cvt[index]);
}
static void Move_CVT_Stretched( EXEC_OPS Int index, TT_F26Dot6 value )
{
int ov=CUR.cvt[index];
CUR.cvt[index] += MulDiv_Round( value, 0x10000, CURRENT_Ratio() );
DBG_PRINT3(" cvt[%d]%d:=%d", index, ov, CUR.cvt[index]);
}
static Bool Calc_Length( EXEC_OP )
{
CUR.opcode = CUR.code[CUR.IP];
switch ( CUR.opcode )
{
case 0x40:
if ( CUR.IP + 1 >= CUR.codeSize )
return FAILURE;
CUR.length = CUR.code[CUR.IP + 1] + 2;
break;
case 0x41:
if ( CUR.IP + 1 >= CUR.codeSize )
return FAILURE;
CUR.length = CUR.code[CUR.IP + 1] * 2 + 2;
break;
case 0xB0:
case 0xB1:
case 0xB2:
case 0xB3:
case 0xB4:
case 0xB5:
case 0xB6:
case 0xB7:
CUR.length = CUR.opcode - 0xB0 + 2;
break;
case 0xB8:
case 0xB9:
case 0xBA:
case 0xBB:
case 0xBC:
case 0xBD:
case 0xBE:
case 0xBF:
CUR.length = (CUR.opcode - 0xB8) * 2 + 3;
break;
default:
CUR.length = 1;
break;
}
if ( CUR.IP + CUR.length > CUR.codeSize )
return FAILURE;
return SUCCESS;
}
static Short GetShortIns( EXEC_OP )
{
CUR.IP += 2;
return ( CUR.code[CUR.IP-2] << 8) +
CUR.code[CUR.IP-1];
}
static Bool Ins_Goto_CodeRange( EXEC_OPS Int aRange, Int aIP )
{
TCodeRange* WITH;
if ( aRange < 1 || aRange > 3 )
{
CUR.error = TT_Err_Bad_Argument;
return FAILURE;
}
WITH = &CUR.codeRangeTable[aRange - 1];
if ( WITH->Base == NULL )
{
CUR.error = TT_Err_Invalid_CodeRange;
return FAILURE;
}
if ( aIP > WITH->Size )
{
CUR.error = TT_Err_Code_Overflow;
return FAILURE;
}
CUR.code = WITH->Base;
CUR.codeSize = WITH->Size;
CUR.IP = aIP;
CUR.curRange = aRange;
return SUCCESS;
}
static void Direct_Move( EXEC_OPS PGlyph_Zone zone,
Int point,
TT_F26Dot6 distance )
{
TT_F26Dot6 v;
v = CUR.GS.freeVector.x;
if ( v != 0 )
{
zone->cur_x[point] += MulDiv_Round( distance,
v * 0x10000L,
CUR.F_dot_P );
zone->touch[point] |= TT_Flag_Touched_X;
}
v = CUR.GS.freeVector.y;
if ( v != 0 )
{
zone->cur_y[point] += MulDiv_Round( distance,
v * 0x10000L,
CUR.F_dot_P );
zone->touch[point] |= TT_Flag_Touched_Y;
}
}
static void Direct_Move_X( EXEC_OPS PGlyph_Zone zone,
Int point,
TT_F26Dot6 distance )
{ (void)exc;
zone->cur_x[point] += distance;
zone->touch[point] |= TT_Flag_Touched_X;
}
static void Direct_Move_Y( EXEC_OPS PGlyph_Zone zone,
Int point,
TT_F26Dot6 distance )
{ (void)exc;
zone->cur_y[point] += distance;
zone->touch[point] |= TT_Flag_Touched_Y;
}
static TT_F26Dot6 Round_None( EXEC_OPS TT_F26Dot6 distance,
TT_F26Dot6 compensation )
{
TT_F26Dot6 val;
(void)exc;
if ( distance >= 0 )
{
val = distance + compensation;
if ( val < 0 )
val = 0;
}
else {
val = distance - compensation;
if ( val > 0 )
val = 0;
}
return val;
}
static TT_F26Dot6 Round_To_Grid( EXEC_OPS TT_F26Dot6 distance,
TT_F26Dot6 compensation )
{
TT_F26Dot6 val;
(void)exc;
if ( distance >= 0 )
{
val = (distance + compensation + 32) & (-64);
if ( val < 0 )
val = 0;
}
else
{
val = -( (compensation - distance + 32) & (-64) );
if ( val > 0 )
val = 0;
}
return val;
}
static TT_F26Dot6 Round_To_Half_Grid( EXEC_OPS TT_F26Dot6 distance,
TT_F26Dot6 compensation )
{
TT_F26Dot6 val;
(void)exc;
if ( distance >= 0 )
{
val = ((distance + compensation) & (-64)) + 32;
if ( val < 0 )
val = 0;
}
else
{
val = -( ((compensation - distance) & (-64)) + 32 );
if ( val > 0 )
val = 0;
}
return val;
}
static TT_F26Dot6 Round_Down_To_Grid( EXEC_OPS TT_F26Dot6 distance,
TT_F26Dot6 compensation )
{
TT_F26Dot6 val;
(void)exc;
if ( distance >= 0 )
{
val = (distance + compensation) & (-64);
if ( val < 0 )
val = 0;
}
else
{
val = -( (compensation - distance) & (-64) );
if ( val > 0 )
val = 0;
}
return val;
}
static TT_F26Dot6 Round_Up_To_Grid( EXEC_OPS TT_F26Dot6 distance,
TT_F26Dot6 compensation )
{
TT_F26Dot6 val;
(void)exc;
if ( distance >= 0 )
{
val = (distance + compensation + 63) & (-64);
if ( val < 0 )
val = 0;
}
else
{
val = -( (compensation - distance + 63) & (-64) );
if ( val > 0 )
val = 0;
}
return val;
}
static TT_F26Dot6 Round_To_Double_Grid( EXEC_OPS TT_F26Dot6 distance,
TT_F26Dot6 compensation )
{
TT_F26Dot6 val;
(void)exc;
if ( distance >= 0 )
{
val = (distance + compensation + 16) & (-32);
if ( val < 0 )
val = 0;
}
else
{
val = -( (compensation - distance + 16) & (-32) );
if ( val > 0 )
val = 0;
}
return val;
}
static TT_F26Dot6 Round_Super( EXEC_OPS TT_F26Dot6 distance,
TT_F26Dot6 compensation )
{
TT_F26Dot6 val;
if ( distance >= 0 )
{
val = (distance - CUR.phase + CUR.threshold + compensation) &
(-CUR.period);
if ( val < 0 )
val = 0;
val += CUR.phase;
}
else
{
val = -( (CUR.threshold - CUR.phase - distance + compensation) &
(-CUR.period) );
if ( val > 0 )
val = 0;
val -= CUR.phase;
}
return val;
}
static TT_F26Dot6 Round_Super_45( EXEC_OPS TT_F26Dot6 distance,
TT_F26Dot6 compensation )
{
TT_F26Dot6 val;
if ( distance >= 0 )
{
val = ( (distance - CUR.phase + CUR.threshold + compensation) /
CUR.period ) * CUR.period;
if ( val < 0 )
val = 0;
val += CUR.phase;
}
else
{
val = -( ( (CUR.threshold - CUR.phase - distance + compensation) /
CUR.period ) * CUR.period );
if ( val > 0 )
val = 0;
val -= CUR.phase;
}
return val;
}
static void Compute_Round( EXEC_OPS Byte round_mode )
{
switch ( round_mode )
{
case TT_Round_Off:
CUR.func_round = (TRound_Function)Round_None;
break;
case TT_Round_To_Grid:
CUR.func_round = (TRound_Function)Round_To_Grid;
break;
case TT_Round_Up_To_Grid:
CUR.func_round = (TRound_Function)Round_Up_To_Grid;
break;
case TT_Round_Down_To_Grid:
CUR.func_round = (TRound_Function)Round_Down_To_Grid;
break;
case TT_Round_To_Half_Grid:
CUR.func_round = (TRound_Function)Round_To_Half_Grid;
break;
case TT_Round_To_Double_Grid:
CUR.func_round = (TRound_Function)Round_To_Double_Grid;
break;
case TT_Round_Super:
CUR.func_round = (TRound_Function)Round_Super;
break;
case TT_Round_Super_45:
CUR.func_round = (TRound_Function)Round_Super_45;
break;
}
}
static void SetSuperRound( EXEC_OPS TT_F26Dot6 GridPeriod,
Long selector )
{
switch ( selector & 0xC0 )
{
case 0:
CUR.period = GridPeriod / 2;
break;
case 0x40:
CUR.period = GridPeriod;
break;
case 0x80:
CUR.period = GridPeriod * 2;
break;
case 0xC0:
CUR.period = GridPeriod;
break;
}
switch ( selector & 0x30 )
{
case 0:
CUR.phase = 0;
break;
case 0x10:
CUR.phase = CUR.period / 4;
break;
case 0x20:
CUR.phase = CUR.period / 2;
break;
case 0x30:
CUR.phase = GridPeriod * 3 / 4;
break;
}
if ( (selector & 0x0F) == 0 )
CUR.threshold = CUR.period - 1;
else
CUR.threshold = ( (Int)(selector & 0x0F) - 4L ) * CUR.period / 8;
CUR.period /= 256;
CUR.phase /= 256;
CUR.threshold /= 256;
}
static TT_F26Dot6 Project( EXEC_OPS TT_F26Dot6 Vx, TT_F26Dot6 Vy )
{
THROW_PATENTED;
return 0;
}
static TT_F26Dot6 Dual_Project( EXEC_OPS TT_F26Dot6 Vx, TT_F26Dot6 Vy )
{
THROW_PATENTED;
return 0;
}
static TT_F26Dot6 Free_Project( EXEC_OPS TT_F26Dot6 Vx, TT_F26Dot6 Vy )
{
THROW_PATENTED;
return 0;
}
static TT_F26Dot6 Project_x( EXEC_OPS TT_F26Dot6 Vx, TT_F26Dot6 Vy )
{ (void)exc; (void)Vy;
return Vx;
}
static TT_F26Dot6 Project_y( EXEC_OPS TT_F26Dot6 Vx, TT_F26Dot6 Vy )
{ (void)exc; (void)Vx;
return Vy;
}
static void Compute_Funcs( EXEC_OP )
{
if ( CUR.GS.freeVector.x == 0x4000 )
{
CUR.func_freeProj = (TProject_Function)Project_x;
CUR.F_dot_P = CUR.GS.projVector.x * 0x10000L;
}
else
{
if ( CUR.GS.freeVector.y == 0x4000 )
{
CUR.func_freeProj = (TProject_Function)Project_y;
CUR.F_dot_P = CUR.GS.projVector.y * 0x10000L;
}
else
{
CUR.func_move = (TMove_Function)Direct_Move;
CUR.func_freeProj = (TProject_Function)Free_Project;
CUR.F_dot_P = (Long)CUR.GS.projVector.x * CUR.GS.freeVector.x * 4 +
(Long)CUR.GS.projVector.y * CUR.GS.freeVector.y * 4;
}
}
CUR.cached_metrics = FALSE;
if ( CUR.GS.projVector.x == 0x4000 )
CUR.func_project = (TProject_Function)Project_x;
else
{
if ( CUR.GS.projVector.y == 0x4000 )
CUR.func_project = (TProject_Function)Project_y;
else
CUR.func_project = (TProject_Function)Project;
}
if ( CUR.GS.dualVector.x == 0x4000 )
CUR.func_dualproj = (TProject_Function)Project_x;
else
{
if ( CUR.GS.dualVector.y == 0x4000 )
CUR.func_dualproj = (TProject_Function)Project_y;
else
CUR.func_dualproj = (TProject_Function)Dual_Project;
}
CUR.func_move = (TMove_Function)Direct_Move;
if ( CUR.F_dot_P == 0x40000000L )
{
if ( CUR.GS.freeVector.x == 0x4000 )
CUR.func_move = (TMove_Function)Direct_Move_X;
else
{
if ( CUR.GS.freeVector.y == 0x4000 )
CUR.func_move = (TMove_Function)Direct_Move_Y;
}
}
if ( ABS( CUR.F_dot_P ) < 0x4000000L )
CUR.F_dot_P = 0x40000000L;
CUR.metrics.ratio = 0;
}
static Bool Normalize( EXEC_OPS TT_F26Dot6 Vx,
TT_F26Dot6 Vy,
TT_UnitVector* R )
{
TT_F26Dot6 W;
Bool S1, S2;
if ( ABS( Vx ) < 0x10000L && ABS( Vy ) < 0x10000L )
{
Vx *= 0x100;
Vy *= 0x100;
W = Norm( Vx, Vy );
if ( W == 0 )
{
return SUCCESS;
}
R->x = (TT_F2Dot14)MulDiv_Round( Vx, 0x4000L, W );
R->y = (TT_F2Dot14)MulDiv_Round( Vy, 0x4000L, W );
return SUCCESS;
}
W = Norm( Vx, Vy );
if ( W <= 0 )
{
CUR.error = TT_Err_Divide_By_Zero;
return FAILURE;
}
Vx = MulDiv_Round( Vx, 0x4000L, W );
Vy = MulDiv_Round( Vy, 0x4000L, W );
W = Vx * Vx + Vy * Vy;
if ( Vx < 0 )
{
Vx = -Vx;
S1 = TRUE;
}
else
S1 = FALSE;
if ( Vy < 0 )
{
Vy = -Vy;
S2 = TRUE;
}
else
S2 = FALSE;
while ( W < 0x10000000L )
{
if ( Vx < Vy )
Vx++;
else
Vy++;
W = Vx * Vx + Vy * Vy;
}
while ( W >= 0x10004000L )
{
if ( Vx < Vy )
Vx--;
else
Vy--;
W = Vx * Vx + Vy * Vy;
}
if ( S1 )
Vx = -Vx;
if ( S2 )
Vy = -Vy;
R->x = (TT_F2Dot14)Vx;
R->y = (TT_F2Dot14)Vy;
return SUCCESS;
}
static void Ins_DUP( INS_ARG )
{ (void)exc;
args[1] = args[0];
}
static void Ins_POP( INS_ARG )
{ (void)exc; (void)args;
}
static void Ins_CLEAR( INS_ARG )
{ (void)args;
CUR.new_top = 0;
}
static void Ins_SWAP( INS_ARG )
{
Long L;
(void)exc;
L = args[0];
args[0] = args[1];
args[1] = L;
}
static void Ins_DEPTH( INS_ARG )
{
args[0] = CUR.top;
}
static void Ins_CINDEX( INS_ARG )
{
Long L;
L = args[0];
if ( L<0 || L > CUR.args )
CUR.error = TT_Err_Invalid_Reference;
else
args[0] = CUR.stack[CUR.args - L];
}
static void Ins_MINDEX( INS_ARG )
{
Long L, K;
L = args[0];
if ( L<0 || L > CUR.args )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
K = CUR.stack[CUR.args - L];
memmove( (&CUR.stack[CUR.args - L ]),
(&CUR.stack[CUR.args - L + 1]),
(L - 1) * sizeof ( Long ) );
CUR.stack[ CUR.args-1 ] = K;
}
static void Ins_ROLL( INS_ARG )
{
Long A, B, C;
(void)exc;
A = args[2];
B = args[1];
C = args[0];
args[2] = C;
args[1] = A;
args[0] = B;
}
static Bool SkipCode( EXEC_OP )
{
CUR.IP += CUR.length;
if ( CUR.IP < CUR.codeSize )
if ( CALC_Length() == SUCCESS )
return SUCCESS;
CUR.error = TT_Err_Code_Overflow;
return FAILURE;
}
static void Ins_IF( INS_ARG )
{
Int nIfs;
Bool Out;
if ( args[0] != 0 )
return;
nIfs = 1;
Out = 0;
do
{
if ( SKIP_Code() == FAILURE )
return;
switch ( CUR.opcode )
{
case 0x58:
nIfs++;
break;
case 0x1b:
Out = (nIfs == 1);
break;
case 0x59:
nIfs--;
Out = (nIfs == 0);
break;
}
} while ( Out == 0 );
}
static void Ins_ELSE( INS_ARG )
{
Int nIfs;
(void)args;
nIfs = 1;
do
{
if ( SKIP_Code() == FAILURE )
return;
switch ( CUR.opcode )
{
case 0x58:
nIfs++;
break;
case 0x59:
nIfs--;
break;
}
} while ( nIfs != 0 );
}
static void Ins_EIF( INS_ARG )
{ (void)exc; (void)args;
}
static void Ins_JROT( INS_ARG )
{
if ( args[1] != 0 )
{
CUR.IP += (Int)(args[0]);
CUR.step_ins = FALSE;
}
}
static void Ins_JMPR( INS_ARG )
{
CUR.IP += (Int)(args[0]);
CUR.step_ins = FALSE;
}
static void Ins_JROF( INS_ARG )
{
if ( args[1] == 0 )
{
CUR.IP += (Int)(args[0]);
CUR.step_ins = FALSE;
}
}
static void Ins_LT( INS_ARG )
{ (void)exc;
if ( args[0] < args[1] )
args[0] = 1;
else
args[0] = 0;
}
static void Ins_LTEQ( INS_ARG )
{ (void)exc;
if ( args[0] <= args[1] )
args[0] = 1;
else
args[0] = 0;
}
static void Ins_GT( INS_ARG )
{ (void)exc;
if ( args[0] > args[1] )
args[0] = 1;
else
args[0] = 0;
}
static void Ins_GTEQ( INS_ARG )
{ (void)exc;
if ( args[0] >= args[1] )
args[0] = 1;
else
args[0] = 0;
}
static void Ins_EQ( INS_ARG )
{ (void)exc;
if ( args[0] == args[1] )
args[0] = 1;
else
args[0] = 0;
}
static void Ins_NEQ( INS_ARG )
{ (void)exc;
if ( args[0] != args[1] )
args[0] = 1;
else
args[0] = 0;
}
static void Ins_ODD( INS_ARG )
{
if ( (CUR_Func_round( args[0], 0L ) & 127) == 64 )
args[0] = 1;
else
args[0] = 0;
}
static void Ins_EVEN( INS_ARG )
{
if ( (CUR_Func_round( args[0], 0L ) & 127) == 0 )
args[0] = 1;
else
args[0] = 0;
}
static void Ins_AND( INS_ARG )
{ (void)exc;
if ( args[0] != 0 && args[1] != 0 )
args[0] = 1;
else
args[0] = 0;
}
static void Ins_OR( INS_ARG )
{ (void)exc;
if ( args[0] != 0 || args[1] != 0 )
args[0] = 1;
else
args[0] = 0;
}
static void Ins_NOT( INS_ARG )
{ (void)exc;
if ( args[0] != 0 )
args[0] = 0;
else
args[0] = 1;
}
static void Ins_ADD( INS_ARG )
{ (void)exc;
args[0] += args[1];
}
static void Ins_SUB( INS_ARG )
{ (void)exc;
args[0] -= args[1];
}
static void Ins_DIV( INS_ARG )
{
if ( args[1] == 0 )
{
CUR.error = TT_Err_Divide_By_Zero;
return;
}
args[0] = MulDiv_Round( args[0], 64L, args[1] );
DBG_PRINT1(" %d", args[0]);
}
static void Ins_MUL( INS_ARG )
{ (void)exc;
args[0] = MulDiv_Round( args[0], args[1], 64L );
}
static void Ins_ABS( INS_ARG )
{ (void)exc;
args[0] = ABS( args[0] );
}
static void Ins_NEG( INS_ARG )
{ (void)exc;
args[0] = -args[0];
}
static void Ins_FLOOR( INS_ARG )
{ (void)exc;
args[0] &= -64;
}
static void Ins_CEILING( INS_ARG )
{ (void)exc;
args[0] = (args[0] + 63) & (-64);
}
static void Ins_MAX( INS_ARG )
{ (void)exc;
if ( args[1] > args[0] )
args[0] = args[1];
}
static void Ins_MIN( INS_ARG )
{ (void)exc;
if ( args[1] < args[0] )
args[0] = args[1];
}
static void Ins_ROUND( INS_ARG )
{
args[0] = CUR_Func_round( args[0],
CUR.metrics.compensations[CUR.opcode - 0x68] );
}
static void Ins_NROUND( INS_ARG )
{
args[0] = Round_None( EXEC_ARGS
args[0],
CUR.metrics.compensations[CUR.opcode - 0x6C] );
}
static void skip_FDEF( EXEC_OP )
{
while ( SKIP_Code() == SUCCESS )
{
switch ( CUR.opcode )
{
case 0x89:
case 0x2c:
CUR.error = TT_Err_Nested_DEFS;
return;
case 0x2d:
return;
}
}
}
static void Ins_FDEF( INS_ARG )
{
PDefRecord pRec;
if ( BOUNDS( args[0], CUR.numFDefs ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
pRec = &CUR.FDefs[args[0]];
pRec->Range = CUR.curRange;
pRec->Opc = (Byte)(args[0]);
pRec->Start = CUR.IP + 1;
pRec->Active = TRUE;
skip_FDEF(EXEC_ARG);
}
static void Ins_ENDF( INS_ARG )
{
PCallRecord pRec;
(void)args;
if ( CUR.callTop <= 0 )
{
CUR.error = TT_Err_ENDF_In_Exec_Stream;
return;
}
CUR.callTop--;
pRec = &CUR.callStack[CUR.callTop];
pRec->Cur_Count--;
CUR.step_ins = FALSE;
if ( pRec->Cur_Count > 0 )
{
CUR.callTop++;
CUR.IP = pRec->Cur_Restart;
}
else
INS_Goto_CodeRange( pRec->Caller_Range,
pRec->Caller_IP );
}
static void Ins_CALL( INS_ARG )
{
PCallRecord pCrec;
if ( BOUNDS( args[0], CUR.numFDefs ) || !CUR.FDefs[args[0]].Active )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
if ( CUR.callTop >= CUR.callSize )
{
CUR.error = TT_Err_Stack_Overflow;
return;
}
DBG_PRINT1("%d", args[0]);
pCrec = &CUR.callStack[CUR.callTop];
pCrec->Caller_Range = CUR.curRange;
pCrec->Caller_IP = CUR.IP + 1;
pCrec->Cur_Count = 1;
pCrec->Cur_Restart = CUR.FDefs[args[0]].Start;
CUR.callTop++;
INS_Goto_CodeRange( CUR.FDefs[args[0]].Range,
CUR.FDefs[args[0]].Start );
CUR.step_ins = FALSE;
}
static void Ins_LOOPCALL( INS_ARG )
{
PCallRecord pTCR;
if ( BOUNDS( args[1], CUR.numFDefs ) || !CUR.FDefs[args[1]].Active )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
if ( CUR.callTop >= CUR.callSize )
{
CUR.error = TT_Err_Stack_Overflow;
return;
}
if ( args[0] > 0 )
{
pTCR = &CUR.callStack[CUR.callTop];
pTCR->Caller_Range = CUR.curRange;
pTCR->Caller_IP = CUR.IP + 1;
pTCR->Cur_Count = (Int)(args[0]);
pTCR->Cur_Restart = CUR.FDefs[args[1]].Start;
CUR.callTop++;
INS_Goto_CodeRange( CUR.FDefs[args[1]].Range,
CUR.FDefs[args[1]].Start );
CUR.step_ins = FALSE;
}
}
static void Ins_IDEF( INS_ARG )
{
if (CUR.countIDefs >= CUR.numIDefs || args[0] > 255)
CUR.error = TT_Err_Storage_Overflow;
else
{
PDefRecord pTDR;
CUR.IDefPtr[(Byte)(args[0])] = CUR.countIDefs;
pTDR = &CUR.IDefs[CUR.countIDefs++];
pTDR->Opc = (Byte)(args[0]);
pTDR->Start = CUR.IP + 1;
pTDR->Range = CUR.curRange;
pTDR->Active = TRUE;
skip_FDEF(EXEC_ARG);
}
}
static void Ins_NPUSHB( INS_ARG )
{
Int L, K;
L = (Int)CUR.code[CUR.IP + 1];
if ( BOUNDS( L, CUR.stackSize+1-CUR.top ) )
{
CUR.error = TT_Err_Stack_Overflow;
return;
}
for ( K = 1; K <= L; K++ )
{ args[K - 1] = CUR.code[CUR.IP + K + 1];
DBG_PRINT1(" %d", args[K - 1]);
}
CUR.new_top += L;
}
static void Ins_NPUSHW( INS_ARG )
{
Int L, K;
L = (Int)CUR.code[CUR.IP + 1];
if ( BOUNDS( L, CUR.stackSize+1-CUR.top ) )
{
CUR.error = TT_Err_Stack_Overflow;
return;
}
CUR.IP += 2;
for ( K = 0; K < L; K++ )
{ args[K] = GET_ShortIns();
DBG_PRINT1(" %d", args[K]);
}
CUR.step_ins = FALSE;
CUR.new_top += L;
}
static void Ins_PUSHB( INS_ARG )
{
Int L, K;
L = ((Int)CUR.opcode - 0xB0 + 1);
if ( BOUNDS( L, CUR.stackSize+1-CUR.top ) )
{
CUR.error = TT_Err_Stack_Overflow;
return;
}
for ( K = 1; K <= L; K++ )
{ args[K - 1] = CUR.code[CUR.IP + K];
DBG_PRINT1(" %d", args[K - 1]);
}
}
static void Ins_PUSHW( INS_ARG )
{
Int L, K;
L = CUR.opcode - 0xB8 + 1;
if ( BOUNDS( L, CUR.stackSize+1-CUR.top ) )
{
CUR.error = TT_Err_Stack_Overflow;
return;
}
CUR.IP++;
for ( K = 0; K < L; K++ )
{ args[K] = GET_ShortIns();
DBG_PRINT1(" %d", args[K]);
}
CUR.step_ins = FALSE;
}
static void Ins_RS( INS_ARG )
{
if ( BOUNDS( args[0], CUR.storeSize ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
args[0] = CUR.storage[args[0]];
}
static void Ins_WS( INS_ARG )
{
if ( BOUNDS( args[0], CUR.storeSize ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
CUR.storage[args[0]] = args[1];
}
static void Ins_WCVTP( INS_ARG )
{
if ( BOUNDS( args[0], CUR.cvtSize ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
CUR_Func_write_cvt( args[0], args[1] );
}
static void Ins_WCVTF( INS_ARG )
{
int ov;
if ( BOUNDS( args[0], CUR.cvtSize ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
ov = CUR.cvt[args[0]];
CUR.cvt[args[0]] = FUnits_To_Pixels( EXEC_ARGS args[1] );
DBG_PRINT3(" cvt[%d]%d:=%d", args[0], ov, CUR.cvt[args[0]]);
}
static void Ins_RCVT( INS_ARG )
{
int index;
if ( BOUNDS( args[0], CUR.cvtSize ) )
{
#if 0
CUR.error = TT_Err_Invalid_Reference;
return;
#else
index=args[0];
args[0] = 0;
DBG_PRINT1(" cvt[%d] stubbed with 0", index);
#endif
}
index=args[0];
args[0] = CUR_Func_read_cvt( index );
DBG_PRINT3(" cvt[%d]%d:%d", index, CUR.cvt[index], args[0]);
}
static void Ins_SVTCA( INS_ARG )
{
Short A, B;
(void)args;
if ( CUR.opcode & 1 )
A = 0x4000;
else
A = 0;
B = A ^ 0x4000;
CUR.GS.freeVector.x = A;
CUR.GS.projVector.x = A;
CUR.GS.dualVector.x = A;
CUR.GS.freeVector.y = B;
CUR.GS.projVector.y = B;
CUR.GS.dualVector.y = B;
COMPUTE_Funcs();
}
static void Ins_SPVTCA( INS_ARG )
{
Short A, B;
(void)args;
if ( CUR.opcode & 1 )
A = 0x4000;
else
A = 0;
B = A ^ 0x4000;
CUR.GS.projVector.x = A;
CUR.GS.dualVector.x = A;
CUR.GS.projVector.y = B;
CUR.GS.dualVector.y = B;
COMPUTE_Funcs();
}
static void Ins_SFVTCA( INS_ARG )
{
Short A, B;
(void)args;
if ( CUR.opcode & 1 )
A = 0x4000;
else
A = 0;
B = A ^ 0x4000;
CUR.GS.freeVector.x = A;
CUR.GS.freeVector.y = B;
COMPUTE_Funcs();
}
static Bool Ins_SxVTL( EXEC_OPS Int aIdx1,
Int aIdx2,
Int aOpc,
TT_UnitVector* Vec )
{
Long A, B, C;
if ( BOUNDS( aIdx1, CUR.zp2.n_points ) ||
BOUNDS( aIdx2, CUR.zp1.n_points ) )
{
CUR.error = TT_Err_Invalid_Reference;
return FAILURE;
}
A = CUR.zp1.cur_x[aIdx2] - CUR.zp2.cur_x[aIdx1];
B = CUR.zp1.cur_y[aIdx2] - CUR.zp2.cur_y[aIdx1];
if ( (aOpc & 1) != 0 )
{
C = B;
B = A;
A = -C;
}
if ( NORMalize( A, B, Vec ) == FAILURE )
{
CUR.error = TT_Err_Ok;
Vec->x = 0x4000;
Vec->y = 0;
}
return SUCCESS;
}
static void Ins_SPVTL( INS_ARG )
{
if ( INS_SxVTL( args[1],
args[0],
CUR.opcode,
&CUR.GS.projVector) == FAILURE )
return;
CUR.GS.dualVector = CUR.GS.projVector;
COMPUTE_Funcs();
}
static void Ins_SFVTL( INS_ARG )
{
if ( INS_SxVTL( (Int)(args[1]),
(Int)(args[0]),
CUR.opcode,
&CUR.GS.freeVector) == FAILURE )
return;
COMPUTE_Funcs();
}
static void Ins_SFVTPV( INS_ARG )
{ (void)args;
CUR.GS.freeVector = CUR.GS.projVector;
COMPUTE_Funcs();
}
static void Ins_SDPVTL( INS_ARG )
{
Long A, B, C;
Long p1, p2;
p1 = args[1];
p2 = args[0];
if ( BOUNDS( p2, CUR.zp1.n_points ) ||
BOUNDS( p1, CUR.zp2.n_points ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
A = CUR.zp1.org_x[p2] - CUR.zp2.org_x[p1];
B = CUR.zp1.org_y[p2] - CUR.zp2.org_y[p1];
if ( (CUR.opcode & 1) != 0 )
{
C = B;
B = A;
A = -C;
}
if ( NORMalize( A, B, &CUR.GS.dualVector ) == FAILURE )
return;
A = CUR.zp1.cur_x[p2] - CUR.zp2.cur_x[p1];
B = CUR.zp1.cur_y[p2] - CUR.zp2.cur_y[p1];
if ( (CUR.opcode & 1) != 0 )
{
C = B;
B = A;
A = -C;
}
if ( NORMalize( A, B, &CUR.GS.projVector ) == FAILURE )
return;
COMPUTE_Funcs();
}
static void Ins_SPVFS( INS_ARG )
{
Short S;
Long X, Y;
S = (Short)args[1];
Y = (Long)S;
S = (Short)args[0];
X = (Long)S;
if ( NORMalize( X, Y, &CUR.GS.projVector ) == FAILURE )
return;
CUR.GS.dualVector = CUR.GS.projVector;
COMPUTE_Funcs();
}
static void Ins_SFVFS( INS_ARG )
{
Short S;
Long X, Y;
S = (Short)args[1];
Y = (Long)S;
S = (Short)args[0];
X = S;
if ( NORMalize( X, Y, &CUR.GS.freeVector ) == FAILURE )
return;
COMPUTE_Funcs();
}
static void Ins_GPV( INS_ARG )
{
args[0] = CUR.GS.projVector.x;
args[1] = CUR.GS.projVector.y;
}
static void Ins_GFV( INS_ARG )
{
args[0] = CUR.GS.freeVector.x;
args[1] = CUR.GS.freeVector.y;
}
static void Ins_SRP0( INS_ARG )
{
CUR.GS.rp0 = (Int)(args[0]);
}
static void Ins_SRP1( INS_ARG )
{
CUR.GS.rp1 = (Int)(args[0]);
}
static void Ins_SRP2( INS_ARG )
{
CUR.GS.rp2 = (Int)(args[0]);
}
static void Ins_SZP0( INS_ARG )
{
switch ( args[0] )
{
case 0:
CUR.zp0 = CUR.twilight;
break;
case 1:
CUR.zp0 = CUR.pts;
break;
default:
CUR.error = TT_Err_Invalid_Reference;
return;
break;
}
CUR.GS.gep0 = (Int)(args[0]);
}
static void Ins_SZP1( INS_ARG )
{
switch ( args[0] )
{
case 0:
CUR.zp1 = CUR.twilight;
break;
case 1:
CUR.zp1 = CUR.pts;
break;
default:
CUR.error = TT_Err_Invalid_Reference;
return;
}
CUR.GS.gep1 = (Int)(args[0]);
}
static void Ins_SZP2( INS_ARG )
{
switch ( args[0] )
{
case 0:
CUR.zp2 = CUR.twilight;
break;
case 1:
CUR.zp2 = CUR.pts;
break;
default:
CUR.error = TT_Err_Invalid_Reference;
return;
}
CUR.GS.gep2 = (Int)(args[0]);
}
static void Ins_SZPS( INS_ARG )
{
switch ( args[0] )
{
case 0:
CUR.zp0 = CUR.twilight;
break;
case 1:
CUR.zp0 = CUR.pts;
break;
default:
CUR.error = TT_Err_Invalid_Reference;
return;
}
CUR.zp1 = CUR.zp0;
CUR.zp2 = CUR.zp0;
CUR.GS.gep0 = (Int)(args[0]);
CUR.GS.gep1 = (Int)(args[0]);
CUR.GS.gep2 = (Int)(args[0]);
}
static void Ins_RTHG( INS_ARG )
{ (void)args;
CUR.GS.round_state = TT_Round_To_Half_Grid;
CUR.func_round = (TRound_Function)Round_To_Half_Grid;
}
static void Ins_RTG( INS_ARG )
{ (void)args;
CUR.GS.round_state = TT_Round_To_Grid;
CUR.func_round = (TRound_Function)Round_To_Grid;
}
static void Ins_RTDG( INS_ARG )
{ (void)args;
CUR.GS.round_state = TT_Round_To_Double_Grid;
CUR.func_round = (TRound_Function)Round_To_Double_Grid;
}
static void Ins_RUTG( INS_ARG )
{ (void)args;
CUR.GS.round_state = TT_Round_Up_To_Grid;
CUR.func_round = (TRound_Function)Round_Up_To_Grid;
}
static void Ins_RDTG( INS_ARG )
{ (void)args;
CUR.GS.round_state = TT_Round_Down_To_Grid;
CUR.func_round = (TRound_Function)Round_Down_To_Grid;
}
static void Ins_ROFF( INS_ARG )
{ (void)args;
CUR.GS.round_state = TT_Round_Off;
CUR.func_round = (TRound_Function)Round_None;
}
static void Ins_SROUND( INS_ARG )
{
SET_SuperRound( 0x4000L, args[0] );
CUR.GS.round_state = TT_Round_Super;
CUR.func_round = (TRound_Function)Round_Super;
}
static void Ins_S45ROUND( INS_ARG )
{
SET_SuperRound( 0x2D41L, args[0] );
CUR.GS.round_state = TT_Round_Super_45;
CUR.func_round = (TRound_Function)Round_Super_45;
}
static void Ins_SLOOP( INS_ARG )
{
CUR.GS.loop = args[0];
}
static void Ins_SMD( INS_ARG )
{
CUR.GS.minimum_distance = args[0];
}
static void Ins_INSTCTRL( INS_ARG )
{
Long K, L;
K = args[1];
L = args[0];
if ( K < 0 || K > 3 )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
CUR.GS.instruct_control = (Int)((CUR.GS.instruct_control & (~K)) | (L & K));
}
static void Ins_SCANCTRL( INS_ARG )
{
Int A;
A = (Int)(args[0] & 0xFF);
if ( A == 0xFF )
{
CUR.GS.scan_control = TRUE;
return;
}
else if ( A == 0 )
{
CUR.GS.scan_control = FALSE;
return;
}
A *= 64;
if ( (args[0] & 0x100) != 0 && CUR.metrics.pointSize <= A )
CUR.GS.scan_control = TRUE;
if ( (args[0] & 0x200) != 0 && CUR.metrics.rotated )
CUR.GS.scan_control = TRUE;
if ( (args[0] & 0x400) != 0 && CUR.metrics.stretched )
CUR.GS.scan_control = TRUE;
if ( (args[0] & 0x800) != 0 && CUR.metrics.pointSize > A )
CUR.GS.scan_control = FALSE;
if ( (args[0] & 0x1000) != 0 && CUR.metrics.rotated )
CUR.GS.scan_control = FALSE;
if ( (args[0] & 0x2000) != 0 && CUR.metrics.stretched )
CUR.GS.scan_control = FALSE;
}
static void Ins_SCANTYPE( INS_ARG )
{
if ( args[0] >= 0 && args[0] <= 5 )
{
if ( args[0] == 3 )
args[0] = 2;
CUR.GS.scan_type = (Int)args[0];
}
}
static void Ins_SCVTCI( INS_ARG )
{
CUR.GS.control_value_cutin = (TT_F26Dot6)args[0];
}
static void Ins_SSWCI( INS_ARG )
{
CUR.GS.single_width_cutin = (TT_F26Dot6)args[0];
}
static void Ins_SSW( INS_ARG )
{
CUR.GS.single_width_value = (TT_F26Dot6)(args[0] >> 10);
}
static void Ins_FLIPON( INS_ARG )
{ (void)args;
CUR.GS.auto_flip = TRUE;
}
static void Ins_FLIPOFF( INS_ARG )
{ (void)args;
CUR.GS.auto_flip = FALSE;
}
static void Ins_SANGW( INS_ARG )
{ (void)exc; (void)args;
}
static void Ins_SDB( INS_ARG )
{
CUR.GS.delta_base = (Int)args[0];
}
static void Ins_SDS( INS_ARG )
{
CUR.GS.delta_shift = (Int)args[0];
}
static void Ins_GC( INS_ARG )
{
Long L;
L = args[0];
if ( BOUNDS( L, CUR.zp2.n_points ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
switch ( CUR.opcode & 1 )
{
case 0:
L = CUR_Func_project( CUR.zp2.cur_x[L],
CUR.zp2.cur_y[L] );
break;
case 1:
L = CUR_Func_dualproj( CUR.zp2.org_x[L],
CUR.zp2.org_y[L] );
break;
}
args[0] = L;
}
static void Ins_SCFS( INS_ARG )
{
Long K;
Int L;
L = (Int)args[0];
if ( BOUNDS( args[0], CUR.zp2.n_points ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
K = CUR_Func_project( CUR.zp2.cur_x[L],
CUR.zp2.cur_y[L] );
CUR_Func_move( &CUR.zp2, L, args[1] - K );
if ( CUR.GS.gep2 == 0 )
{
CUR.zp2.org_x[L] = CUR.zp2.cur_x[L];
CUR.zp2.org_y[L] = CUR.zp2.cur_y[L];
}
}
static void Ins_MD( INS_ARG )
{
Long K, L;
TT_F26Dot6 D;
K = args[1];
L = args[0];
if( BOUNDS( args[0], CUR.zp2.n_points ) ||
BOUNDS( args[1], CUR.zp1.n_points ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
if ( CUR.opcode & 1 )
D = CUR_Func_project( CUR.zp2.cur_x[L] - CUR.zp1.cur_x[K],
CUR.zp2.cur_y[L] - CUR.zp1.cur_y[K] );
else
D = CUR_Func_dualproj( CUR.zp2.org_x[L] - CUR.zp1.org_x[K],
CUR.zp2.org_y[L] - CUR.zp1.org_y[K] );
args[0] = D;
}
static void Ins_MPPEM( INS_ARG )
{
args[0] = CURRENT_Ppem();
DBG_PRINT1(" %d", args[0]);
}
static void Ins_MPS( INS_ARG )
{
args[0] = CUR.metrics.pointSize;
}
static void Ins_FLIPPT( INS_ARG )
{
Long point;
(void)args;
if ( CUR.top < CUR.GS.loop )
{
CUR.error = TT_Err_Too_Few_Arguments;
return;
}
while ( CUR.GS.loop > 0 )
{
CUR.args--;
point = CUR.stack[CUR.args];
if ( BOUNDS( point, CUR.pts.n_points ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
CUR.pts.touch[point] ^= TT_Flag_On_Curve;
CUR.GS.loop--;
}
CUR.GS.loop = 1;
CUR.new_top = CUR.args;
}
static void Ins_FLIPRGON( INS_ARG )
{
Long I, K, L;
K = args[1];
L = args[0];
if ( BOUNDS( K, CUR.pts.n_points ) ||
BOUNDS( L, CUR.pts.n_points ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
for ( I = L; I <= K; I++ )
CUR.pts.touch[I] |= TT_Flag_On_Curve;
}
static void Ins_FLIPRGOFF( INS_ARG )
{
Long I, K, L;
K = args[1];
L = args[0];
if ( BOUNDS( K, CUR.pts.n_points ) ||
BOUNDS( L, CUR.pts.n_points ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
for ( I = L; I <= K; I++ )
CUR.pts.touch[I] &= ~TT_Flag_On_Curve;
}
static Bool Compute_Point_Displacement( EXEC_OPS
PCoordinates x,
PCoordinates y,
PGlyph_Zone zone,
Int* refp )
{
TGlyph_Zone zp;
Int p;
TT_F26Dot6 d;
if ( CUR.opcode & 1 )
{
zp = CUR.zp0;
p = CUR.GS.rp1;
}
else
{
zp = CUR.zp1;
p = CUR.GS.rp2;
}
if ( BOUNDS( p, zp.n_points ) )
{
CUR.error = TT_Err_Invalid_Displacement;
return FAILURE;
}
*zone = zp;
*refp = p;
d = CUR_Func_project( zp.cur_x[p] - zp.org_x[p],
zp.cur_y[p] - zp.org_y[p] );
*x = MulDiv_Round(d, (Long)CUR.GS.freeVector.x * 0x10000L, CUR.F_dot_P );
*y = MulDiv_Round(d, (Long)CUR.GS.freeVector.y * 0x10000L, CUR.F_dot_P );
return SUCCESS;
}
static void Move_Zp2_Point( EXEC_OPS
Long point,
TT_F26Dot6 dx,
TT_F26Dot6 dy,
Bool touch )
{
if ( CUR.GS.freeVector.x != 0 )
{
CUR.zp2.cur_x[point] += dx;
if ( touch )
CUR.zp2.touch[point] |= TT_Flag_Touched_X;
}
if ( CUR.GS.freeVector.y != 0 )
{
CUR.zp2.cur_y[point] += dy;
if ( touch )
CUR.zp2.touch[point] |= TT_Flag_Touched_Y;
}
}
static void Ins_SHP( INS_ARG )
{
TGlyph_Zone zp;
Int refp;
TT_F26Dot6 dx,
dy;
Long point;
(void)args;
if ( CUR.top < CUR.GS.loop )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
if ( COMPUTE_Point_Displacement( &dx, &dy, &zp, &refp ) )
return;
while ( CUR.GS.loop > 0 )
{
CUR.args--;
point = CUR.stack[CUR.args];
if ( BOUNDS( point, CUR.zp2.n_points ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
MOVE_Zp2_Point( point, dx, dy, TRUE );
CUR.GS.loop--;
}
CUR.GS.loop = 1;
CUR.new_top = CUR.args;
}
static void Ins_SHC( INS_ARG )
{
TGlyph_Zone zp;
Int refp;
TT_F26Dot6 dx,
dy;
Long contour, i;
Int first_point, last_point;
contour = args[0];
if ( BOUNDS( args[0], CUR.pts.n_contours ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
if ( COMPUTE_Point_Displacement( &dx, &dy, &zp, &refp ) )
return;
if ( contour == 0 )
first_point = 0;
else
first_point = CUR.pts.contours[contour-1] + 1;
last_point = CUR.pts.contours[contour];
for ( i = first_point; i <= last_point; i++ )
{
if ( zp.cur_x != CUR.zp2.cur_x || refp != i )
MOVE_Zp2_Point( i, dx, dy, FALSE );
}
}
static void Ins_SHZ( INS_ARG )
{
TGlyph_Zone zp;
Int refp;
TT_F26Dot6 dx,
dy;
Int last_point;
Long i;
if ( BOUNDS( args[0], 2 ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
if ( COMPUTE_Point_Displacement( &dx, &dy, &zp, &refp ) )
return;
last_point = zp.n_points - 1;
for ( i = 0; i <= last_point; i++ )
{
if ( zp.cur_x != CUR.zp2.cur_x || refp != i )
MOVE_Zp2_Point( i, dx, dy, FALSE );
}
}
static void Ins_SHPIX( INS_ARG )
{
TT_F26Dot6 dx, dy;
Long point;
if ( CUR.top < CUR.GS.loop )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
dx = MulDiv_Round( args[0],
(Long)CUR.GS.freeVector.x,
0x4000 );
dy = MulDiv_Round( args[0],
(Long)CUR.GS.freeVector.y,
0x4000 );
while ( CUR.GS.loop > 0 )
{
CUR.args--;
point = CUR.stack[CUR.args];
if ( BOUNDS( point, CUR.zp2.n_points ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
MOVE_Zp2_Point( point, dx, dy, TRUE );
CUR.GS.loop--;
}
CUR.GS.loop = 1;
CUR.new_top = CUR.args;
}
static void Ins_MSIRP( INS_ARG )
{
Int point;
TT_F26Dot6 distance;
point = (Int)args[0];
if ( BOUNDS( args[0], CUR.zp1.n_points ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
if ( CUR.GS.gep0 == 0 )
{
CUR.zp1.org_x[point] = CUR.zp0.org_x[CUR.GS.rp0];
CUR.zp1.org_y[point] = CUR.zp0.org_y[CUR.GS.rp0];
CUR.zp1.cur_x[point] = CUR.zp1.org_x[point];
CUR.zp1.cur_y[point] = CUR.zp1.org_y[point];
}
distance = CUR_Func_project( CUR.zp1.cur_x[point] -
CUR.zp0.cur_x[CUR.GS.rp0],
CUR.zp1.cur_y[point] -
CUR.zp0.cur_y[CUR.GS.rp0] );
CUR_Func_move( &CUR.zp1, point, args[1] - distance );
CUR.GS.rp1 = CUR.GS.rp0;
CUR.GS.rp2 = point;
if ( (CUR.opcode & 1) != 0 )
CUR.GS.rp0 = point;
}
static void Ins_MDAP( INS_ARG )
{
Int point;
TT_F26Dot6 cur_dist,
distance;
point = (Int)args[0];
if ( BOUNDS( args[0], CUR.zp0.n_points ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
if ( (CUR.opcode & 1) != 0 )
{
cur_dist = CUR_Func_project( CUR.zp0.cur_x[point],
CUR.zp0.cur_y[point] );
distance = CUR_Func_round( cur_dist,
CUR.metrics.compensations[0] ) - cur_dist;
}
else
distance = 0;
CUR_Func_move( &CUR.zp0, point, distance );
CUR.GS.rp0 = point;
CUR.GS.rp1 = point;
}
static void Ins_MIAP( INS_ARG )
{
Int cvtEntry, point;
TT_F26Dot6 distance,
org_dist;
cvtEntry = (Int)args[1];
point = (Int)args[0];
if ( BOUNDS( args[0], CUR.zp0.n_points ) ||
BOUNDS( args[1], CUR.cvtSize ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
distance = CUR_Func_read_cvt( cvtEntry );
DBG_PRINT3(" cvtEntry=%d point=%d distance=%d", cvtEntry, point, distance);
if ( CUR.GS.gep0 == 0 )
{
CUR.zp0.org_x[point] = MulDiv_Round( CUR.GS.freeVector.x,
distance, 0x4000L );
CUR.zp0.cur_x[point] = CUR.zp0.org_x[point];
CUR.zp0.org_y[point] = MulDiv_Round( CUR.GS.freeVector.y,
distance, 0x4000L );
CUR.zp0.cur_y[point] = CUR.zp0.org_y[point];
}
org_dist = CUR_Func_project( CUR.zp0.cur_x[point],
CUR.zp0.cur_y[point] );
if ( (CUR.opcode & 1) != 0 )
{
if ( ABS(distance - org_dist) > CUR.GS.control_value_cutin )
distance = org_dist;
distance = CUR_Func_round( distance, CUR.metrics.compensations[0] );
}
CUR_Func_move( &CUR.zp0, point, distance - org_dist );
CUR.GS.rp0 = point;
CUR.GS.rp1 = point;
}
static void Ins_MDRP( INS_ARG )
{
Int point;
TT_F26Dot6 distance,
org_dist;
point = (Int)args[0];
if ( BOUNDS( args[0], CUR.zp1.n_points ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
org_dist = CUR_Func_dualproj( CUR.zp1.org_x[point] -
CUR.zp0.org_x[CUR.GS.rp0],
CUR.zp1.org_y[point] -
CUR.zp0.org_y[CUR.GS.rp0] );
if ( ABS(org_dist) < CUR.GS.single_width_cutin )
{
if ( org_dist >= 0 )
org_dist = CUR.GS.single_width_value;
else
org_dist = -CUR.GS.single_width_value;
}
if ( (CUR.opcode & 4) != 0 )
distance = CUR_Func_round( org_dist,
CUR.metrics.compensations[CUR.opcode & 3] );
else
distance = Round_None( EXEC_ARGS
org_dist,
CUR.metrics.compensations[CUR.opcode & 3] );
if ( (CUR.opcode & 8) != 0 )
{
if ( org_dist >= 0 )
{
if ( distance < CUR.GS.minimum_distance )
distance = CUR.GS.minimum_distance;
}
else
{
if ( distance > -CUR.GS.minimum_distance )
distance = -CUR.GS.minimum_distance;
}
}
org_dist = CUR_Func_project( CUR.zp1.cur_x[point] -
CUR.zp0.cur_x[CUR.GS.rp0],
CUR.zp1.cur_y[point] -
CUR.zp0.cur_y[CUR.GS.rp0] );
CUR_Func_move( &CUR.zp1, point, distance - org_dist );
CUR.GS.rp1 = CUR.GS.rp0;
CUR.GS.rp2 = point;
if ( (CUR.opcode & 16) != 0 )
CUR.GS.rp0 = point;
}
static void Ins_MIRP( INS_ARG )
{
Int point,
cvtEntry;
TT_F26Dot6 cvt_dist,
distance,
cur_dist,
org_dist;
point = (Int)args[0];
cvtEntry = (Int)args[1];
if ( BOUNDS( args[0], CUR.zp1.n_points ) ||
BOUNDS( args[1]+1, CUR.cvtSize+1 ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
if ( args[1] < 0 )
cvt_dist = 0;
else
cvt_dist = CUR_Func_read_cvt( cvtEntry );
if ( ABS( cvt_dist ) < CUR.GS.single_width_cutin )
{
if ( cvt_dist >= 0 )
cvt_dist = CUR.GS.single_width_value;
else
cvt_dist = -CUR.GS.single_width_value;
}
if ( CUR.GS.gep1 == 0 )
{
CUR.zp1.org_x[point] = CUR.zp0.org_x[CUR.GS.rp0] +
MulDiv_Round( cvt_dist,
CUR.GS.freeVector.x,
0x4000 );
CUR.zp1.org_y[point] = CUR.zp0.org_y[CUR.GS.rp0] +
MulDiv_Round( cvt_dist,
CUR.GS.freeVector.y,
0x4000 );
CUR.zp1.cur_x[point] = CUR.zp1.org_x[point];
CUR.zp1.cur_y[point] = CUR.zp1.org_y[point];
}
org_dist = CUR_Func_dualproj( CUR.zp1.org_x[point] -
CUR.zp0.org_x[CUR.GS.rp0],
CUR.zp1.org_y[point] -
CUR.zp0.org_y[CUR.GS.rp0] );
cur_dist = CUR_Func_project( CUR.zp1.cur_x[point] -
CUR.zp0.cur_x[CUR.GS.rp0],
CUR.zp1.cur_y[point] -
CUR.zp0.cur_y[CUR.GS.rp0] );
if ( CUR.GS.auto_flip )
{
if ( (org_dist ^ cvt_dist) < 0 )
cvt_dist = -cvt_dist;
}
if ( (CUR.opcode & 4) != 0 )
{
if ( CUR.GS.gep0 == CUR.GS.gep1 )
if ( ABS( cvt_dist - org_dist ) >= CUR.GS.control_value_cutin )
cvt_dist = org_dist;
distance = CUR_Func_round( cvt_dist,
CUR.metrics.compensations[CUR.opcode & 3] );
}
else
distance = Round_None( EXEC_ARGS
cvt_dist,
CUR.metrics.compensations[CUR.opcode & 3] );
if ( (CUR.opcode & 8) != 0 )
{
if ( org_dist >= 0 )
{
if ( distance < CUR.GS.minimum_distance )
distance = CUR.GS.minimum_distance;
}
else
{
if ( distance > -CUR.GS.minimum_distance )
distance = -CUR.GS.minimum_distance;
}
}
CUR_Func_move( &CUR.zp1, point, distance - cur_dist );
CUR.GS.rp1 = CUR.GS.rp0;
if ( (CUR.opcode & 16) != 0 )
CUR.GS.rp0 = point;
CUR.GS.rp2 = point;
}
static void Ins_ALIGNRP( INS_ARG )
{
Int point;
TT_F26Dot6 distance;
(void)args;
if ( CUR.top < CUR.GS.loop )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
while ( CUR.GS.loop > 0 )
{
CUR.args--;
point = (Int)CUR.stack[CUR.args];
if ( BOUNDS( point, CUR.zp1.n_points ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
distance = CUR_Func_project( CUR.zp1.cur_x[point] -
CUR.zp0.cur_x[CUR.GS.rp0],
CUR.zp1.cur_y[point] -
CUR.zp0.cur_y[CUR.GS.rp0] );
CUR_Func_move( &CUR.zp1, point, -distance );
CUR.GS.loop--;
}
CUR.GS.loop = 1;
CUR.new_top = CUR.args;
}
static void Ins_AA( INS_ARG )
{ (void)exc; (void)args;
}
static void Ins_ISECT( INS_ARG )
{
Long point,
a0, a1,
b0, b1;
TT_F26Dot6 discriminant;
TT_F26Dot6 dx, dy,
dax, day,
dbx, dby;
TT_F26Dot6 val;
TT_Vector R;
point = args[0];
a0 = args[1];
a1 = args[2];
b0 = args[3];
b1 = args[4];
if ( BOUNDS( b0, CUR.zp0.n_points ) ||
BOUNDS( b1, CUR.zp0.n_points ) ||
BOUNDS( a0, CUR.zp1.n_points ) ||
BOUNDS( a1, CUR.zp1.n_points ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
dbx = CUR.zp0.cur_x[b1] - CUR.zp0.cur_x[b0];
dby = CUR.zp0.cur_y[b1] - CUR.zp0.cur_y[b0];
dax = CUR.zp1.cur_x[a1] - CUR.zp1.cur_x[a0];
day = CUR.zp1.cur_y[a1] - CUR.zp1.cur_y[a0];
dx = CUR.zp0.cur_x[b0] - CUR.zp1.cur_x[a0];
dy = CUR.zp0.cur_y[b0] - CUR.zp1.cur_y[a0];
CUR.zp2.touch[point] |= TT_Flag_Touched_Both;
discriminant = MulDiv_Round( dax, -dby, 0x40L ) +
MulDiv_Round( day, dbx, 0x40L );
if ( ABS( discriminant ) >= 0x40 )
{
val = MulDiv_Round( dx, -dby, 0x40L ) + MulDiv_Round( dy, dbx, 0x40L );
R.x = MulDiv_Round( val, dax, discriminant );
R.y = MulDiv_Round( val, day, discriminant );
CUR.zp2.cur_x[point] = CUR.zp1.cur_x[a0] + R.x;
CUR.zp2.cur_y[point] = CUR.zp1.cur_y[a0] + R.y;
}
else
{
CUR.zp2.cur_x[point] = ( CUR.zp1.cur_x[a0] +
CUR.zp1.cur_x[a1] +
CUR.zp0.cur_x[b0] +
CUR.zp1.cur_x[b1] ) / 4;
CUR.zp2.cur_y[point] = ( CUR.zp1.cur_y[a0] +
CUR.zp1.cur_y[a1] +
CUR.zp0.cur_y[b0] +
CUR.zp1.cur_y[b1] ) / 4;
}
}
static void Ins_ALIGNPTS( INS_ARG )
{
Int p1, p2;
TT_F26Dot6 distance;
p1 = (Int)args[0];
p2 = (Int)args[1];
if ( BOUNDS( args[0], CUR.zp1.n_points ) ||
BOUNDS( args[1], CUR.zp0.n_points ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
distance = CUR_Func_project( CUR.zp0.cur_x[p2] -
CUR.zp1.cur_x[p1],
CUR.zp0.cur_y[p2] -
CUR.zp1.cur_x[p1] ) / 2;
CUR_Func_move( &CUR.zp1, p1, distance );
CUR_Func_move( &CUR.zp0, p2, -distance );
}
static void Ins_IP( INS_ARG )
{
TT_F26Dot6 org_a, org_b, org_x,
cur_a, cur_b, cur_x,
distance;
Int point;
(void)args;
if ( CUR.top < CUR.GS.loop )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
org_a = CUR_Func_dualproj( CUR.zp0.org_x[CUR.GS.rp1],
CUR.zp0.org_y[CUR.GS.rp1] );
org_b = CUR_Func_dualproj( CUR.zp1.org_x[CUR.GS.rp2],
CUR.zp1.org_y[CUR.GS.rp2] );
cur_a = CUR_Func_project( CUR.zp0.cur_x[CUR.GS.rp1],
CUR.zp0.cur_y[CUR.GS.rp1] );
cur_b = CUR_Func_project( CUR.zp1.cur_x[CUR.GS.rp2],
CUR.zp1.cur_y[CUR.GS.rp2] );
while ( CUR.GS.loop > 0 )
{
CUR.args--;
point = (Int)CUR.stack[CUR.args];
if ( BOUNDS( point, CUR.zp2.n_points ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
org_x = CUR_Func_dualproj( CUR.zp2.org_x[point],
CUR.zp2.org_y[point] );
cur_x = CUR_Func_project( CUR.zp2.cur_x[point],
CUR.zp2.cur_y[point] );
if ( ( org_a <= org_b && org_x <= org_a ) ||
( org_a > org_b && org_x >= org_a ) )
distance = ( cur_a - org_a ) + ( org_x - cur_x );
else if ( ( org_a <= org_b && org_x >= org_b ) ||
( org_a > org_b && org_x < org_b ) )
distance = ( cur_b - org_b ) + ( org_x - cur_x );
else
distance = MulDiv_Round( cur_b - cur_a,
org_x - org_a,
org_b - org_a ) + ( cur_a - cur_x );
CUR_Func_move( &CUR.zp2, point, distance );
CUR.GS.loop--;
}
CUR.GS.loop = 1;
CUR.new_top = CUR.args;
}
static void Ins_UTP( INS_ARG )
{
Byte mask;
if ( BOUNDS( args[0], CUR.zp0.n_points ) )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
mask = 0xFF;
if ( CUR.GS.freeVector.x != 0 )
mask &= ~TT_Flag_Touched_X;
if ( CUR.GS.freeVector.y != 0 )
mask &= ~TT_Flag_Touched_Y;
CUR.zp0.touch[args[0]] &= mask;
}
struct LOC_Ins_IUP
{
PCoordinates orgs;
PCoordinates curs;
};
static void Shift( Int p1,
Int p2,
Int p,
struct LOC_Ins_IUP* LINK )
{
Int i;
TT_F26Dot6 x;
x = LINK->curs[p] - LINK->orgs[p];
for ( i = p1; i < p; i++ )
LINK->curs[i] += x;
for ( i = p + 1; i <= p2; i++ )
LINK->curs[i] += x;
}
static void Interp( Int p1, Int p2,
Int ref1, Int ref2,
struct LOC_Ins_IUP* LINK )
{
Long i;
TT_F26Dot6 x, x1, x2, d1, d2;
if ( p1 > p2 )
return;
x1 = LINK->orgs[ref1];
d1 = LINK->curs[ref1] - LINK->orgs[ref1];
x2 = LINK->orgs[ref2];
d2 = LINK->curs[ref2] - LINK->orgs[ref2];
if ( x1 == x2 )
{
for ( i = p1; i <= p2; i++ )
{
x = LINK->orgs[i];
if ( x <= x1 )
x += d1;
else
x += d2;
LINK->curs[i] = x;
}
return;
}
if ( x1 < x2 )
{
for ( i = p1; i <= p2; i++ )
{
x = LINK->orgs[i];
if ( x <= x1 )
x += d1;
else
{
if ( x >= x2 )
x += d2;
else
x = LINK->curs[ref1] +
MulDiv_Round( x - x1,
LINK->curs[ref2] - LINK->curs[ref1],
x2 - x1 );
}
LINK->curs[i] = x;
}
return;
}
for ( i = p1; i <= p2; i++ )
{
x = LINK->orgs[i];
if ( x <= x2 )
x += d2;
else
{
if ( x >= x1 )
x += d1;
else
x = LINK->curs[ref1] +
MulDiv_Round( x - x1,
LINK->curs[ref2] - LINK->curs[ref1],
x2 - x1 );
}
LINK->curs[i] = x;
}
}
static void Ins_IUP( INS_ARG )
{
struct LOC_Ins_IUP V;
unsigned char mask;
Long first_point;
Long end_point;
Long first_touched;
Long cur_touched;
Long point;
Long contour;
(void)args;
if ( CUR.opcode & 1 )
{
mask = TT_Flag_Touched_X;
V.orgs = CUR.pts.org_x;
V.curs = CUR.pts.cur_x;
}
else
{
mask = TT_Flag_Touched_Y;
V.orgs = CUR.pts.org_y;
V.curs = CUR.pts.cur_y;
}
contour = 0;
point = 0;
do
{
end_point = CUR.pts.contours[contour];
first_point = point;
while ( point <= end_point && (CUR.pts.touch[point] & mask) == 0 )
point++;
if ( point <= end_point )
{
first_touched = point;
cur_touched = point;
point++;
while ( point <= end_point )
{
if ( (CUR.pts.touch[point] & mask) != 0 )
{
Interp( (Int)(cur_touched + 1),
(Int)(point - 1),
(Int)cur_touched,
(Int)point,
&V );
cur_touched = point;
}
point++;
}
if ( cur_touched == first_touched )
Shift( (Int)first_point, (Int)end_point, (Int)cur_touched, &V );
else
{
Interp((Int)(cur_touched + 1),
(Int)(end_point),
(Int)(cur_touched),
(Int)(first_touched),
&V );
Interp((Int)(first_point),
(Int)(first_touched - 1),
(Int)(cur_touched),
(Int)(first_touched),
&V );
}
}
contour++;
} while ( contour < CUR.pts.n_contours );
}
static void Ins_DELTAP( INS_ARG )
{
Int k;
Long A, B, C, nump;
nump = args[0];
for ( k = 1; k <= nump; k++ )
{
if ( CUR.args < 2 )
{
CUR.error = TT_Err_Too_Few_Arguments;
return;
}
CUR.args -= 2;
A = CUR.stack[CUR.args + 1];
B = CUR.stack[CUR.args];
#if 0
if ( BOUNDS( A, CUR.zp0.n_points ) )
#else
if ( BOUNDS( A, CUR.zp0.n_points + 2 ) )
#endif
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
C = (B & 0xF0) >> 4;
switch ( CUR.opcode )
{
case 0x5d:
break;
case 0x71:
C += 16;
break;
case 0x72:
C += 32;
break;
}
C += CUR.GS.delta_base;
if ( CURRENT_Ppem() == C )
{
B = (B & 0xF) - 8;
if ( B >= 0 )
B++;
B = B * 64 / (1L << CUR.GS.delta_shift);
CUR_Func_move( &CUR.zp0, (Int)A, (Int)B );
}
}
CUR.new_top = CUR.args;
}
static void Ins_DELTAC( INS_ARG )
{
Long nump, k;
Long A, B, C;
nump = args[0];
for ( k = 1; k <= nump; k++ )
{
if ( CUR.args < 2 )
{
CUR.error = TT_Err_Too_Few_Arguments;
return;
}
CUR.args -= 2;
A = CUR.stack[CUR.args + 1];
B = CUR.stack[CUR.args];
if ( A >= CUR.cvtSize )
{
CUR.error = TT_Err_Invalid_Reference;
return;
}
C = ((unsigned long)(B & 0xF0)) >> 4;
switch ( CUR.opcode )
{
case 0x73:
break;
case 0x74:
C += 16;
break;
case 0x75:
C += 32;
break;
}
C += CUR.GS.delta_base;
if ( CURRENT_Ppem() == C )
{
B = (B & 0xF) - 8;
if ( B >= 0 )
B++;
B = B * 64 / (1L << CUR.GS.delta_shift);
CUR_Func_move_cvt( A, B );
}
}
CUR.new_top = CUR.args;
}
static void Ins_DEBUG( INS_ARG )
{ (void)args;
CUR.error = TT_Err_Debug_OpCode;
}
static void Ins_GETINFO( INS_ARG )
{
Long K;
K = 0;
if ( (args[0] & 1) != 0 )
K = 3;
if ( CUR.metrics.rotated )
K |= 0x80;
if ( CUR.metrics.stretched )
K |= 0x100;
args[0] = K;
}
static void Ins_UNKNOWN( INS_ARG )
{
Byte i;
TDefRecord* def;
PCallRecord call;
# if 0
if (CUR.opcode > sizeof(CUR.IDefPtr) / sizeof(CUR.IDefPtr[0])) {
CUR.error = TT_Err_Invalid_Opcode;
return;
}
# endif
i = CUR.IDefPtr[(Byte)CUR.opcode];
if (i >= CUR.numIDefs)
{
CUR.error = TT_Err_Invalid_Opcode;
return;
}
def = &CUR.IDefs[i];
if ( CUR.callTop >= CUR.callSize )
{
CUR.error = TT_Err_Stack_Overflow;
return;
}
call = CUR.callStack + CUR.callTop++;
call->Caller_Range = CUR.curRange;
call->Caller_IP = CUR.IP+1;
call->Cur_Count = 1;
call->Cur_Restart = def->Start;
INS_Goto_CodeRange( def->Range, def->Start );
CUR.step_ins = FALSE;
return;
}
static struct { const char *sName; TInstruction_Function p; } Instruct_Dispatch[256] =
{
{"  SVTCA  y  ", Ins_SVTCA }
,{"  SVTCA  x  ", Ins_SVTCA }
,{"  SPvTCA y  ", Ins_SPVTCA }
,{"  SPvTCA x  ", Ins_SPVTCA }
,{"  SFvTCA y  ", Ins_SFVTCA }
,{"  SFvTCA x  ", Ins_SFVTCA }
,{"  SPvTL
,{"  SPvTL +   ", Ins_SPVTL }
,{"  SFvTL
,{"  SFvTL +   ", Ins_SFVTL }
,{"  SPvFS     ", Ins_SPVFS }
,{"  SFvFS     ", Ins_SFVFS }
,{"  GPV       ", Ins_GPV }
,{"  GFV       ", Ins_GFV }
,{"  SFvTPv    ", Ins_SFVTPV }
,{"  ISECT     ", Ins_ISECT }
,{"  SRP0      ", Ins_SRP0 }
,{"  SRP1      ", Ins_SRP1 }
,{"  SRP2      ", Ins_SRP2 }
,{"  SZP0      ", Ins_SZP0 }
,{"  SZP1      ", Ins_SZP1 }
,{"  SZP2      ", Ins_SZP2 }
,{"  SZPS      ", Ins_SZPS }
,{"  SLOOP     ", Ins_SLOOP }
,{"  RTG       ", Ins_RTG }
,{"  RTHG      ", Ins_RTHG }
,{"  SMD       ", Ins_SMD }
,{"  ELSE      ", Ins_ELSE }
,{"  JMPR      ", Ins_JMPR }
,{"  SCvTCi    ", Ins_SCVTCI }
,{"  SSwCi     ", Ins_SSWCI }
,{"  SSW       ", Ins_SSW }
,{"  DUP       ", Ins_DUP }
,{"  POP       ", Ins_POP }
,{"  CLEAR     ", Ins_CLEAR }
,{"  SWAP      ", Ins_SWAP }
,{"  DEPTH     ", Ins_DEPTH }
,{"  CINDEX    ", Ins_CINDEX }
,{"  MINDEX    ", Ins_MINDEX }
,{"  AlignPTS  ", Ins_ALIGNPTS}
,{"  INS_$28   ", Ins_UNKNOWN }
,{"  UTP       ", Ins_UTP }
,{"  LOOPCALL  ", Ins_LOOPCALL}
,{"  CALL      ", Ins_CALL }
,{"  FDEF      ", Ins_FDEF }
,{"  ENDF      ", Ins_ENDF }
,{"  MDAP[0]   ", Ins_MDAP }
,{"  MDAP[1]   ", Ins_MDAP }
,{"  IUP[0]    ", Ins_IUP }
,{"  IUP[1]    ", Ins_IUP }
,{"  SHP[0]    ", Ins_SHP }
,{"  SHP[1]    ", Ins_SHP }
,{"  SHC[0]    ", Ins_SHC }
,{"  SHC[1]    ", Ins_SHC }
,{"  SHZ[0]    ", Ins_SHZ }
,{"  SHZ[1]    ", Ins_SHZ }
,{"  SHPIX     ", Ins_SHPIX }
,{"  IP        ", Ins_IP }
,{"  MSIRP[0]  ", Ins_MSIRP }
,{"  MSIRP[1]  ", Ins_MSIRP }
,{"  AlignRP   ", Ins_ALIGNRP }
,{"  RTDG      ", Ins_RTDG }
,{"  MIAP[0]   ", Ins_MIAP }
,{"  MIAP[1]   ", Ins_MIAP }
,{"  NPushB    ", Ins_NPUSHB }
,{"  NPushW    ", Ins_NPUSHW }
,{"  WS        ", Ins_WS }
,{"  RS        ", Ins_RS }
,{"  WCvtP     ", Ins_WCVTP }
,{"  RCvt      ", Ins_RCVT }
,{"  GC[0]     ", Ins_GC }
,{"  GC[1]     ", Ins_GC }
,{"  SCFS      ", Ins_SCFS }
,{"  MD[0]     ", Ins_MD }
,{"  MD[1]     ", Ins_MD }
,{"  MPPEM     ", Ins_MPPEM }
,{"  MPS       ", Ins_MPS }
,{"  FlipON    ", Ins_FLIPON }
,{"  FlipOFF   ", Ins_FLIPOFF }
,{"  DEBUG     ", Ins_DEBUG }
,{"  LT        ", Ins_LT }
,{"  LTEQ      ", Ins_LTEQ }
,{"  GT        ", Ins_GT }
,{"  GTEQ      ", Ins_GTEQ }
,{"  EQ        ", Ins_EQ }
,{"  NEQ       ", Ins_NEQ }
,{"  ODD       ", Ins_ODD }
,{"  EVEN      ", Ins_EVEN }
,{"  IF        ", Ins_IF }
,{"  EIF       ", Ins_EIF }
,{"  AND       ", Ins_AND }
,{"  OR        ", Ins_OR }
,{"  NOT       ", Ins_NOT }
,{"  DeltaP1   ", Ins_DELTAP }
,{"  SDB       ", Ins_SDB }
,{"  SDS       ", Ins_SDS }
,{"  ADD       ", Ins_ADD }
,{"  SUB       ", Ins_SUB }
,{"  DIV       ", Ins_DIV }
,{"  MUL       ", Ins_MUL }
,{"  ABS       ", Ins_ABS }
,{"  NEG       ", Ins_NEG }
,{"  FLOOR     ", Ins_FLOOR }
,{"  CEILING   ", Ins_CEILING }
,{"  ROUND[0]  ", Ins_ROUND }
,{"  ROUND[1]  ", Ins_ROUND }
,{"  ROUND[2]  ", Ins_ROUND }
,{"  ROUND[3]  ", Ins_ROUND }
,{"  NROUND[0] ", Ins_NROUND }
,{"  NROUND[1] ", Ins_NROUND }
,{"  NROUND[2] ", Ins_NROUND }
,{"  NROUND[3] ", Ins_NROUND }
,{"  WCvtF     ", Ins_WCVTF }
,{"  DeltaP2   ", Ins_DELTAP }
,{"  DeltaP3   ", Ins_DELTAP }
,{"  DeltaCn[0] ", Ins_DELTAC }
,{"  DeltaCn[1] ", Ins_DELTAC }
,{"  DeltaCn[2] ", Ins_DELTAC }
,{"  SROUND    ", Ins_SROUND }
,{"  S45Round  ", Ins_S45ROUND }
,{"  JROT      ", Ins_JROT }
,{"  JROF      ", Ins_JROF }
,{"  ROFF      ", Ins_ROFF }
,{"  INS_$7B   ", Ins_UNKNOWN }
,{"  RUTG      ", Ins_RUTG }
,{"  RDTG      ", Ins_RDTG }
,{"  SANGW     ", Ins_SANGW }
,{"  AA        ", Ins_AA }
,{"  FlipPT    ", Ins_FLIPPT }
,{"  FlipRgON  ", Ins_FLIPRGON }
,{"  FlipRgOFF ", Ins_FLIPRGOFF }
,{"  INS_$83   ", Ins_UNKNOWN }
,{"  INS_$84   ", Ins_UNKNOWN }
,{"  ScanCTRL  ", Ins_SCANCTRL }
,{"  SDPVTL[0] ", Ins_SDPVTL }
,{"  SDPVTL[1] ", Ins_SDPVTL }
,{"  GetINFO   ", Ins_GETINFO }
,{"  IDEF      ", Ins_IDEF }
,{"  ROLL      ", Ins_ROLL }
,{"  MAX       ", Ins_MAX }
,{"  MIN       ", Ins_MIN }
,{"  ScanTYPE  ", Ins_SCANTYPE }
,{"  InstCTRL  ", Ins_INSTCTRL }
,{"  INS_$8F   ", Ins_UNKNOWN }
,{"  INS_$90  ", Ins_UNKNOWN }
,{"  INS_$91  ", Ins_UNKNOWN }
,{"  INS_$92  ", Ins_UNKNOWN }
,{"  INS_$93  ", Ins_UNKNOWN }
,{"  INS_$94  ", Ins_UNKNOWN }
,{"  INS_$95  ", Ins_UNKNOWN }
,{"  INS_$96  ", Ins_UNKNOWN }
,{"  INS_$97  ", Ins_UNKNOWN }
,{"  INS_$98  ", Ins_UNKNOWN }
,{"  INS_$99  ", Ins_UNKNOWN }
,{"  INS_$9A  ", Ins_UNKNOWN }
,{"  INS_$9B  ", Ins_UNKNOWN }
,{"  INS_$9C  ", Ins_UNKNOWN }
,{"  INS_$9D  ", Ins_UNKNOWN }
,{"  INS_$9E  ", Ins_UNKNOWN }
,{"  INS_$9F  ", Ins_UNKNOWN }
,{"  INS_$A0  ", Ins_UNKNOWN }
,{"  INS_$A1  ", Ins_UNKNOWN }
,{"  INS_$A2  ", Ins_UNKNOWN }
,{"  INS_$A3  ", Ins_UNKNOWN }
,{"  INS_$A4  ", Ins_UNKNOWN }
,{"  INS_$A5  ", Ins_UNKNOWN }
,{"  INS_$A6  ", Ins_UNKNOWN }
,{"  INS_$A7  ", Ins_UNKNOWN }
,{"  INS_$A8  ", Ins_UNKNOWN }
,{"  INS_$A9  ", Ins_UNKNOWN }
,{"  INS_$AA  ", Ins_UNKNOWN }
,{"  INS_$AB  ", Ins_UNKNOWN }
,{"  INS_$AC  ", Ins_UNKNOWN }
,{"  INS_$AD  ", Ins_UNKNOWN }
,{"  INS_$AE  ", Ins_UNKNOWN }
,{"  INS_$AF  ", Ins_UNKNOWN }
,{"  PushB[0]  ", Ins_PUSHB }
,{"  PushB[1]  ", Ins_PUSHB }
,{"  PushB[2]  ", Ins_PUSHB }
,{"  PushB[3]  ", Ins_PUSHB }
,{"  PushB[4]  ", Ins_PUSHB }
,{"  PushB[5]  ", Ins_PUSHB }
,{"  PushB[6]  ", Ins_PUSHB }
,{"  PushB[7]  ", Ins_PUSHB }
,{"  PushW[0]  ", Ins_PUSHW }
,{"  PushW[1]  ", Ins_PUSHW }
,{"  PushW[2]  ", Ins_PUSHW }
,{"  PushW[3]  ", Ins_PUSHW }
,{"  PushW[4]  ", Ins_PUSHW }
,{"  PushW[5]  ", Ins_PUSHW }
,{"  PushW[6]  ", Ins_PUSHW }
,{"  PushW[7]  ", Ins_PUSHW }
,{"  MDRP[00]  ", Ins_MDRP }
,{"  MDRP[01]  ", Ins_MDRP }
,{"  MDRP[02]  ", Ins_MDRP }
,{"  MDRP[03]  ", Ins_MDRP }
,{"  MDRP[04]  ", Ins_MDRP }
,{"  MDRP[05]  ", Ins_MDRP }
,{"  MDRP[06]  ", Ins_MDRP }
,{"  MDRP[07]  ", Ins_MDRP }
,{"  MDRP[08]  ", Ins_MDRP }
,{"  MDRP[09]  ", Ins_MDRP }
,{"  MDRP[10]  ", Ins_MDRP }
,{"  MDRP[11]  ", Ins_MDRP }
,{"  MDRP[12]  ", Ins_MDRP }
,{"  MDRP[13]  ", Ins_MDRP }
,{"  MDRP[14]  ", Ins_MDRP }
,{"  MDRP[15]  ", Ins_MDRP }
,{"  MDRP[16]  ", Ins_MDRP }
,{"  MDRP[17]  ", Ins_MDRP }
,{"  MDRP[18]  ", Ins_MDRP }
,{"  MDRP[19]  ", Ins_MDRP }
,{"  MDRP[20]  ", Ins_MDRP }
,{"  MDRP[21]  ", Ins_MDRP }
,{"  MDRP[22]  ", Ins_MDRP }
,{"  MDRP[23]  ", Ins_MDRP }
,{"  MDRP[24]  ", Ins_MDRP }
,{"  MDRP[25]  ", Ins_MDRP }
,{"  MDRP[26]  ", Ins_MDRP }
,{"  MDRP[27]  ", Ins_MDRP }
,{"  MDRP[28]  ", Ins_MDRP }
,{"  MDRP[29]  ", Ins_MDRP }
,{"  MDRP[30]  ", Ins_MDRP }
,{"  MDRP[31]  ", Ins_MDRP }
,{"  MIRP[00]  ", Ins_MIRP }
,{"  MIRP[01]  ", Ins_MIRP }
,{"  MIRP[02]  ", Ins_MIRP }
,{"  MIRP[03]  ", Ins_MIRP }
,{"  MIRP[04]  ", Ins_MIRP }
,{"  MIRP[05]  ", Ins_MIRP }
,{"  MIRP[06]  ", Ins_MIRP }
,{"  MIRP[07]  ", Ins_MIRP }
,{"  MIRP[08]  ", Ins_MIRP }
,{"  MIRP[09]  ", Ins_MIRP }
,{"  MIRP[10]  ", Ins_MIRP }
,{"  MIRP[11]  ", Ins_MIRP }
,{"  MIRP[12]  ", Ins_MIRP }
,{"  MIRP[13]  ", Ins_MIRP }
,{"  MIRP[14]  ", Ins_MIRP }
,{"  MIRP[15]  ", Ins_MIRP }
,{"  MIRP[16]  ", Ins_MIRP }
,{"  MIRP[17]  ", Ins_MIRP }
,{"  MIRP[18]  ", Ins_MIRP }
,{"  MIRP[19]  ", Ins_MIRP }
,{"  MIRP[20]  ", Ins_MIRP }
,{"  MIRP[21]  ", Ins_MIRP }
,{"  MIRP[22]  ", Ins_MIRP }
,{"  MIRP[23]  ", Ins_MIRP }
,{"  MIRP[24]  ", Ins_MIRP }
,{"  MIRP[25]  ", Ins_MIRP }
,{"  MIRP[26]  ", Ins_MIRP }
,{"  MIRP[27]  ", Ins_MIRP }
,{"  MIRP[28]  ", Ins_MIRP }
,{"  MIRP[29]  ", Ins_MIRP }
,{"  MIRP[30]  ", Ins_MIRP }
,{"  MIRP[31]  ", Ins_MIRP }
};
TT_Error RunIns( PExecution_Context exc )
{
TT_Error Result;
Int A;
PDefRecord WITH;
PCallRecord WITH1;
bool bFirst;
bool dbg_prt = (DBG_PRT_FUN != NULL);
# ifdef DEBUG
ttfMemory *mem = exc->current_face->font->tti->ttf_memory;
F26Dot6 *save_ox, *save_oy, *save_cx, *save_cy;
DBG_PRINT("\n%% *** Entering RunIns ***");
# endif
CUR.metrics.ratio = 0;
if ( CUR.metrics.x_ppem != CUR.metrics.y_ppem )
{
CUR.func_read_cvt = Read_CVT_Stretched;
CUR.func_write_cvt = Write_CVT_Stretched;
CUR.func_move_cvt = Move_CVT_Stretched;
}
else
{
CUR.func_read_cvt = Read_CVT;
CUR.func_write_cvt = Write_CVT;
CUR.func_move_cvt = Move_CVT;
}
COMPUTE_Funcs();
Compute_Round( EXEC_ARGS (Byte)exc->GS.round_state );
# ifdef DEBUG
if (dbg_prt && CUR.pts.n_points) {
save_ox = mem->alloc_bytes(mem, CUR.pts.n_points * sizeof(*save_ox), "RunIns");
save_oy = mem->alloc_bytes(mem, CUR.pts.n_points * sizeof(*save_oy), "RunIns");
save_cx = mem->alloc_bytes(mem, CUR.pts.n_points * sizeof(*save_cx), "RunIns");
save_cy = mem->alloc_bytes(mem, CUR.pts.n_points * sizeof(*save_cy), "RunIns");
if (!save_ox || !save_oy || !save_cx || !save_cy)
return TT_Err_Out_Of_Memory;
} else
save_ox = save_oy = save_cx = save_cy = NULL;
# endif
Result = setjmp(exc->trap);
if (Result) {
CUR.error = Result;
goto _LExit;
}
bFirst = true;
do
{
CALC_Length();
CUR.args = CUR.top - Pop_Push_Count[CUR.opcode * 2];
if ( CUR.args < 0 )
{
CUR.error = TT_Err_Too_Few_Arguments;
goto _LErrorLabel;
}
CUR.new_top = CUR.args + Pop_Push_Count[CUR.opcode * 2 + 1];
if ( CUR.new_top > CUR.stackSize )
{
CUR.error = TT_Err_Stack_Overflow;
goto _LErrorLabel;
}
CUR.step_ins = TRUE;
CUR.error = TT_Err_Ok;
# ifdef DEBUG
DBG_PRINT3("\n%%n=%5d IP=%5d OP=%s            ", nInstrCount, CUR.IP, Instruct_Dispatch[CUR.opcode].sName);
if (save_ox != NULL) {
memcpy(save_ox, CUR.pts.org_x, sizeof(CUR.pts.org_x[0]) * CUR.pts.n_points);
memcpy(save_oy, CUR.pts.org_y, sizeof(CUR.pts.org_y[0]) * CUR.pts.n_points);
memcpy(save_cx, CUR.pts.cur_x, sizeof(CUR.pts.cur_x[0]) * CUR.pts.n_points);
memcpy(save_cy, CUR.pts.cur_y, sizeof(CUR.pts.cur_y[0]) * CUR.pts.n_points);
}
# endif
Instruct_Dispatch[CUR.opcode].p( EXEC_ARGS &CUR.stack[CUR.args] );
# ifdef DEBUG
if (save_ox != NULL) {
F26Dot6 *pp[4], *qq[4];
const char *ss[] = {"org.x", "org.y", "cur.x", "cur.y"};
int l = 0, i, j;
pp[0] = save_ox,
pp[1] = save_oy,
pp[2] = save_cx,
pp[3] = save_cy;
qq[0] = CUR.pts.org_x;
qq[1] = CUR.pts.org_y;
qq[2] = CUR.pts.cur_x;
qq[3] = CUR.pts.cur_y;
for(i = 0; i < 4; i++)
for(j = 0;j < CUR.pts.n_points; j++)
{ F26Dot6 *ppi = pp[i], *qqi = qq[i];
if(ppi[j] != qqi[j] || bFirst)
{
DBG_PRINT4("%%  %s[%d]%d:=%d", ss[i], j, pp[i][j], qq[i][j]);
if(++l > 3)
{ l=0;
DBG_PRINT("\n");
}
}
}
nInstrCount++;
bFirst=FALSE;
}
# endif
DBG_PAINT
if ( CUR.error != TT_Err_Ok )
{
switch ( CUR.error )
{
case TT_Err_Invalid_Opcode:
A = 0;
while ( A < CUR.numIDefs )
{
WITH = &CUR.IDefs[A];
if ( WITH->Active && CUR.opcode == WITH->Opc )
{
if ( CUR.callTop >= CUR.callSize )
{
CUR.error = TT_Err_Invalid_Reference;
goto _LErrorLabel;
}
WITH1 = &CUR.callStack[CUR.callTop];
WITH1->Caller_Range = CUR.curRange;
WITH1->Caller_IP = CUR.IP + 1;
WITH1->Cur_Count = 1;
WITH1->Cur_Restart = WITH->Start;
if ( INS_Goto_CodeRange( WITH->Range, WITH->Start ) == FAILURE )
goto _LErrorLabel;
goto _LSuiteLabel;
}
else
{
A++;
continue;
}
}
CUR.error = TT_Err_Invalid_Opcode;
goto _LErrorLabel;
break;
default:
CUR.error = CUR.error;
goto _LErrorLabel;
break;
}
}
CUR.top = CUR.new_top;
if ( CUR.step_ins )
CUR.IP += CUR.length;
_LSuiteLabel:
if ( CUR.IP >= CUR.codeSize )
{
if ( CUR.callTop > 0 )
{
CUR.error = TT_Err_Code_Overflow;
goto _LErrorLabel;
}
else
goto _LNo_Error;
}
} while ( !CUR.instruction_trap );
_LNo_Error:
Result = TT_Err_Ok;
goto _LExit;
_LErrorLabel:
Result = CUR.error;
DBG_PRINT1("%%  ERROR=%d", Result);
_LExit:
# ifdef DEBUG
if (save_ox != NULL) {
mem->free(mem, save_ox, "RunIns");
mem->free(mem, save_oy, "RunIns");
mem->free(mem, save_cx, "RunIns");
mem->free(mem, save_cy, "RunIns");
}
# endif
return Result;
}