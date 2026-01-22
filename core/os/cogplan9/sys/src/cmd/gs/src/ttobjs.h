#ifndef TTOBJS_H
#define TTOBJS_H
#include "ttcommon.h"
#include "tttypes.h"
#include "tttables.h"
#include <setjmp.h>
#ifdef __cplusplus
extern "C" {
#endif
struct _TFace;
struct _TInstance;
struct _TExecution_Context;
struct _TGlyph;
#ifndef TFace_defined
#define TFace_defined
typedef struct _TFace  TFace;
#endif
typedef TFace*         PFace;
#ifndef TInstance_defined
#define TInstance_defined
typedef struct _TInstance TInstance;
#endif
typedef TInstance*         PInstance;
#ifndef TExecution_Context_defined
#define TExecution_Context_defined
typedef struct _TExecution_Context TExecution_Context;
#endif
typedef TExecution_Context*         PExecution_Context;
typedef struct _TGlyph  TGlyph;
typedef TGlyph*         PGlyph;
struct  _TGraphicsState
{
Int            rp0;
Int            rp1;
Int            rp2;
TT_UnitVector  dualVector;
TT_UnitVector  projVector;
TT_UnitVector  freeVector;
Long           loop;
TT_F26Dot6     minimum_distance;
Int            round_state;
Bool           auto_flip;
TT_F26Dot6     control_value_cutin;
TT_F26Dot6     single_width_cutin;
TT_F26Dot6     single_width_value;
Int            delta_base;
Int            delta_shift;
Byte           instruct_control;
Bool           scan_control;
Int            scan_type;
Int            gep0;
Int            gep1;
Int            gep2;
};
typedef struct _TGraphicsState  TGraphicsState;
extern const TGraphicsState  Default_GraphicsState;
#  define MAX_CODE_RANGES   3
#  define TT_CodeRange_Font  1
#  define TT_CodeRange_Cvt   2
#  define TT_CodeRange_Glyph 3
struct  _TCodeRange
{
PByte  Base;
Int    Size;
};
typedef struct _TCodeRange  TCodeRange;
typedef TCodeRange*         PCodeRange;
typedef TCodeRange  TCodeRangeTable[MAX_CODE_RANGES];
struct  _TDefRecord
{
Int   Range;
Int   Start;
Byte  Opc;
Bool  Active;
};
typedef struct _TDefRecord  TDefRecord;
typedef TDefRecord*         PDefRecord;
typedef TDefRecord*         PDefArray;
struct  _TCallRecord
{
Int  Caller_Range;
Int  Caller_IP;
Int  Cur_Count;
Int  Cur_Restart;
};
typedef struct _TCallRecord  TCallRecord;
typedef TCallRecord*         PCallRecord;
typedef TCallRecord*         PCallStack;
struct  _TGlyph_Zone
{
int           n_points;
int           n_contours;
PCoordinates  org_x;
PCoordinates  org_y;
PCoordinates  cur_x;
PCoordinates  cur_y;
Byte*         touch;
Short*        contours;
};
typedef struct _TGlyph_Zone  TGlyph_Zone;
typedef TGlyph_Zone         *PGlyph_Zone;
#ifndef TT_STATIC_INTERPRETER
#define EXEC_OPS   PExecution_Context exc,
#define EXEC_OP    PExecution_Context exc
#define EXEC_ARGS  exc,
#define EXEC_ARG   exc
#else
#define EXEC_OPS
#define EXEC_OP
#define EXEC_ARGS
#define EXEC_ARG
#endif
typedef TT_F26Dot6  (*TRound_Function)( EXEC_OPS TT_F26Dot6 distance,
TT_F26Dot6 compensation );
typedef void  (*TMove_Function)( EXEC_OPS PGlyph_Zone zone,
Int         point,
TT_F26Dot6  distance );
typedef TT_F26Dot6  (*TProject_Function)( EXEC_OPS TT_F26Dot6 Vx,
TT_F26Dot6 Vy );
typedef TT_F26Dot6  (*TGet_CVT_Function)( EXEC_OPS  Int index );
typedef void  (*TSet_CVT_Function)( EXEC_OPS  Int         index,
TT_F26Dot6  value );
struct  _TTransform
{
TT_Fixed    xx, xy;
TT_Fixed    yx, yy;
TT_F26Dot6  ox, oy;
};
typedef struct _TTransform  TTransform;
typedef TTransform         *PTransform;
struct  _TSubglyph_Record
{
Int          index;
Bool         is_scaled;
Bool         is_hinted;
Bool         preserve_pps;
Long         file_offset;
TT_BBox      bbox;
TGlyph_Zone  zone;
Int          arg1;
Int          arg2;
Int          element_flag;
TTransform   transform;
TT_Vector    pp1, pp2;
Int          leftBearing;
Int          advanceWidth;
};
typedef struct _TSubglyph_Record  TSubglyph_Record;
typedef TSubglyph_Record*         PSubglyph_Record;
typedef TSubglyph_Record*         PSubglyph_Stack;
struct  _TIns_Metrics
{
TT_F26Dot6  pointSize;
Int         x_resolution;
Int         y_resolution;
Int         x_ppem;
Int         y_ppem;
Long        x_scale1;
Long        x_scale2;
Long        y_scale1;
Long        y_scale2;
Long        x_ratio;
Long        y_ratio;
Int         ppem;
Long        ratio;
Long        scale1;
Long        scale2;
TT_F26Dot6  compensations[4];
Bool        rotated;
Bool        stretched;
};
typedef struct _TIns_Metrics  TIns_Metrics;
typedef TIns_Metrics         *PIns_Metrics;
struct  _TFace
{
ttfReader *r;
ttfFont *font;
TMaxProfile  maxProfile;
Int       numLocations;
Int    fontPgmSize;
PByte  fontProgram;
Int    cvtPgmSize;
PByte  cvtProgram;
Int    cvtSize;
PShort cvt;
Int  numGlyphs;
Int  maxPoints;
Int  maxContours;
Int  maxComponents;
};
struct  _TInstance
{
PFace            face;
Bool             valid;
TIns_Metrics     metrics;
Int              numFDefs;
PDefArray        FDefs;
Int              numIDefs;
PDefArray        IDefs;
Int		     countIDefs;
Byte	     IDefPtr[256];
TCodeRangeTable  codeRangeTable;
TGraphicsState   GS;
TGraphicsState   default_GS;
Int              cvtSize;
PLong            cvt;
Int              storeSize;
PStorage            storage;
};
struct  _TExecution_Context
{
PFace           current_face;
Int             error;
Int             curRange;
PByte           code;
Int             IP;
Int             codeSize;
Byte            opcode;
Int             length;
Bool            step_ins;
Int             numFDefs;
PDefRecord      FDefs;
Int             numIDefs;
PDefRecord      IDefs;
Int		    countIDefs;
Byte	    IDefPtr[256];
PByte           glyphIns;
Int             glyphSize;
Int             callTop,
callSize;
PCallStack      callStack;
TCodeRangeTable codeRangeTable;
Int             storeSize;
PStorage        storage;
Int             stackSize;
Int             top;
PStorage        stack;
Int             args,
new_top;
TT_F26Dot6      period;
TT_F26Dot6      phase;
TT_F26Dot6      threshold;
TIns_Metrics    metrics;
Int             cur_ppem;
Long            scale1;
Long            scale2;
Bool            cached_metrics;
TGlyph_Zone     zp0,
zp1,
zp2,
pts,
twilight;
Bool            instruction_trap;
TGraphicsState  GS;
TGraphicsState  default_GS;
Bool            is_composite;
Int             cvtSize;
PLong           cvt;
Long               F_dot_P;
TRound_Function    func_round;
TProject_Function  func_project,
func_dualproj,
func_freeProj;
TMove_Function     func_move;
TGet_CVT_Function  func_read_cvt;
TSet_CVT_Function  func_write_cvt;
TSet_CVT_Function  func_move_cvt;
jmp_buf            trap;
Int                n_contours;
Int                n_points;
Int                maxGlyphSize;
Int                lock;
};
TT_Error  Goto_CodeRange( PExecution_Context  exec, Int  range, Int  IP );
void  Unset_CodeRange( PExecution_Context  exec );
PCodeRange  Get_CodeRange( PExecution_Context  exec, Int  range );
TT_Error  Set_CodeRange( PExecution_Context  exec,
Int                 range,
void*               base,
Int                 length );
TT_Error  Clear_CodeRange( PExecution_Context  exec, Int  range );
PExecution_Context  New_Context( PFace  face );
TT_Error  Done_Context( PExecution_Context  exec );
TT_Error  Context_Load( PExecution_Context  exec,
PInstance           ins );
TT_Error  Context_Save( PExecution_Context  exec,
PInstance           ins );
TT_Error  Context_Run( PExecution_Context  exec,
Bool                debug );
TT_Error  Instance_Init( PInstance  ins );
TT_Error  Instance_Reset( PInstance  ins,
Bool       debug );
TT_Error  Instance_Create( void*  _instance,
void*  _face );
TT_Error  Instance_Destroy( void* _instance );
TT_Error  Context_Destroy( void*  _context );
TT_Error  Context_Create( void*  _context, void*  _face );
TT_Pos   Scale_X( PIns_Metrics  metrics, TT_Pos  x );
TT_Pos   Scale_Y( PIns_Metrics  metrics, TT_Pos  y );
TT_Error  Face_Create( PFace  _face);
TT_Error  Face_Destroy( PFace  _face);
#ifdef __cplusplus
}
#endif
#endif