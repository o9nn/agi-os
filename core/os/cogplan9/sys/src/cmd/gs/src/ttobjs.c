#include "ttmisc.h"
#include "ttfoutl.h"
#include "ttobjs.h"
#include "ttcalc.h"
#include "ttload.h"
#include "ttinterp.h"
#ifdef TT_EXTEND_ENGINE
#endif
TT_Error  Goto_CodeRange( PExecution_Context  exec, Int  range, Int  IP )
{
PCodeRange  cr;
if ( range < 1 || range > 3 )
return TT_Err_Bad_Argument;
cr = &exec->codeRangeTable[range - 1];
if ( cr->Base == NULL )
return TT_Err_Invalid_CodeRange;
if ( IP > cr->Size )
return TT_Err_Code_Overflow;
exec->code     = cr->Base;
exec->codeSize = cr->Size;
exec->IP       = IP;
exec->curRange = range;
return TT_Err_Ok;
}
void  Unset_CodeRange( PExecution_Context  exec )
{
exec->code = 0;
exec->codeSize = 0;
}
PCodeRange  Get_CodeRange( PExecution_Context  exec, Int  range )
{
if ( range < 1 || range > 3 )
return (PCodeRange)NULL;
else
return &exec->codeRangeTable[range - 1];
}
TT_Error  Set_CodeRange( PExecution_Context  exec,
Int                 range,
void*               base,
Int                 length )
{
if ( range < 1 || range > 3 )
return TT_Err_Bad_Argument;
exec->codeRangeTable[range - 1].Base = (unsigned char*)base;
exec->codeRangeTable[range - 1].Size = length;
return TT_Err_Ok;
}
TT_Error Clear_CodeRange( PExecution_Context  exec, Int  range )
{
if ( range < 1 || range > 3 )
return TT_Err_Bad_Argument;
exec->codeRangeTable[range - 1].Base = (Byte*)NULL;
exec->codeRangeTable[range - 1].Size = 0;
return TT_Err_Ok;
}
#define FREE(ptr) { mem->free(mem, ptr, "ttobjs.c"); ptr = NULL; }
#define ALLOC_ARRAY(ptr, old_count, count, type) \
(old_count >= count ? 0 : \
!(free_aux(mem, ptr),   \
ptr = mem->alloc_bytes(mem, (count) * sizeof(type), "ttobjs.c")))
#define SETMAX(a, b) a = (a > b ? a : b)
static int free_aux(ttfMemory *mem, void *ptr)
{
mem->free(mem, ptr, "ttobjs.c");
return 0;
}
TT_Error  Context_Destroy( void*  _context )
{
PExecution_Context  exec = (PExecution_Context)_context;
ttfMemory *mem;
if ( !exec )
return TT_Err_Ok;
if ( !exec->current_face ) {
return TT_Err_Out_Of_Memory;
}
if (--exec->lock)
return TT_Err_Ok;
mem = exec->current_face->font->tti->ttf_memory;
FREE( exec->pts.cur_y );
FREE( exec->pts.cur_x );
FREE( exec->pts.org_y );
FREE( exec->pts.org_x );
FREE( exec->pts.touch );
FREE( exec->pts.contours );
exec->pts.n_points   = 0;
exec->pts.n_contours = 0;
FREE( exec->twilight.touch );
FREE( exec->twilight.cur_y );
FREE( exec->twilight.cur_x );
FREE( exec->twilight.org_y );
FREE( exec->twilight.org_x );
FREE( exec->twilight.contours );
exec->twilight.n_points   = 0;
exec->twilight.n_contours = 0;
FREE( exec->stack );
exec->stackSize = 0;
FREE( exec->callStack );
exec->callSize = 0;
exec->callTop  = 0;
exec->glyphSize = 0;
exec->maxGlyphSize = 0;
exec->current_face    = (PFace)NULL;
return TT_Err_Ok;
}
TT_Error  Context_Create( void*  _context, void*  _face )
{
PExecution_Context  exec = (PExecution_Context)_context;
PFace        face = (PFace)_face;
ttfMemory   *mem = face->font->tti->ttf_memory;
TMaxProfile *maxp = &face->maxProfile;
Int          n_points, n_twilight;
Int          callSize, stackSize;
callSize  = 32;
stackSize = maxp->maxStackElements + 32;
n_points        = face->maxPoints + 2;
n_twilight      = maxp->maxTwilightPoints;
if ( ALLOC_ARRAY( exec->callStack, exec->callSize, callSize, TCallRecord ) ||
ALLOC_ARRAY( exec->stack, exec->stackSize, stackSize, Long )           ||
ALLOC_ARRAY( exec->pts.org_x, exec->n_points, n_points, TT_F26Dot6 )        ||
ALLOC_ARRAY( exec->pts.org_y, exec->n_points, n_points, TT_F26Dot6 )        ||
ALLOC_ARRAY( exec->pts.cur_x, exec->n_points, n_points, TT_F26Dot6 )        ||
ALLOC_ARRAY( exec->pts.cur_y, exec->n_points, n_points, TT_F26Dot6 )        ||
ALLOC_ARRAY( exec->pts.touch, exec->n_points, n_points, Byte )                          ||
ALLOC_ARRAY( exec->twilight.org_x, exec->twilight.n_points, n_twilight, TT_F26Dot6 ) ||
ALLOC_ARRAY( exec->twilight.org_y, exec->twilight.n_points, n_twilight, TT_F26Dot6 ) ||
ALLOC_ARRAY( exec->twilight.cur_x, exec->twilight.n_points, n_twilight, TT_F26Dot6 ) ||
ALLOC_ARRAY( exec->twilight.cur_y, exec->twilight.n_points, n_twilight, TT_F26Dot6 ) ||
ALLOC_ARRAY( exec->twilight.touch, exec->twilight.n_points, n_twilight, Byte )                   ||
ALLOC_ARRAY( exec->pts.contours, exec->n_contours, face->maxContours, UShort )
)
goto Fail_Memory;
SETMAX(exec->callSize, callSize);
SETMAX(exec->stackSize, stackSize);
SETMAX(exec->twilight.n_points, n_twilight);
SETMAX(exec->maxGlyphSize, maxp->maxSizeOfInstructions);
SETMAX(exec->n_contours, face->maxContours);
SETMAX(exec->n_points, n_points);
exec->lock++;
return TT_Err_Ok;
Fail_Memory:
return TT_Err_Out_Of_Memory;
}
TT_Error Context_Load( PExecution_Context  exec,
PInstance           ins )
{
Int  i;
exec->current_face = ins->face;
exec->numFDefs = ins->numFDefs;
exec->numIDefs = ins->numIDefs;
exec->FDefs    = ins->FDefs;
exec->IDefs    = ins->IDefs;
exec->countIDefs = ins->countIDefs;
memcpy(exec->IDefPtr, ins->IDefPtr, sizeof(exec->IDefPtr));
exec->metrics  = ins->metrics;
for ( i = 0; i < MAX_CODE_RANGES; i++ )
exec->codeRangeTable[i] = ins->codeRangeTable[i];
exec->pts.n_points   = 0;
exec->pts.n_contours = 0;
exec->instruction_trap = FALSE;
exec->GS = ins->GS;
exec->cvtSize = ins->cvtSize;
exec->cvt     = ins->cvt;
exec->storeSize = ins->storeSize;
exec->storage   = ins->storage;
return TT_Err_Ok;
}
TT_Error  Context_Save( PExecution_Context  exec,
PInstance           ins )
{
Int  i;
for ( i = 0; i < MAX_CODE_RANGES; i++ ) {
ins->codeRangeTable[i] = exec->codeRangeTable[i];
exec->codeRangeTable[i].Base = 0;
exec->codeRangeTable[i].Size = 0;
}
exec->numFDefs = 0;
exec->numIDefs = 0;
memcpy(ins->IDefPtr, exec->IDefPtr, sizeof(ins->IDefPtr));
ins->countIDefs = exec->countIDefs;
exec->countIDefs = 0;
exec->FDefs    = 0;
exec->IDefs    = 0;
exec->cvtSize = 0;
exec->cvt     = 0;
exec->storeSize = 0;
exec->storage   = 0;
exec->current_face = 0;
return TT_Err_Ok;
}
TT_Error  Context_Run( PExecution_Context  exec,
Bool                debug )
{
TT_Error  error;
if ( ( error = Goto_CodeRange( exec, TT_CodeRange_Glyph, 0 ) ) )
return error;
exec->zp0 = exec->pts;
exec->zp1 = exec->pts;
exec->zp2 = exec->pts;
exec->GS.gep0 = 1;
exec->GS.gep1 = 1;
exec->GS.gep2 = 1;
exec->GS.projVector.x = 0x4000;
exec->GS.projVector.y = 0x0000;
exec->GS.freeVector = exec->GS.projVector;
exec->GS.dualVector = exec->GS.projVector;
exec->GS.round_state = 1;
exec->GS.loop        = 1;
exec->top     = 0;
exec->callTop = 0;
if ( !debug ) {
error = RunIns( exec );
Unset_CodeRange(exec);
return error;
} else
return TT_Err_Ok;
}
const TGraphicsState  Default_GraphicsState =
{
0, 0, 0,
{ 0x4000, 0 },
{ 0x4000, 0 },
{ 0x4000, 0 },
1, 64, 1,
TRUE, 68, 0, 0, 9, 3,
0, FALSE, 2, 1, 1, 1
};
TT_Error  Instance_Destroy( void* _instance )
{
PInstance  ins = (PInstance)_instance;
ttfMemory *mem;
if ( !_instance )
return TT_Err_Ok;
if ( !ins->face ) {
return TT_Err_Out_Of_Memory;
}
mem = ins->face->font->tti->ttf_memory;
FREE( ins->cvt );
ins->cvtSize = 0;
FREE( ins->FDefs );
FREE( ins->IDefs );
FREE( ins->storage );
ins->numFDefs = 0;
ins->numIDefs = 0;
ins->face = (PFace)NULL;
ins->valid = FALSE;
return TT_Err_Ok;
}
TT_Error  Instance_Create( void*  _instance,
void*  _face )
{
PInstance ins  = (PInstance)_instance;
PFace     face = (PFace)_face;
ttfMemory *mem = face->font->tti->ttf_memory;
PMaxProfile  maxp = &face->maxProfile;
Int       i;
ins->FDefs=NULL;
ins->IDefs=NULL;
ins->cvt=NULL;
ins->storage=NULL;
ins->face = face;
ins->valid = FALSE;
ins->numFDefs = maxp->maxFunctionDefs;
ins->numIDefs = maxp->maxInstructionDefs;
ins->countIDefs = 0;
if (maxp->maxInstructionDefs > 255)
goto Fail_Memory;
memset(ins->IDefPtr, (Byte)ins->numIDefs, sizeof(ins->IDefPtr));
if (ins->numFDefs < 50)
ins->numFDefs = 50;
ins->cvtSize  = face->cvtSize;
ins->metrics.pointSize    = 10 * 64;
ins->metrics.x_resolution = 96;
ins->metrics.y_resolution = 96;
ins->metrics.x_ppem = 0;
ins->metrics.y_ppem = 0;
ins->metrics.rotated   = FALSE;
ins->metrics.stretched = FALSE;
ins->storeSize = maxp->maxStorage;
for ( i = 0; i < 4; i++ )
ins->metrics.compensations[i] = 0;
if ( ALLOC_ARRAY( ins->FDefs, 0, ins->numFDefs, TDefRecord )  ||
ALLOC_ARRAY( ins->IDefs, 0, ins->numIDefs, TDefRecord )  ||
ALLOC_ARRAY( ins->cvt, 0, ins->cvtSize, Long )           ||
ALLOC_ARRAY( ins->storage, 0, ins->storeSize, Long )     )
goto Fail_Memory;
memset (ins->FDefs, 0, ins->numFDefs * sizeof(TDefRecord));
memset (ins->IDefs, 0, ins->numIDefs * sizeof(TDefRecord));
ins->GS = Default_GraphicsState;
return TT_Err_Ok;
Fail_Memory:
Instance_Destroy( ins );
return TT_Err_Out_Of_Memory;
}
TT_Error  Instance_Init( PInstance  ins )
{
PExecution_Context  exec;
TT_Error  error;
PFace     face = ins->face;
exec = ins->face->font->exec;
ins->GS = Default_GraphicsState;
Context_Load( exec, ins );
exec->callTop   = 0;
exec->top       = 0;
exec->period    = 64;
exec->phase     = 0;
exec->threshold = 0;
exec->metrics.x_ppem    = 0;
exec->metrics.y_ppem    = 0;
exec->metrics.pointSize = 0;
exec->metrics.x_scale1  = 0;
exec->metrics.x_scale2  = 1;
exec->metrics.y_scale1  = 0;
exec->metrics.y_scale2  = 1;
exec->metrics.ppem      = 0;
exec->metrics.scale1    = 0;
exec->metrics.scale2    = 1;
exec->metrics.ratio     = 1 << 16;
exec->instruction_trap = FALSE;
exec->cvtSize = ins->cvtSize;
exec->cvt     = ins->cvt;
exec->F_dot_P = 0x10000;
Set_CodeRange( exec,
TT_CodeRange_Font,
face->fontProgram,
face->fontPgmSize );
Clear_CodeRange( exec, TT_CodeRange_Cvt );
Clear_CodeRange( exec, TT_CodeRange_Glyph );
if ( face->fontPgmSize > 0 )
{
error = Goto_CodeRange( exec, TT_CodeRange_Font, 0 );
if ( error )
goto Fin;
error = RunIns( exec );
Unset_CodeRange(exec);
}
else
error = TT_Err_Ok;
Fin:
Context_Save( exec, ins );
ins->valid = FALSE;
return error;
}
TT_Error  Instance_Reset( PInstance  ins,
Bool       debug )
{
TT_Error  error;
Int       i;
PFace     face = ins->face;
PExecution_Context exec = ins->face->font->exec;
if ( !ins )
return TT_Err_Invalid_Instance_Handle;
if ( ins->valid )
return TT_Err_Ok;
if ( ins->metrics.x_ppem < 1 ||
ins->metrics.y_ppem < 1 )
return TT_Err_Invalid_PPem;
if ( ins->metrics.x_ppem >= ins->metrics.y_ppem )
{
ins->metrics.scale1  = ins->metrics.x_scale1;
ins->metrics.scale2  = ins->metrics.x_scale2;
ins->metrics.ppem    = ins->metrics.x_ppem;
ins->metrics.x_ratio = 1 << 16;
ins->metrics.y_ratio = MulDiv_Round( ins->metrics.y_ppem,
0x10000,
ins->metrics.x_ppem );
}
else
{
ins->metrics.scale1  = ins->metrics.y_scale1;
ins->metrics.scale2  = ins->metrics.y_scale2;
ins->metrics.ppem    = ins->metrics.y_ppem;
ins->metrics.x_ratio = MulDiv_Round( ins->metrics.x_ppem,
0x10000,
ins->metrics.y_ppem );
ins->metrics.y_ratio = 1 << 16;
}
for ( i = 0; i < ins->cvtSize; i++ )
ins->cvt[i] = MulDiv_Round( face->cvt[i],
ins->metrics.scale1,
ins->metrics.scale2 );
ins->GS = Default_GraphicsState;
Context_Load( exec, ins );
Set_CodeRange( exec,
TT_CodeRange_Cvt,
face->cvtProgram,
face->cvtPgmSize );
Clear_CodeRange( exec, TT_CodeRange_Glyph );
for ( i = 0; i < exec->storeSize; i++ )
exec->storage[i] = 0;
exec->instruction_trap = FALSE;
exec->top     = 0;
exec->callTop = 0;
for ( i = 0; i < exec->twilight.n_points; i++ )
{
exec->twilight.org_x[i] = 0;
exec->twilight.org_y[i] = 0;
exec->twilight.cur_x[i] = 0;
exec->twilight.cur_y[i] = 0;
}
if ( face->cvtPgmSize > 0 )
{
error = Goto_CodeRange( exec, TT_CodeRange_Cvt, 0 );
if (error)
goto Fin;
error = RunIns( exec );
Unset_CodeRange(exec);
}
else
error = TT_Err_Ok;
ins->GS = exec->GS;
Fin:
Context_Save( exec, ins );
if ( !error )
ins->valid = TRUE;
return error;
}
TT_Error  Face_Destroy( PFace face )
{
ttfMemory *mem = face->font->tti->ttf_memory;
if ( !face )
return TT_Err_Ok;
FREE( face->cvt );
face->cvtSize = 0;
FREE( face->fontProgram );
FREE( face->cvtProgram );
face->fontPgmSize = 0;
face->cvtPgmSize  = 0;
return TT_Err_Ok;
}
#define LOAD_( table ) \
( error = Load_TrueType_##table (face) )
TT_Error  Face_Create( PFace  face)
{
TT_Error      error;
if (
LOAD_(MaxProfile)                  ||
LOAD_(CVT)                         ||
LOAD_(Programs)
)
goto Fail;
return TT_Err_Ok;
Fail :
Face_Destroy( face );
return error;
}
TT_Pos  Scale_X( PIns_Metrics  metrics, TT_Pos  x )
{
return MulDiv_Round( x, metrics->x_scale1, metrics->x_scale2 );
}
TT_Pos  Scale_Y( PIns_Metrics  metrics, TT_Pos  y )
{
return MulDiv_Round( y, metrics->y_scale1, metrics->y_scale2 );
}