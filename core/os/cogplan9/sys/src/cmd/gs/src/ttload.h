#ifndef TTLOAD_H
#define TTLOAD_H
#include "ttcommon.h"
#ifdef __cplusplus
extern "C" {
#endif
Int LookUp_TrueType_Table( PFace face,
Long tag );
TT_Error Load_TrueType_Directory ( PFace face,
int faceIndex );
TT_Error Load_TrueType_MaxProfile ( PFace face );
TT_Error Load_TrueType_Gasp ( PFace face );
TT_Error Load_TrueType_Header ( PFace face );
TT_Error Load_TrueType_Horizontal_Header( PFace face );
TT_Error Load_TrueType_Locations ( PFace face );
TT_Error Load_TrueType_Names ( PFace face );
TT_Error Load_TrueType_CVT ( PFace face );
TT_Error Load_TrueType_CMap ( PFace face );
TT_Error Load_TrueType_HMTX ( PFace face );
TT_Error Load_TrueType_Programs ( PFace face );
TT_Error Load_TrueType_OS2 ( PFace face );
TT_Error Load_TrueType_PostScript ( PFace face );
TT_Error Load_TrueType_Hdmx ( PFace face );
TT_Error Load_TrueType_Any( PFace face,
Long tag,
Long offset,
void* buffer,
Long* length );
TT_Error Free_TrueType_Names( PFace face );
TT_Error Free_TrueType_Hdmx ( PFace face );
#define USE_Stream( original, duplicate ) \
( error = TT_Use_Stream( original, &duplicate ) )
#define DONE_Stream( _stream ) \
TT_Done_Stream( &_stream )
#define DEFINE_A_FRAME TFileFrame frame = TT_Null_FileFrame
#define DEFINE_A_STREAM TT_Stream stream
#define GET_Byte() ttfReader__Byte (r)
#define GET_UShort() ttfReader__UShort(r)
#define GET_Short() ttfReader__Short (r)
#define GET_Long() ttfReader__Int (r)
#define GET_ULong() ttfReader__UInt(r)
#ifdef TT_CONFIG_REENTRANT
#define DEFINE_STREAM_LOCALS \
TT_Error error; \
DEFINE_A_STREAM; \
DEFINE_A_FRAME
#define DEFINE_STREAM_LOCALS_WO_FRAME \
TT_Error error; \
DEFINE_A_STREAM
#define DEFINE_LOAD_LOCALS( STREAM ) \
TT_Error error; \
DEFINE_A_STREAM = (STREAM); \
DEFINE_A_FRAME
#define DEFINE_LOAD_LOCALS_WO_FRAME( STREAM ) \
TT_Error error; \
DEFINE_A_STREAM = (STREAM)
#define DEFINE_ALL_LOCALS \
TT_Error error; \
DEFINE_A_STREAM; \
DEFINE_A_FRAME
#define ACCESS_Frame( _size_ ) \
( error = TT_Access_Frame( stream, &frame, _size_ ) )
#define CHECK_ACCESS_Frame( _size_ ) \
( error = TT_Check_And_Access_Frame( stream, &frame, _size_ ) )
#define FORGET_Frame() \
( error = TT_Forget_Frame( &frame ) )
#define FILE_Pos() TT_File_Pos ( stream )
#define FILE_Seek( _position_ ) \
( error = TT_Seek_File( stream, _position_ ) )
#define FILE_Skip( _distance_ ) \
( error = TT_Skip_File( stream, _distance_ ) )
#define FILE_Read( buffer, count ) \
( error = TT_Read_File ( stream, buffer, count ) )
#define FILE_Read_At( pos, buffer, count ) \
( error = TT_Read_At_File( stream, pos, buffer, count ) )
#else
#define DEFINE_STREAM_LOCALS \
TT_Error error
#define DEFINE_STREAM_LOCALS_WO_FRAME \
TT_Error error
#define DEFINE_LOAD_LOCALS( STREAM ) \
TT_Error error
#define DEFINE_LOAD_LOCALS_WO_FRAME( STREAM ) \
TT_Error error
#define DEFINE_ALL_LOCALS \
TT_Error error; \
DEFINE_A_STREAM
#define ACCESS_Frame( _size_ ) \
( error = TT_Access_Frame( _size_ ) )
#define CHECK_ACCESS_Frame( _size_ ) \
( error = TT_Check_And_Access_Frame( _size_ ) )
#define FORGET_Frame() \
( error = TT_Forget_Frame() )
#define GET_Tag4() TT_Get_Long ()
#define FILE_Pos() TT_File_Pos()
#define FILE_Seek( _position_ ) \
( error = TT_Seek_File( _position_ ) )
#define FILE_Skip( _distance_ ) \
( error = TT_Skip_File( _distance_ ) )
#define FILE_Read( buffer, count ) \
( error = TT_Read_File ( buffer, count ) )
#define FILE_Read_At( pos, buffer, count ) \
( error = TT_Read_At_File( pos, buffer, count ) )
#endif
#ifdef __cplusplus
}
#endif
#endif