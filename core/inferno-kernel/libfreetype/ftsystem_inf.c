#include <ft2build.h>
#include FT_CONFIG_CONFIG_H
#include <freetype/internal/ftdebug.h>
#include <freetype/ftsystem.h>
#include <freetype/fterrors.h>
#include <freetype/fttypes.h>
#include "lib9.h"
#include "kernel.h"
FT_CALLBACK_DEF( void* )
ft_alloc( FT_Memory  memory,
long       size )
{
FT_UNUSED( memory );
return malloc( size );
}
FT_CALLBACK_DEF( void* )
ft_realloc( FT_Memory  memory,
long       cur_size,
long       new_size,
void*      block )
{
FT_UNUSED( memory );
FT_UNUSED( cur_size );
return realloc( block, new_size );
}
FT_CALLBACK_DEF( void )
ft_free( FT_Memory  memory,
void*      block )
{
FT_UNUSED( memory );
free( block );
}
#undef  FT_COMPONENT
#define FT_COMPONENT  trace_io
#define STREAM_FD( stream )  ( (int)stream->descriptor.pointer )
#define CLOSED_FD	(void*)-1
FT_CALLBACK_DEF( void )
ft_ansi_stream_close( FT_Stream  stream )
{
kclose( STREAM_FD( stream ) );
stream->descriptor.pointer = CLOSED_FD;
stream->size               = 0;
stream->base               = 0;
}
FT_CALLBACK_DEF( unsigned long )
ft_ansi_stream_io( FT_Stream       stream,
unsigned long   offset,
unsigned char*  buffer,
unsigned long   count )
{
int fd;
fd = STREAM_FD( stream );
kseek( fd, offset, SEEK_SET );
if(count == 0)
return 0;
return (unsigned long)kread( fd, buffer, count);
}
FT_EXPORT_DEF( FT_Error )
FT_Stream_Open( FT_Stream stream, const char*  filepathname)
{
Dir *dir;
int  file;
if ( !stream )
return FT_Err_Invalid_Stream_Handle;
file = kopen( (char*)filepathname, OREAD);
if ( file < 0) {
FT_ERROR(( "FT_Stream_Open:" ));
FT_ERROR(( " could not open `%s'\n", filepathname ));
return FT_Err_Cannot_Open_Resource;
}
dir = kdirfstat(file);
if (dir == nil) {
kclose(file);
FT_ERROR(( "FT_Stream_Open:" ));
FT_ERROR(( " could not stat `%s'\n", filepathname ));
return FT_Err_Cannot_Open_Resource;
}
stream->size = dir->length;
free(dir);
stream->descriptor.pointer = (void*)file;
stream->pathname.pointer = (char*)filepathname;
stream->pos = 0;
stream->read  = ft_ansi_stream_io;
stream->close = ft_ansi_stream_close;
FT_TRACE1(( "FT_Stream_Open:" ));
FT_TRACE1(( " opened `%s' (%d bytes) successfully\n",
filepathname, stream->size ));
return FT_Err_Ok;
}
#ifdef FT_DEBUG_MEMORY
extern FT_Int
ft_mem_debug_init( FT_Memory  memory );
extern void
ft_mem_debug_done( FT_Memory  memory );
#endif
FT_EXPORT_DEF( FT_Memory )
FT_New_Memory( void )
{
FT_Memory  memory;
memory = (FT_Memory)malloc( sizeof ( *memory ) );
if ( memory )
{
memory->user    = 0;
memory->alloc   = ft_alloc;
memory->realloc = ft_realloc;
memory->free    = ft_free;
#ifdef FT_DEBUG_MEMORY
ft_mem_debug_init( memory );
#endif
}
return memory;
}
FT_EXPORT_DEF( void )
FT_Done_Memory( FT_Memory  memory )
{
#ifdef FT_DEBUG_MEMORY
ft_mem_debug_done( memory );
#endif
memory->free( memory, memory );
}