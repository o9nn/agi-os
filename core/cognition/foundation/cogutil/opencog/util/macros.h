#ifndef _OPENCOG_MACROS_H
#define _OPENCOG_MACROS_H
#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)
#define TRACE_INFO " (" __FILE__ ":" TOSTRING(__LINE__) ")"
#define OC_UNUSED(varname) (void)varname;
#define FREAD_CK(ptr,size,count,stream) \
b_read = b_read && (fread(ptr,size,count,stream)==(size_t)count)
#define CHECK_FREAD \
{ if ( !b_read ) throw IOException(TRACE_INFO, "%s - failed to read.", __FUNCTION__ ); }
#ifndef _GLIBCXX_USE_NOEXCEPT
# if __GNUC__ >= 4
# define _GLIBCXX_USE_NOEXCEPT throw()
# else
# define _GLIBCXX_USE_NOEXCEPT
# endif
#endif
#endif