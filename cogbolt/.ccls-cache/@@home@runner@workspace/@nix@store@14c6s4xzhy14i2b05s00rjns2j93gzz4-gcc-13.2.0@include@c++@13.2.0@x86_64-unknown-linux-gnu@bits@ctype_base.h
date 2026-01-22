namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
struct ctype_base
{
typedef const int* 		__to_type;
typedef unsigned short 	mask;
static const mask upper    	= _ISupper;
static const mask lower 	= _ISlower;
static const mask alpha 	= _ISalpha;
static const mask digit 	= _ISdigit;
static const mask xdigit 	= _ISxdigit;
static const mask space 	= _ISspace;
static const mask print 	= _ISprint;
static const mask graph 	= _ISalpha | _ISdigit | _ISpunct;
static const mask cntrl 	= _IScntrl;
static const mask punct 	= _ISpunct;
static const mask alnum 	= _ISalpha | _ISdigit;
#if __cplusplus >= 201103L
static const mask blank	= _ISblank;
#endif
};
_GLIBCXX_END_NAMESPACE_VERSION
}