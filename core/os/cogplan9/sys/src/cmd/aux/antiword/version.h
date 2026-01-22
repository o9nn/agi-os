#if !defined(__version_h)
#define __version_h 1
#define PURPOSESTRING	"Display MS-Word files"
#if defined(__riscos)
#define AUTHORSTRING	" 1998-2005 Adri van Os"
#else
#define AUTHORSTRING	"(C) 1998-2005 Adri van Os"
#endif
#define VERSIONSTRING	"0.37  (21 Oct 2005)"
#if defined(__dos)
#if defined(__DJGPP__)
#define VERSIONSTRING2	" # 32-bit Protected Mode"
#else
#define VERSIONSTRING2	" # 16-bit Real Mode"
#endif
#endif
#if defined(DEBUG)
#define STATUSSTRING	"DEBUG version"
#else
#define STATUSSTRING	"GNU General Public License"
#endif
#endif