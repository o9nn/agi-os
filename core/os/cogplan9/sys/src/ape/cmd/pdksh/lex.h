#define IDENT 64
typedef struct source Source;
struct source {
const char *str;
int type;
const char *start;
union {
char **strv;
struct shf *shf;
struct tbl *tblp;
char *freeme;
} u;
char ugbuf[2];
int line;
int errline;
const char *file;
int flags;
Area *areap;
XString xs;
Source *next;
};
#define SEOF 0
#define SFILE 1
#define SSTDIN 2
#define SSTRING 3
#define SWSTR 4
#define SWORDS 5
#define SWORDSEP 6
#define SALIAS 7
#define SREREAD 8
#define SF_ECHO BIT(0)
#define SF_ALIAS BIT(1)
#define SF_ALIASEND BIT(2)
#define SF_TTY BIT(3)
#define SBASE 0
#define SWORD 1
#ifdef KSH
#define SLETPAREN 2
#endif
#define SSQUOTE 3
#define SDQUOTE 4
#define SBRACE 5
#define SCSPAREN 6
#define SBQUOTE 7
#define SASPAREN 8
#define SHEREDELIM 9
#define SHEREDQUOTE 10
#define SPATTERN 11
#define STBRACE 12
typedef union {
int i;
char *cp;
char **wp;
struct op *o;
struct ioword *iop;
} YYSTYPE;
#define LWORD 256
#define LOGAND 257
#define LOGOR 258
#define BREAK 259
#define IF 260
#define THEN 261
#define ELSE 262
#define ELIF 263
#define FI 264
#define CASE 265
#define ESAC 266
#define FOR 267
#define SELECT 268
#define WHILE 269
#define UNTIL 270
#define DO 271
#define DONE 272
#define IN 273
#define FUNCTION 274
#define TIME 275
#define REDIR 276
#ifdef KSH
#define MDPAREN 277
#endif
#define BANG 278
#define DBRACKET 279
#define COPROC 280
#define YYERRCODE 300
#define CONTIN BIT(0)
#define ONEWORD BIT(1)
#define ALIAS BIT(2)
#define KEYWORD BIT(3)
#define LETEXPR BIT(4)
#define VARASN BIT(5)
#define ARRAYVAR BIT(6)
#define ESACONLY BIT(7)
#define CMDWORD BIT(8)
#define HEREDELIM BIT(9)
#define HERES 10
EXTERN Source *source;
EXTERN YYSTYPE yylval;
EXTERN struct ioword *heres [HERES], **herep;
EXTERN char ident [IDENT+1];
#ifdef HISTORY
# define HISTORYSIZE 128
EXTERN char **history;
EXTERN char **histptr;
EXTERN int histsize;
#endif