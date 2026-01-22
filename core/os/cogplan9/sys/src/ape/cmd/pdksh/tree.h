#define NOBLOCK ((struct op *)NULL)
#define NOWORD ((char *)NULL)
#define NOWORDS ((char **)NULL)
struct op {
short type;
union {
short evalflags;
short ksh_func;
} u;
char **args;
char **vars;
struct ioword **ioact;
struct op *left, *right;
char *str;
int lineno;
};
#define TEOF 0
#define TCOM 1
#define TPAREN 2
#define TPIPE 3
#define TLIST 4
#define TOR 5
#define TAND 6
#define TBANG 7
#define TDBRACKET 8
#define TFOR 9
#define TSELECT 10
#define TCASE 11
#define TIF 12
#define TWHILE 13
#define TUNTIL 14
#define TELIF 15
#define TPAT 16
#define TBRACE 17
#define TASYNC 18
#define TFUNCT 19
#define TTIME 20
#define TEXEC 21
#define TCOPROC 22
#define EOS 0
#define CHAR 1
#define QCHAR 2
#define COMSUB 3
#define EXPRSUB 4
#define OQUOTE 5
#define CQUOTE 6
#define OSUBST 7
#define CSUBST 8
#define OPAT 9
#define SPAT 10
#define CPAT 11
struct ioword {
int unit;
int flag;
char *name;
char *delim;
char *heredoc;
};
#define IOTYPE 0xF
#define IOREAD 0x1
#define IOWRITE 0x2
#define IORDWR 0x3
#define IOHERE 0x4
#define IOCAT 0x5
#define IODUP 0x6
#define IOEVAL BIT(4)
#define IOSKIP BIT(5)
#define IOCLOB BIT(6)
#define IORDUP BIT(7)
#define IONAMEXP BIT(8)
#define XEXEC BIT(0)
#define XFORK BIT(1)
#define XBGND BIT(2)
#define XPIPEI BIT(3)
#define XPIPEO BIT(4)
#define XPIPE (XPIPEI|XPIPEO)
#define XXCOM BIT(5)
#define XPCLOSE BIT(6)
#define XCCLOSE BIT(7)
#define XERROK BIT(8)
#define XCOPROC BIT(9)
#define XTIME BIT(10)
#define XINTACT BIT(11)
#define DOBLANK BIT(0)
#define DOGLOB BIT(1)
#define DOPAT BIT(2)
#define DOTILDE BIT(3)
#define DONTRUNCOMMAND BIT(4)
#define DOASNTILDE BIT(5)
#define DOBRACE_ BIT(6)
#define DOMAGIC_ BIT(7)
#define DOTEMP_ BIT(8)
#define DOVACHECK BIT(9)
#define DOMARKDIRS BIT(10)
#define DB_NORM 1
#define DB_OR 2
#define DB_AND 3
#define DB_BE 4
#define DB_PAT 5