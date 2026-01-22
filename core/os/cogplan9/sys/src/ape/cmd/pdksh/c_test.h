enum Test_op {
TO_NONOP = 0,
TO_STNZE, TO_STZER, TO_OPTION,
TO_FILAXST,
TO_FILEXST,
TO_FILREG, TO_FILBDEV, TO_FILCDEV, TO_FILSYM, TO_FILFIFO, TO_FILSOCK,
TO_FILCDF, TO_FILID, TO_FILGID, TO_FILSETG, TO_FILSTCK, TO_FILUID,
TO_FILRD, TO_FILGZ, TO_FILTT, TO_FILSETU, TO_FILWR, TO_FILEX,
TO_STEQL, TO_STNEQ, TO_STLT, TO_STGT, TO_INTEQ, TO_INTNE, TO_INTGT,
TO_INTGE, TO_INTLT, TO_INTLE, TO_FILEQ, TO_FILNT, TO_FILOT
};
typedef enum Test_op Test_op;
enum Test_meta {
TM_OR,
TM_AND,
TM_NOT,
TM_OPAREN,
TM_CPAREN,
TM_UNOP,
TM_BINOP,
TM_END
};
typedef enum Test_meta Test_meta;
#define TEF_ERROR BIT(0)
#define TEF_DBRACKET BIT(1)
typedef struct test_env Test_env;
struct test_env {
int flags;
union {
char **wp;
XPtrV *av;
} pos;
char **wp_end;
int (*isa) ARGS((Test_env *te, Test_meta meta));
const char *(*getopnd) ARGS((Test_env *te, Test_op op, int do_eval));
int (*eval) ARGS((Test_env *te, Test_op op, const char *opnd1,
const char *opnd2, int do_eval));
void (*error) ARGS((Test_env *te, int offset, const char *msg));
};
Test_op test_isop ARGS((Test_env *te, Test_meta meta, const char *s));
int test_eval ARGS((Test_env *te, Test_op op, const char *opnd1,
const char *opnd2, int do_eval));
int test_parse ARGS((Test_env *te));