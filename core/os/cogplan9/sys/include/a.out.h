typedef struct Exec Exec;
struct Exec
{
long magic;
long text;
long data;
long bss;
long syms;
long entry;
long spsz;
long pcsz;
};
#define HDR_MAGIC 0x00008000
#define _MAGIC(f, b) ((f)|((((4*(b))+0)*(b))+7))
#define A_MAGIC _MAGIC(0, 8)
#define I_MAGIC _MAGIC(0, 11)
#define J_MAGIC _MAGIC(0, 12)
#define K_MAGIC _MAGIC(0, 13)
#define V_MAGIC _MAGIC(0, 16)
#define X_MAGIC _MAGIC(0, 17)
#define M_MAGIC _MAGIC(0, 18)
#define D_MAGIC _MAGIC(0, 19)
#define E_MAGIC _MAGIC(0, 20)
#define Q_MAGIC _MAGIC(0, 21)
#define N_MAGIC _MAGIC(0, 22)
#define L_MAGIC _MAGIC(0, 23)
#define P_MAGIC _MAGIC(0, 24)
#define U_MAGIC _MAGIC(0, 25)
#define S_MAGIC _MAGIC(HDR_MAGIC, 26)
#define T_MAGIC _MAGIC(HDR_MAGIC, 27)
#define R_MAGIC _MAGIC(HDR_MAGIC, 28)
#define MIN_MAGIC 8
#define MAX_MAGIC 28
#define DYN_MAGIC 0x80000000
typedef struct Sym Sym;
struct Sym
{
vlong value;
uint sig;
char type;
char *name;
};