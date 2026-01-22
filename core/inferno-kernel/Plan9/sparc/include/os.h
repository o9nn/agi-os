typedef struct FPU FPU;
struct FPU
{
ulong   fsr;
};
extern Proc *getup();
#define up (getup())
#define BIGEND