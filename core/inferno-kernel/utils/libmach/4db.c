#include <lib9.h>
#include <bio.h>
#include "mach.h"
char	*mipsexcep(Map*, Rgetter);
int	mipsfoll(Map*, ulong, Rgetter, ulong*);
int	mipsinst(Map*, ulong, char, char*, int);
int	mipsdas(Map*, ulong, char*, int);
int	mipsinstlen(Map*, ulong);
Machdata mipsmach2be =
{
{0, 0, 0, 0xD},
4,
beswab,
beswal,
beswav,
risctrace,
riscframe,
mipsexcep,
0,
beieeesftos,
beieeedftos,
mipsfoll,
mipsinst,
mipsdas,
mipsinstlen,
};
Machdata mipsmach2le =
{
{0, 0, 0, 0xD},
4,
leswab,
leswal,
leswav,
risctrace,
riscframe,
mipsexcep,
0,
leieeesftos,
leieeedftos,
mipsfoll,
mipsinst,
mipsdas,
mipsinstlen,
};