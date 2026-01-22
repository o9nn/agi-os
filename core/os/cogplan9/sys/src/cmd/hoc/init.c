#include <u.h>
#include <libc.h>
#include "hoc.h"
#include "y.tab.h"
static struct {
char *name;
int kval;
} keywords[] = {
"proc", PROC,
"func", FUNC,
"return", RETURN,
"if", IF,
"else", ELSE,
"while", WHILE,
"for", FOR,
"print", PRINT,
"read", READ,
0, 0,
};
static struct {
char *name;
double cval;
} consts[] = {
"PI", 3.14159265358979323846,
"E", 2.71828182845904523536,
"GAMMA", 0.57721566490153286060,
"DEG", 57.29577951308232087680,
"PHI", 1.61803398874989484820,
0, 0
};
static struct {
char *name;
double (*func)(double);
} builtins[] = {
"sin", sin,
"cos", cos,
"tan", tan,
"atan", atan,
"asin", Asin,
"acos", Acos,
"sinh", Sinh,
"cosh", Cosh,
"tanh", tanh,
"log", Log,
"log10", Log10,
"exp", Exp,
"sqrt", Sqrt,
"int", integer,
"abs", fabs,
0, 0
};
void
init(void)
{
int i;
Symbol *s;
for (i = 0; keywords[i].name; i++)
install(keywords[i].name, keywords[i].kval, 0.0);
for (i = 0; consts[i].name; i++)
install(consts[i].name, VAR, consts[i].cval);
for (i = 0; builtins[i].name; i++) {
s = install(builtins[i].name, BLTIN, 0.0);
s->u.ptr = builtins[i].func;
}
}