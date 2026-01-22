#include "e.h"
double BeforeSub = 1.2;
double AfterSub = 0.2;
double Dvshift = 0.25;
double Dhshift = 0.025;
double Dh2shift = 0.05;
double Dheight = 0.25;
double Barv = 0.68;
double Barh = 0.05;
double Ubarv = 0.1;
double Ubarh = 0.05;
char *IRspace = "\\^";
double Fatshift = 0.05;
int Funnyps = 5;
double Funnyht = 0.2;
double Funnybase = 0.3;
int Intps = 4;
double Intht = 1.15;
double Intbase = 0.3;
double Int1h = 0.4;
double Int1v = 0.2;
double Int2h = 0.05;
double Int2v = 0.1;
char *Matspace = "\\ \\ ";
double Overgap = 0.3;
double Overwid = 0.5;
double Overline = 0.1;
double Parenbase = 0.4;
double Parenshift = 0.13;
double Parenheight = 0.3;
double Pilegap = 0.4;
double Pilebase = 0.5;
double Subbase = 0.2;
double Supshift = 0.4;
char *Sub1space = "\\|";
char *Sup1space = "\\|";
char *Sub2space = "\\^";
char *SS1space = "\\^";
char *SS2space = "\\^";
struct tune {
char *name;
char *cval;
} tune[] ={
"vec_def", "\\f1\\v'-.5m'\\s-3\\(->\\s0\\v'.5m'\\fP",
"dyad_def", "\\f1\\v'-.5m'\\s-3\\z\\(<-\\|\\(->\\s0\\v'.5m'\\fP",
"hat_def", "\\f1\\v'-.05m'\\s+1^\\s0\\v'.05m'\\fP",
"tilde_def", "\\f1\\v'-.05m'\\s+1~\\s0\\v'.05m'\\fP",
"dot_def", "\\f1\\v'-.67m'.\\v'.67m'\\fP",
"dotdot_def", "\\f1\\v'-.67m'..\\v'.67m'\\fP",
"utilde_def", "\\f1\\v'1.0m'\\s+2~\\s-2\\v'-1.0m'\\fP",
"sum_def", "\\|\\v'.3m'\\s+5\\(*S\\s-5\\v'-.3m'\\|",
"union_def", "\\|\\v'.3m'\\s+5\\(cu\\s-5\\v'-.3m'\\|",
"inter_def", "\\|\\v'.3m'\\s+5\\(ca\\s-5\\v'-.3m'\\|",
"prod_def", "\\|\\v'.3m'\\s+5\\(*P\\s-5\\v'-.3m'\\|",
"int_def", "\\v'.1m'\\s+4\\(is\\s-4\\v'-.1m'",
0, 0
};
tbl *ftunetbl[TBLSIZE];
char *ftunes[] ={
"Subbase",
"Supshift",
0
};
void init_tune(void)
{
int i;
for (i = 0; tune[i].name != NULL; i++)
install(deftbl, tune[i].name, tune[i].cval, 0);
for (i = 0; ftunes[i] != NULL; i++)
install(ftunetbl, ftunes[i], (char *) 0, 0);
}
#define eq(s, t) (strcmp(s,t) == 0)
void ftune(char *s, char *t)
{
double dummy;
double f = atof(t);
double *target;
while (*t == ' ' || *t == '\t')
t++;
if (eq(s, "Subbase"))
target = &Subbase;
else if (eq(s, "Supshift"))
target = &Supshift;
else
target = &dummy;
if (t[0] == '+' || t[0] == '-')
*target += f;
else
*target = f;
}