#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "awk.h"
#include "ytab.h"
struct xx
{	int token;
char *name;
char *pname;
} proc[] = {
{ PROGRAM, "program", NULL },
{ BOR, "boolop", " || " },
{ AND, "boolop", " && " },
{ NOT, "boolop", " !" },
{ NE, "relop", " != " },
{ EQ, "relop", " == " },
{ LE, "relop", " <= " },
{ LT, "relop", " < " },
{ GE, "relop", " >= " },
{ GT, "relop", " > " },
{ ARRAY, "array", NULL },
{ INDIRECT, "indirect", "$(" },
{ SUBSTR, "substr", "substr" },
{ SUB, "sub", "sub" },
{ GSUB, "gsub", "gsub" },
{ INDEX, "sindex", "sindex" },
{ SPRINTF, "awksprintf", "sprintf " },
{ ADD, "arith", " + " },
{ MINUS, "arith", " - " },
{ MULT, "arith", " * " },
{ DIVIDE, "arith", " / " },
{ MOD, "arith", " % " },
{ UMINUS, "arith", " -" },
{ POWER, "arith", " **" },
{ PREINCR, "incrdecr", "++" },
{ POSTINCR, "incrdecr", "++" },
{ PREDECR, "incrdecr", "--" },
{ POSTDECR, "incrdecr", "--" },
{ CAT, "cat", " " },
{ PASTAT, "pastat", NULL },
{ PASTAT2, "dopa2", NULL },
{ MATCH, "matchop", " ~ " },
{ NOTMATCH, "matchop", " !~ " },
{ MATCHFCN, "matchop", "matchop" },
{ INTEST, "intest", "intest" },
{ PRINTF, "awkprintf", "printf" },
{ PRINT, "printstat", "print" },
{ CLOSE, "closefile", "closefile" },
{ DELETE, "awkdelete", "awkdelete" },
{ SPLIT, "split", "split" },
{ ASSIGN, "assign", " = " },
{ ADDEQ, "assign", " += " },
{ SUBEQ, "assign", " -= " },
{ MULTEQ, "assign", " *= " },
{ DIVEQ, "assign", " /= " },
{ MODEQ, "assign", " %= " },
{ POWEQ, "assign", " ^= " },
{ CONDEXPR, "condexpr", " ?: " },
{ IF, "ifstat", "if(" },
{ WHILE, "whilestat", "while(" },
{ FOR, "forstat", "for(" },
{ DO, "dostat", "do" },
{ IN, "instat", "instat" },
{ NEXT, "jump", "next" },
{ NEXTFILE, "jump", "nextfile" },
{ EXIT, "jump", "exit" },
{ BREAK, "jump", "break" },
{ CONTINUE, "jump", "continue" },
{ RETURN, "jump", "ret" },
{ BLTIN, "bltin", "bltin" },
{ CALL, "call", "call" },
{ ARG, "arg", "arg" },
{ VARNF, "getnf", "NF" },
{ GETLINE, "getline", "getline" },
{ 0, "", "" },
};
#define SIZE	(LASTTOKEN - FIRSTTOKEN + 1)
char *table[SIZE];
char *names[SIZE];
int main(int argc, char *argv[])
{
struct xx *p;
int i, n, tok;
char c;
FILE *fp;
char buf[200], name[200], def[200];
printf("#include <stdio.h>\n");
printf("#include \"awk.h\"\n");
printf("#include \"ytab.h\"\n\n");
for (i = SIZE; --i >= 0; )
names[i] = "";
if ((fp = fopen("ytab.h", "r")) == NULL) {
fprintf(stderr, "maketab can't open ytab.h!\n");
exit(1);
}
printf("static char *printname[%d] = {\n", SIZE);
i = 0;
while (fgets(buf, sizeof buf, fp) != NULL) {
n = sscanf(buf, "%1c %s %s %d", &c, def, name, &tok);
if (c != '#' || (n != 4 && strcmp(def,"define") != 0))
continue;
if (tok < FIRSTTOKEN || tok > LASTTOKEN) {
fprintf(stderr, "maketab funny token %d %s ignored\n", tok, buf);
continue;
}
names[tok-FIRSTTOKEN] = (char *) malloc(strlen(name)+1);
strcpy(names[tok-FIRSTTOKEN], name);
printf("\t(char *) \"%s\",\t\n", name, tok);
i++;
}
printf("};\n\n");
for (p=proc; p->token!=0; p++)
table[p->token-FIRSTTOKEN] = p->name;
printf("\nCell *(*proctab[%d])(Node **, int) = {\n", SIZE);
for (i=0; i<SIZE; i++)
if (table[i]==0)
printf("\tnullproc,\t\n", names[i]);
else
printf("\t%s,\t\n", table[i], names[i]);
printf("};\n\n");
printf("char *tokname(int n)\n");
printf("{\n");
printf("	static char buf[100];\n\n");
printf("	if (n < FIRSTTOKEN || n > LASTTOKEN) {\n");
printf("		sprintf(buf, \"token %%d\", n);\n");
printf("		return buf;\n");
printf("	}\n");
printf("	return printname[n-FIRSTTOKEN];\n");
printf("}\n");
return 0;
}