#include "tl.h"
extern int	tl_yylex(void);
extern int	tl_verbose, tl_errs;
int	tl_yychar = 0;
YYSTYPE	tl_yylval;
static Node	*tl_formula(void);
static Node	*tl_factor(void);
static Node	*tl_level(int);
static int	prec[2][4] = {
{ U_OPER,  V_OPER, 0, 0 },
{ OR, AND, IMPLIES, EQUIV, },
};
static Node *
tl_factor(void)
{	Node *ptr = ZN;
switch (tl_yychar) {
case '(':
ptr = tl_formula();
if (tl_yychar != ')')
tl_yyerror("expected ')'");
tl_yychar = tl_yylex();
break;
case NOT:
ptr = tl_yylval;
tl_yychar = tl_yylex();
ptr->lft = tl_factor();
ptr = push_negation(ptr);
break;
case ALWAYS:
tl_yychar = tl_yylex();
ptr = tl_factor();
#ifndef NO_OPT
if (ptr->ntyp == FALSE
||  ptr->ntyp == TRUE)
break;
if (ptr->ntyp == V_OPER)
{	if (ptr->lft->ntyp == FALSE)
break;
ptr = ptr->rgt;
}
#endif
ptr = tl_nn(V_OPER, False, ptr);
break;
#ifdef NXT
case NEXT:
tl_yychar = tl_yylex();
ptr = tl_factor();
if (ptr->ntyp == TRUE)
break;
ptr = tl_nn(NEXT, ptr, ZN);
break;
#endif
case EVENTUALLY:
tl_yychar = tl_yylex();
ptr = tl_factor();
#ifndef NO_OPT
if (ptr->ntyp == TRUE
||  ptr->ntyp == FALSE)
break;
if (ptr->ntyp == U_OPER
&&  ptr->lft->ntyp == TRUE)
break;
if (ptr->ntyp == U_OPER)
{
ptr = ptr->rgt;
}
#endif
ptr = tl_nn(U_OPER, True, ptr);
break;
case PREDICATE:
ptr = tl_yylval;
tl_yychar = tl_yylex();
break;
case TRUE:
case FALSE:
ptr = tl_yylval;
tl_yychar = tl_yylex();
break;
}
if (!ptr) tl_yyerror("expected predicate");
#if 0
printf("factor:	");
tl_explain(ptr->ntyp);
printf("\n");
#endif
return ptr;
}
static Node *
bin_simpler(Node *ptr)
{	Node *a, *b;
if (ptr)
switch (ptr->ntyp) {
case U_OPER:
#ifndef NO_OPT
if (ptr->rgt->ntyp == TRUE
||  ptr->rgt->ntyp == FALSE
||  ptr->lft->ntyp == FALSE)
{	ptr = ptr->rgt;
break;
}
if (isequal(ptr->lft, ptr->rgt))
{
ptr = ptr->rgt;
break;
}
if (ptr->lft->ntyp == U_OPER
&&  isequal(ptr->lft->lft, ptr->rgt))
{
ptr->lft = ptr->lft->rgt;
break;
}
if (ptr->rgt->ntyp == U_OPER
&&  ptr->rgt->lft->ntyp == TRUE)
{
ptr = ptr->rgt;
break;
}
#ifdef NXT
if (ptr->rgt->ntyp == NEXT
&&  ptr->lft->ntyp == NEXT)
{	ptr = tl_nn(NEXT,
tl_nn(U_OPER,
ptr->lft->lft,
ptr->rgt->lft), ZN);
}
#endif
#endif
break;
case V_OPER:
#ifndef NO_OPT
if (ptr->rgt->ntyp == FALSE
||  ptr->rgt->ntyp == TRUE
||  ptr->lft->ntyp == TRUE)
{	ptr = ptr->rgt;
break;
}
if (isequal(ptr->lft, ptr->rgt))
{
ptr = ptr->rgt;
break;
}
if (ptr->lft->ntyp == FALSE
&&  ptr->rgt->ntyp == V_OPER)
{	ptr->rgt = ptr->rgt->rgt;
break;
}
if (ptr->rgt->ntyp == V_OPER
&&  ptr->rgt->lft->ntyp == FALSE)
{	ptr->lft = False;
ptr->rgt = ptr->rgt->rgt;
break;
}
#endif
break;
case IMPLIES:
#ifndef NO_OPT
if (isequal(ptr->lft, ptr->rgt))
{	ptr = True;
break;
}
#endif
ptr = tl_nn(OR, Not(ptr->lft), ptr->rgt);
ptr = rewrite(ptr);
break;
case EQUIV:
#ifndef NO_OPT
if (isequal(ptr->lft, ptr->rgt))
{	ptr = True;
break;
}
#endif
a = rewrite(tl_nn(AND,
dupnode(ptr->lft),
dupnode(ptr->rgt)));
b = rewrite(tl_nn(AND,
Not(ptr->lft),
Not(ptr->rgt)));
ptr = tl_nn(OR, a, b);
ptr = rewrite(ptr);
break;
case AND:
#ifndef NO_OPT
if (ptr->rgt->ntyp == U_OPER
&&  isequal(ptr->rgt->rgt, ptr->lft))
{	ptr = ptr->lft;
break;
}
if (ptr->lft->ntyp == U_OPER
&&  isequal(ptr->lft->rgt, ptr->rgt))
{	ptr = ptr->rgt;
break;
}
if (ptr->rgt->ntyp == V_OPER
&&  isequal(ptr->rgt->rgt, ptr->lft))
{	ptr = ptr->rgt;
break;
}
if (ptr->lft->ntyp == V_OPER
&&  isequal(ptr->lft->rgt, ptr->rgt))
{	ptr = ptr->lft;
break;
}
if (ptr->rgt->ntyp == U_OPER
&&  ptr->lft->ntyp == U_OPER
&&  isequal(ptr->rgt->rgt, ptr->lft->rgt))
{	ptr = tl_nn(U_OPER,
tl_nn(AND, ptr->lft->lft, ptr->rgt->lft),
ptr->lft->rgt);
break;
}
if (ptr->rgt->ntyp == V_OPER
&&  ptr->lft->ntyp == V_OPER
&&  isequal(ptr->rgt->lft, ptr->lft->lft))
{	ptr = tl_nn(V_OPER,
ptr->rgt->lft,
tl_nn(AND, ptr->lft->rgt, ptr->rgt->rgt));
break;
}
#ifdef NXT
if (ptr->rgt->ntyp == NEXT
&&  ptr->lft->ntyp == NEXT)
{	ptr = tl_nn(NEXT,
tl_nn(AND,
ptr->rgt->lft,
ptr->lft->lft), ZN);
break;
}
#endif
if (isequal(ptr->lft, ptr->rgt)
||  ptr->rgt->ntyp == FALSE
||  ptr->lft->ntyp == TRUE)
{	ptr = ptr->rgt;
break;
}
if (ptr->rgt->ntyp == TRUE
||  ptr->lft->ntyp == FALSE)
{	ptr = ptr->lft;
break;
}
if (ptr->rgt->ntyp == U_OPER
&&  ptr->lft->ntyp == V_OPER
&&  isequal(ptr->lft->rgt, ptr->rgt->rgt))
{	ptr = ptr->lft;
break;
}
#endif
break;
case OR:
#ifndef NO_OPT
if (ptr->rgt->ntyp == U_OPER
&&  isequal(ptr->rgt->rgt, ptr->lft))
{	ptr = ptr->rgt;
break;
}
if (ptr->rgt->ntyp == V_OPER
&&  isequal(ptr->rgt->rgt, ptr->lft))
{	ptr = ptr->lft;
break;
}
if (ptr->rgt->ntyp == U_OPER
&&  ptr->lft->ntyp == U_OPER
&&  isequal(ptr->rgt->lft, ptr->lft->lft))
{	ptr = tl_nn(U_OPER,
ptr->rgt->lft,
tl_nn(OR, ptr->lft->rgt, ptr->rgt->rgt));
break;
}
if (isequal(ptr->lft, ptr->rgt)
||  ptr->rgt->ntyp == FALSE
||  ptr->lft->ntyp == TRUE)
{	ptr = ptr->lft;
break;
}
if (ptr->rgt->ntyp == TRUE
||  ptr->lft->ntyp == FALSE)
{	ptr = ptr->rgt;
break;
}
if (ptr->rgt->ntyp == V_OPER
&&  ptr->lft->ntyp == V_OPER
&&  isequal(ptr->lft->rgt, ptr->rgt->rgt))
{	ptr = tl_nn(V_OPER,
tl_nn(OR, ptr->lft->lft, ptr->rgt->lft),
ptr->rgt->rgt);
break;
}
if (ptr->rgt->ntyp == U_OPER
&&  ptr->lft->ntyp == V_OPER
&&  isequal(ptr->lft->rgt, ptr->rgt->rgt))
{	ptr = ptr->rgt;
break;
}
#endif
break;
}
return ptr;
}
static Node *
tl_level(int nr)
{	int i; Node *ptr = ZN;
if (nr < 0)
return tl_factor();
ptr = tl_level(nr-1);
again:
for (i = 0; i < 4; i++)
if (tl_yychar == prec[nr][i])
{	tl_yychar = tl_yylex();
ptr = tl_nn(prec[nr][i],
ptr, tl_level(nr-1));
ptr = bin_simpler(ptr);
goto again;
}
if (!ptr) tl_yyerror("syntax error");
#if 0
printf("level %d:	", nr);
tl_explain(ptr->ntyp);
printf("\n");
#endif
return ptr;
}
static Node *
tl_formula(void)
{	tl_yychar = tl_yylex();
return tl_level(1);
}
void
tl_parse(void)
{	Node *n;
n = tl_formula();
if (tl_verbose)
{	printf("formula: ");
dump(n);
printf("\n");
}
if (tl_Getchar() != -1)
{	tl_yyerror("syntax error");
tl_errs++;
return;
}
trans(n);
}