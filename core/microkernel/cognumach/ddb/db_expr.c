#if MACH_KDB
#include <mach/boolean.h>
#include <machine/db_machdep.h>
#include <ddb/db_lex.h>
#include <ddb/db_access.h>
#include <ddb/db_command.h>
#include <ddb/db_expr.h>
#include <ddb/db_output.h>
#include <ddb/db_sym.h>
#include <ddb/db_variables.h>
#include <kern/task.h>
static boolean_t
db_term(db_expr_t *valuep)
{
int	t;
switch(t = db_read_token()) {
case tIDENT:
if (!db_value_of_name(db_tok_string, valuep)) {
db_printf("Symbol \"%s\" not found\n", db_tok_string);
db_error(0);
}
return (TRUE);
case tNUMBER:
*valuep = db_tok_number;
return (TRUE);
case tDOT:
*valuep = (db_expr_t)db_dot;
return (TRUE);
case tDOTDOT:
*valuep = (db_expr_t)db_prev;
return (TRUE);
case tPLUS:
*valuep = (db_expr_t) db_next;
return (TRUE);
case tQUOTE:
*valuep = (db_expr_t)db_last_addr;
return (TRUE);
case tDOLLAR:
if (!db_get_variable(valuep))
return (FALSE);
return (TRUE);
case tLPAREN:
if (!db_expression(valuep)) {
db_error("Unmached ()s\n");
}
t = db_read_token();
if (t != tRPAREN) {
db_printf("')' expected at \"%s...\"\n", db_tok_string);
db_error(0);
}
return (TRUE);
default:
db_unread_token(t);
return (FALSE);
}
}
int
db_size_option(const char *modif, boolean_t *u_option, boolean_t *t_option)
{
const char 	*p;
int		size = sizeof(int);
*u_option = FALSE;
*t_option = FALSE;
for (p = modif; *p; p++) {
switch(*p) {
case 'b':
size = sizeof(char);
break;
case 'h':
size = sizeof(short);
break;
case 'l':
size = sizeof(long);
break;
case 'u':
*u_option = TRUE;
break;
case 't':
*t_option = TRUE;
break;
}
}
return(size);
}
static boolean_t
db_unary(db_expr_t *valuep)
{
int	  t;
int	  size;
boolean_t u_opt, t_opt;
task_t	  task;
extern	  task_t db_default_task;
t = db_read_token();
if (t == tMINUS) {
if (!db_unary(valuep)) {
db_error("Expression syntax error after '-'\n");
}
*valuep = -*valuep;
return (TRUE);
}
if (t == tSTAR) {
if (!db_unary(valuep)) {
db_error("Expression syntax error after '*'\n");
}
task = TASK_NULL;
size = sizeof(db_addr_t);
u_opt = FALSE;
t = db_read_token();
if (t == tIDENT && db_tok_string[0] == ':') {
size = db_size_option(&db_tok_string[1], &u_opt, &t_opt);
if (t_opt)
task = db_default_task;
} else
db_unread_token(t);
*valuep = db_get_task_value((db_addr_t)*valuep, size, !u_opt, task);
return (TRUE);
}
if (t == tEXCL) {
if (!db_unary(valuep)) {
db_error("Expression syntax error after '!'\n");
}
*valuep = (!(*valuep));
return (TRUE);
}
db_unread_token(t);
return (db_term(valuep));
}
static boolean_t
db_mult_expr(db_expr_t *valuep)
{
db_expr_t	lhs = 0, rhs;
int		t;
char		c;
if (!db_unary(&lhs))
return (FALSE);
t = db_read_token();
while (t == tSTAR || t == tSLASH || t == tPCT || t == tHASH
|| t == tBIT_AND) {
c = db_tok_string[0];
if (!db_term(&rhs)) {
db_printf("Expression syntax error after '%c'\n", c);
db_error(0);
}
switch(t) {
case tSTAR:
lhs *= rhs;
break;
case tBIT_AND:
lhs &= rhs;
break;
default:
if (rhs == 0) {
db_error("Divide by 0\n");
}
if (t == tSLASH)
lhs /= rhs;
else if (t == tPCT)
lhs %= rhs;
else
lhs = ((lhs+rhs-1)/rhs)*rhs;
}
t = db_read_token();
}
db_unread_token(t);
*valuep = lhs;
return (TRUE);
}
static boolean_t
db_add_expr(db_expr_t *valuep)
{
db_expr_t	lhs, rhs;
int		t;
char		c;
if (!db_mult_expr(&lhs))
return (FALSE);
t = db_read_token();
while (t == tPLUS || t == tMINUS || t == tBIT_OR) {
c = db_tok_string[0];
if (!db_mult_expr(&rhs)) {
db_printf("Expression syntax error after '%c'\n", c);
db_error(0);
}
if (t == tPLUS)
lhs += rhs;
else if (t == tMINUS)
lhs -= rhs;
else
lhs |= rhs;
t = db_read_token();
}
db_unread_token(t);
*valuep = lhs;
return (TRUE);
}
static boolean_t
db_shift_expr(db_expr_t *valuep)
{
db_expr_t	lhs, rhs;
int		t;
if (!db_add_expr(&lhs))
return (FALSE);
t = db_read_token();
while (t == tSHIFT_L || t == tSHIFT_R) {
if (!db_add_expr(&rhs)) {
db_printf("Expression syntax error after \"%s\"\n",
(t == tSHIFT_L)? "<<": ">>");
db_error(0);
}
if (rhs < 0) {
db_error("Negative shift amount\n");
}
if (t == tSHIFT_L)
lhs <<= rhs;
else {
lhs = (natural_t) lhs >> rhs;
}
t = db_read_token();
}
db_unread_token(t);
*valuep = lhs;
return (TRUE);
}
static boolean_t
db_logical_relation_expr(db_expr_t *valuep)
{
db_expr_t	lhs, rhs;
int		t;
char		op[3];
if (!db_shift_expr(&lhs))
return(FALSE);
t = db_read_token();
while (t == tLOG_EQ || t == tLOG_NOT_EQ
|| t == tGREATER || t == tGREATER_EQ
|| t == tLESS || t == tLESS_EQ) {
op[0] = db_tok_string[0];
op[1] = db_tok_string[1];
op[2] = 0;
if (!db_shift_expr(&rhs)) {
db_printf("Expression syntax error after \"%s\"\n", op);
db_error(0);
}
switch(t) {
case tLOG_EQ:
lhs = (lhs == rhs);
break;
case tLOG_NOT_EQ:
lhs = (lhs != rhs);
break;
case tGREATER:
lhs = (lhs > rhs);
break;
case tGREATER_EQ:
lhs = (lhs >= rhs);
break;
case tLESS:
lhs = (lhs < rhs);
break;
case tLESS_EQ:
lhs = (lhs <= rhs);
break;
}
t = db_read_token();
}
db_unread_token(t);
*valuep = lhs;
return (TRUE);
}
static boolean_t
db_logical_and_expr(db_expr_t *valuep)
{
db_expr_t	lhs, rhs;
int		t;
if (!db_logical_relation_expr(&lhs))
return(FALSE);
t = db_read_token();
while (t == tLOG_AND) {
if (!db_logical_relation_expr(&rhs)) {
db_error("Expression syntax error after \"&&\"\n");
}
lhs = (lhs && rhs);
}
db_unread_token(t);
*valuep = lhs;
return (TRUE);
}
static boolean_t
db_logical_or_expr(db_expr_t *valuep)
{
db_expr_t	lhs, rhs;
int		t;
if (!db_logical_and_expr(&lhs))
return(FALSE);
t = db_read_token();
while (t == tLOG_OR) {
if (!db_logical_and_expr(&rhs)) {
db_error("Expression syntax error after \"||\"\n");
}
lhs = (lhs || rhs);
}
db_unread_token(t);
*valuep = lhs;
return (TRUE);
}
int
db_expression(db_expr_t *valuep)
{
return (db_logical_or_expr(valuep));
}
#endif