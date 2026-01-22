#ifndef _DDB_DB_LEX_H_
#define _DDB_DB_LEX_H_
#define TOK_STRING_SIZE 64
#define DB_LEX_LINE_SIZE 256
struct db_lex_context {
int l_char;
int l_token;
char *l_ptr;
char *l_eptr;
};
extern int db_lex(void);
extern int db_read_line(const char *rep_str);
extern void db_flush_line(void);
extern int db_read_char(void);
extern void db_unread_char(int c);
extern int db_read_token(void);
extern void db_unread_token(int t);
extern void db_flush_lex(void);
extern void db_switch_input(char *, int);
extern void db_save_lex_context(struct db_lex_context *);
extern void db_restore_lex_context(const struct db_lex_context *);
extern void db_skip_to_eol(void);
extern db_expr_t db_tok_number;
extern char db_tok_string[TOK_STRING_SIZE];
extern db_expr_t db_radix;
#define tEOF (-1)
#define tEOL 1
#define tNUMBER 2
#define tIDENT 3
#define tPLUS 4
#define tMINUS 5
#define tDOT 6
#define tSTAR 7
#define tSLASH 8
#define tEQ 9
#define tLPAREN 10
#define tRPAREN 11
#define tPCT 12
#define tHASH 13
#define tCOMMA 14
#define tQUOTE 15
#define tDOLLAR 16
#define tEXCL 17
#define tSHIFT_L 18
#define tSHIFT_R 19
#define tDOTDOT 20
#define tSEMI_COLON 21
#define tLOG_EQ 22
#define tLOG_NOT_EQ 23
#define tLESS 24
#define tLESS_EQ 25
#define tGREATER 26
#define tGREATER_EQ 27
#define tBIT_AND 28
#define tBIT_OR 29
#define tLOG_AND 30
#define tLOG_OR 31
#define tSTRING 32
#define tQUESTION 33
#endif