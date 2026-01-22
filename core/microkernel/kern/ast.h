#ifndef _KERN_AST_H_
#define _KERN_AST_H_
#include <kern/kern_types.h>
#include <kern/macros.h>
#include <machine/ast.h>
#define AST_ZILCH 0x0
#define AST_HALT 0x1
#define AST_TERMINATE 0x2
#define AST_BLOCK 0x4
#define AST_NETWORK 0x8
#define AST_NETIPC 0x10
#define AST_SCHEDULING (AST_HALT|AST_TERMINATE|AST_BLOCK)
#ifndef MACHINE_AST_PER_THREAD
#define MACHINE_AST_PER_THREAD 0
#endif
#define AST_PER_THREAD (AST_HALT | AST_TERMINATE | MACHINE_AST_PER_THREAD)
typedef unsigned long ast_t;
extern volatile ast_t need_ast[NCPUS];
#ifdef MACHINE_AST
#else
#define aston(mycpu)
#define astoff(mycpu)
#endif
extern void ast_taken(void);
#define ast_needed(mycpu) need_ast[mycpu]
#define ast_on(mycpu, reasons) \
MACRO_BEGIN \
if ((need_ast[mycpu] |= (reasons)) != AST_ZILCH) \
{ aston(mycpu); } \
MACRO_END
#define ast_off(mycpu, reasons) \
MACRO_BEGIN \
if ((need_ast[mycpu] &= ~(reasons)) == AST_ZILCH) \
{ astoff(mycpu); } \
MACRO_END
#define ast_propagate(thread, mycpu) ast_on((mycpu), (thread)->ast)
#define ast_context(thread, mycpu) \
MACRO_BEGIN \
if ((need_ast[mycpu] = \
(need_ast[mycpu] &~ AST_PER_THREAD) | (thread)->ast) \
!= AST_ZILCH) \
{ aston(mycpu); } \
else \
{ astoff(mycpu); } \
MACRO_END
#define thread_ast_set(thread, reason) (thread)->ast |= (reason)
#define thread_ast_clear(thread, reason) (thread)->ast &= ~(reason)
#define thread_ast_clear_all(thread) (thread)->ast = AST_ZILCH
extern void ast_init (void);
extern void ast_check (void);
#if NCPUS > 1
extern void init_ast_check(const processor_t processor);
extern void cause_ast_check(const processor_t processor);
#endif
#endif