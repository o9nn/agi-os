#ifndef _OPENCOG_SEXPR_AST_H
#define _OPENCOG_SEXPR_AST_H
#include <opencog/atoms/foreign/ForeignAST.h>
namespace opencog
{
class SexprAST : public ForeignAST
{
void init();
static Handle get_next_expr(const std::string&, size_t& l, size_t& r);
protected:
void parse(const std::string&);
virtual Handle next_expr(const std::string&, size_t& l, size_t& r);
public:
SexprAST(Type);
SexprAST(const HandleSeq&&, Type = SEXPR_AST);
SexprAST(const SexprAST&) = delete;
SexprAST& operator=(const SexprAST&) = delete;
SexprAST(const std::string&);
virtual std::string to_string(const std::string& indent) const;
virtual std::string to_short_string(const std::string& indent) const;
static Handle factory(const Handle&);
};
LINK_PTR_DECL(SexprAST)
#define createSexprAST CREATE_DECL(SexprAST)
}
#endif