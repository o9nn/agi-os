#ifndef _OPENCOG_METTA_AST_H
#define _OPENCOG_METTA_AST_H
#include <opencog/atoms/foreign/SexprAST.h>
#include <opencog/atoms/metta-types/atom_types.h>
namespace opencog
{
class MettaAST : public SexprAST
{
void init();
static std::string prt_metta(const Handle&);
protected:
virtual Handle next_expr(const std::string&, size_t& l, size_t& r);
public:
MettaAST(const HandleSeq&&, Type = METTA_AST);
MettaAST(const HandleSeq&&, const std::string&&);
MettaAST(const MettaAST&) = delete;
MettaAST& operator=(const MettaAST&) = delete;
MettaAST(const std::string&);
virtual std::string to_string(const std::string& indent) const;
virtual std::string to_short_string(const std::string& indent) const;
static Handle factory(const Handle&);
};
LINK_PTR_DECL(MettaAST)
#define createMettaAST CREATE_DECL(MettaAST)
}
#endif