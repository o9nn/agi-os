#ifndef _OPENCOG_DATALOG_AST_H
#define _OPENCOG_DATALOG_AST_H
#include <opencog/atoms/foreign/ForeignAST.h>
namespace opencog
{
class DatalogAST : public ForeignAST
{
void init();
protected:
void parse(const std::string&);
public:
DatalogAST(const HandleSeq&&, Type = DATALOG_AST);
DatalogAST(const HandleSeq&&, const std::string&&);
DatalogAST(const DatalogAST&) = delete;
DatalogAST& operator=(const DatalogAST&) = delete;
DatalogAST(const std::string&);
virtual std::string to_string(const std::string& indent) const;
virtual std::string to_short_string(const std::string& indent) const;
static Handle factory(const Handle&);
};
LINK_PTR_DECL(DatalogAST)
#define createDatalogAST CREATE_DECL(DatalogAST)
}
#endif