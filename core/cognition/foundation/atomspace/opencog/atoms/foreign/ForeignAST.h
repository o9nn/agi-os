#ifndef _OPENCOG_FOREIGN_AST_H
#define _OPENCOG_FOREIGN_AST_H
#include <opencog/atoms/base/Link.h>
namespace opencog
{
class ForeignAST : public Link
{
protected:
std::string _name;
virtual ContentHash compute_hash() const;
public:
ForeignAST(const HandleSeq&&, Type = FOREIGN_AST);
ForeignAST(Type);
ForeignAST(Type, const std::string&);
ForeignAST(const ForeignAST&) = delete;
ForeignAST& operator=(const ForeignAST&) = delete;
virtual const std::string& get_name() const { return _name; }
virtual bool operator==(const Atom&) const;
};
LINK_PTR_DECL(ForeignAST)
template< class... Args >
Handle createForeignAST( Args&&... args )
{
Handle tmp(std::make_shared<ForeignAST>(std::forward<Args>(args) ...));
return classserver().factory(tmp);
}
}
#endif