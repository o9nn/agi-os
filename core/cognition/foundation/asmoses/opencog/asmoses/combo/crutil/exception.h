#ifndef _COMBOREDUCT_EXCEPTION_H
#define _COMBOREDUCT_EXCEPTION_H
#include <string>
#include <opencog/asmoses/combo/combo/vertex.h>
namespace opencog { namespace combo {
class ComboReductException
{
protected:
std::string _message;
public:
ComboReductException();
ComboReductException(std::string m = "");
std::string get_message() const;
};
class OverflowException : public ComboReductException
{
vertex _vertex;
public:
OverflowException();
OverflowException(vertex);
vertex get_vertex() const;
};
class EvalException : public ComboReductException
{
vertex _vertex;
public:
EvalException();
EvalException(vertex, std::string m = "");
vertex get_vertex() const;
};
class TypeCheckException : public ComboReductException
{
int _arg;
public:
TypeCheckException();
TypeCheckException(int arg = 0);
};
}}
#endif