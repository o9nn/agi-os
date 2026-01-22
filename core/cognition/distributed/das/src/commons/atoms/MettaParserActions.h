#pragma once
#include <stack>
#include "Atom.h"
#include "ParserActions.h"
using namespace std;
using namespace metta;
using namespace atoms;
namespace atoms {
enum ExpressionType { LINK, AND, OR };
class MettaParserActions : public ParserActions {
public:
MettaParserActions();
~MettaParserActions();
void symbol(const string& name) override;
void variable(const string& value) override;
void literal(const string& value) override;
void literal(int value) override;
void literal(float value) override;
void expression_begin() override;
void expression_end(bool toplevel, const string& metta_string) override;
stack<shared_ptr<Atom>> element_stack;
string metta_expression_handle;
map<string, shared_ptr<Atom>> handle_to_atom;
map<string, string> handle_to_metta_expression;
vector<string> metta_expressions;
private:
unsigned int current_expression_size;
ExpressionType current_expression_type;
stack<unsigned int> expression_size_stack;
stack<ExpressionType> expression_type_stack;
};
}