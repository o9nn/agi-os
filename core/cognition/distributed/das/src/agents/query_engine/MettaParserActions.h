#pragma once
#include <stack>
#include "ParserActions.h"
#include "PatternMatchingQueryProxy.h"
#include "QueryElement.h"
using namespace std;
using namespace metta;
using namespace query_element;
namespace query_engine {
enum ExpressionType { LINK, LINK_TEMPLATE, AND, OR };
class MettaParserActions : public ParserActions {
public:
MettaParserActions(shared_ptr<PatternMatchingQueryProxy> proxy);
~MettaParserActions();
void symbol(const string& name) override;
void variable(const string& value) override;
void literal(const string& value) override;
void literal(int value) override;
void literal(float value) override;
void expression_begin();
void expression_end(bool toplevel, const string& metta_expression);
stack<shared_ptr<QueryElement>> element_stack;
private:
shared_ptr<PatternMatchingQueryProxy> proxy;
unsigned int current_expression_size;
ExpressionType current_expression_type;
stack<unsigned int> expression_size_stack;
stack<ExpressionType> expression_type_stack;
};
}