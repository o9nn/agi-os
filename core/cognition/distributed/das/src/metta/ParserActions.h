#pragma once
#include <memory>
#include <stack>
#include "Token.h"
using namespace std;
namespace metta {
class ParserActions {
public:
ParserActions();
virtual ~ParserActions();
virtual void symbol(const string& name);
virtual void variable(const string& value);
virtual void literal(const string& value);
virtual void literal(int value);
virtual void literal(float value);
virtual void expression_begin();
virtual void expression_end(bool toplevel, const string& metta_string);
};
}