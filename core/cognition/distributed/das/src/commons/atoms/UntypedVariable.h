#pragma once
#include <string>
#include "Wildcard.h"
namespace atoms {
class UntypedVariable : public Wildcard {
public:
string name;
UntypedVariable(const string& name, bool is_toplevel = false);
UntypedVariable(const UntypedVariable& other);
virtual UntypedVariable& operator=(const UntypedVariable& other);
virtual bool operator==(const UntypedVariable& other);
virtual bool operator!=(const UntypedVariable& other);
virtual void validate() const override;
virtual string to_string() const;
virtual string handle() const;
virtual string metta_representation(HandleDecoder& decoder) const;
virtual bool match(const string& handle, Assignment& assignment, HandleDecoder& decoder) override;
virtual void tokenize(vector<string>& output);
virtual void untokenize(vector<string>& tokens);
};
}