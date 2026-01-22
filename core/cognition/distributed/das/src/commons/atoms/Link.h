#pragma once
#include <memory>
#include <string>
#include "Atom.h"
namespace atoms {
class Link : public Atom {
public:
vector<string> targets;
string metta_expression;
Link(const string& type,
const vector<string>& targets,
bool is_toplevel = false,
const Properties& custom_attributes = {},
const string& metta_expression = "");
Link(const string& type, const vector<string>& targets, const Properties& custom_attributes);
Link(vector<string>& tokens);
Link(const Link& other);
virtual Link& operator=(const Link& other);
virtual bool operator==(const Link& other);
virtual bool operator!=(const Link& other);
void validate() const;
virtual string to_string() const;
virtual string handle() const;
virtual string composite_type_hash(HandleDecoder& decoder) const;
virtual vector<string> composite_type(HandleDecoder& decoder) const;
virtual string metta_representation(HandleDecoder& decoder) const;
virtual unsigned int arity() const;
virtual bool match(const string& handle, Assignment& assignment, HandleDecoder& decoder) override;
virtual void tokenize(vector<string>& output);
virtual void untokenize(vector<string>& tokens);
};
}