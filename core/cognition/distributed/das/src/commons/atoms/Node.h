#pragma once
#include "Atom.h"
using namespace commons;
namespace atoms {
class Node : public Atom {
public:
string name;
Node(const string& type,
const string& name,
bool is_toplevel = false,
const Properties& custom_attributes = {});
Node(const string& type, const string& name, const Properties& custom_attributes);
Node(vector<string>& tokens);
Node(const Node& other);
virtual void validate() const override;
virtual Node& operator=(const Node& other);
virtual bool operator==(const Node& other);
virtual bool operator!=(const Node& other);
virtual string to_string() const override;
virtual string handle() const override;
virtual string metta_representation(HandleDecoder& decoder) const override;
virtual bool match(const string& handle, Assignment& assignment, HandleDecoder& decoder) override;
virtual void tokenize(vector<string>& output);
virtual void untokenize(vector<string>& tokens);
};
}