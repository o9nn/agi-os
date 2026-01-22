#pragma once
#include <string>
#include "Atom.h"
namespace atoms {
class Wildcard : public Atom {
public:
Wildcard(const string& type, bool is_toplevel = false, const Properties& custom_attributes = {});
Wildcard(const string& type, const Properties& custom_attributes);
Wildcard(const Wildcard& other);
virtual Wildcard& operator=(const Wildcard& other);
virtual bool operator==(const Wildcard& other);
virtual bool operator!=(const Wildcard& other);
virtual void validate() const override;
virtual string to_string() const;
virtual string schema_handle() const;
virtual void tokenize(vector<string>& output);
virtual void untokenize(vector<string>& tokens);
virtual string handle() const = 0;
virtual string metta_representation(HandleDecoder& decoder) const = 0;
virtual bool match(const string& handle, Assignment& assignment, HandleDecoder& decoder) = 0;
};
}