#pragma once
#include <string>
#include "Assignment.h"
#include "HandleTrie.h"
#include "Properties.h"
#include "Utils.h"
#include "expression_hasher.h"
using namespace commons;
namespace atoms {
class HandleDecoder;
class Atom : public HandleTrie::TrieValue {
protected:
static string UNDEFINED_TYPE;
Atom() {}
public:
static string WILDCARD_STRING;
static string WILDCARD_HANDLE;
string type;
bool is_toplevel;
Properties custom_attributes;
Atom(const string& type, bool is_toplevel = false, const Properties& custom_attributes = {});
Atom(const Atom& other);
virtual void validate() const;
virtual ~Atom() override = default;
virtual Atom& operator=(const Atom& other);
virtual bool operator==(const Atom& other);
virtual bool operator!=(const Atom& other);
static bool is_node(const Atom& atom) { return atom.arity() == 0; }
static bool is_link(const Atom& atom) { return atom.arity() > 0; }
virtual string to_string() const;
virtual string named_type_hash() const;
virtual vector<string> composite_type(HandleDecoder& decoder) const;
virtual string composite_type_hash(HandleDecoder& decoder) const;
virtual string schema_handle() const;
virtual unsigned int arity() const;
virtual void tokenize(vector<string>& output);
virtual void untokenize(vector<string>& tokens);
virtual string handle() const = 0;
virtual string metta_representation(HandleDecoder& decoder) const = 0;
virtual bool match(const string& handle, Assignment& assignment, HandleDecoder& decoder) = 0;
virtual void merge(HandleTrie::TrieValue* other) override {}
};
}