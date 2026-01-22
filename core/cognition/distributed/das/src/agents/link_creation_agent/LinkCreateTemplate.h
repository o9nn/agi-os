#pragma once
#include <memory>
#include <string>
#include <tuple>
#include <variant>
#include <vector>
#include "Link.h"
#include "LinkSchema.h"
#include "Node.h"
#include "QueryAnswer.h"
#include "UntypedVariable.h"
using namespace std;
using namespace atoms;
using namespace query_engine;
namespace link_creation_agent {
class CustomField;
class LinkCreateTemplate;
using CustomFieldTypes = variant<string, shared_ptr<CustomField>>;
using LinkCreateTemplateTypes = variant<shared_ptr<LinkCreateTemplate>,
shared_ptr<Node>,
shared_ptr<UntypedVariable>,
shared_ptr<Link>,
shared_ptr<LinkSchema>>;
class CustomField {
public:
CustomField(const string& name);
CustomField(vector<string>& custom_fields);
~CustomField();
string get_name();
vector<tuple<string, CustomFieldTypes>> get_values();
string to_string();
vector<string> tokenize();
void add_field(const string& name, const CustomFieldTypes& value);
Properties to_properties();
CustomField untokenize(const vector<string>& tokens);
string to_metta_string();
private:
string name;
vector<tuple<string, CustomFieldTypes>> values;
CustomField untokenize(const vector<string>& tokens, size_t& cursor);
};
class LinkCreateTemplate {
public:
LinkCreateTemplate(const string& link_type);
LinkCreateTemplate(vector<string>& link_template);
~LinkCreateTemplate();
string get_link_type();
vector<LinkCreateTemplateTypes> get_targets();
vector<CustomField> get_custom_fields();
string to_string();
vector<string> tokenize();
void add_target(LinkCreateTemplateTypes target);
void add_custom_field(CustomField custom_field);
shared_ptr<Link> process_query_answer(shared_ptr<QueryAnswer> query_answer);
private:
string link_type;
vector<LinkCreateTemplateTypes> targets;
vector<CustomField> custom_fields = {};
};
class LinkCreateTemplateList {
public:
LinkCreateTemplateList(vector<string> link_template);
~LinkCreateTemplateList();
vector<LinkCreateTemplate> get_templates();
private:
vector<LinkCreateTemplate> templates;
};
}