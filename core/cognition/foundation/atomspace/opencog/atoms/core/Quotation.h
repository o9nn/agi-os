#ifndef _OPENCOG_QUOTATION_H
#define _OPENCOG_QUOTATION_H
#include <string>
#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
namespace opencog
{
class Quotation
{
int _quotation_level;
bool _local_quote;
public:
explicit Quotation(int ql=0, bool lq=false);
int level() const;
bool is_locally_quoted() const;
bool is_quoted() const;
bool is_unquoted() const;
static bool is_quotation_type(Type t);
bool consumable(Type t) const;
void update(Type t);
bool operator<(const Quotation& quotation) const;
bool operator==(const Quotation& quotation) const;
std::string to_string(const std::string& indent) const;
};
bool unquoted_below(const Handle&);
bool unquoted_below(const HandleSeq&);
std::string oc_to_string(const Quotation& quotation,
const std::string& indent=empty_string);
}
#endif