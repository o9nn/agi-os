#ifndef _OPENCOG_SOURCESET_H_
#define _OPENCOG_SOURCESET_H_
#include <vector>
#include <mutex>
#include <boost/operators.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include "../Rule.h"
#include "../UREConfig.h"
namespace opencog
{
class Source : public boost::totally_ordered<Source>
{
public:
Source(const Handle& body,
const Handle& vardecl=Handle::UNDEFINED,
double complexity=0.0,
double complexity_factor=1.0);
bool operator==(const Source& other) const;
bool operator<(const Source& other) const;
bool insert_rule(RulePtr rule);
void set_exhausted();
void reset_exhausted();
bool is_exhausted() const;
void set_rule_exhausted(const RulePtr& rule);
bool is_rule_exhausted(const RulePtr& rule) const;
double expand_complexity(double prob) const;
double get_weight() const;
std::string to_string(const std::string& indent=empty_string) const;
const Handle body;
const Handle vardecl;
const double complexity;
const double complexity_factor;
const double weight;
bool exhausted;
RuleSet rules;
private:
mutable std::mutex _mutex;
};
typedef std::shared_ptr<Source> SourcePtr;
#define createSource std::make_shared<Source>
struct source_ptr_less
{
bool operator()(const SourcePtr& l, const SourcePtr& r) const;
};
class SourceSet
{
public:
SourceSet(const UREConfig& config,
const Handle& init_source,
const Handle& init_vardecl);
std::vector<double> get_weights() const;
void set_exhausted();
void reset_exhausted();
bool is_exhausted() const;
void insert(const HandleSet& products, const Source& src,
double prob, const std::string& msgprfx="");
size_t size() const;
bool empty() const;
std::string to_string(const std::string& indent=empty_string) const;
typedef std::vector<SourcePtr> Sources;
Sources sources;
bool exhausted;
private:
const UREConfig& _config;
mutable std::mutex _mutex;
};
std::string oc_to_string(const Source& source,
const std::string& indent=empty_string);
std::string oc_to_string(const SourceSet::Sources& sources,
const std::string& indent=empty_string);
std::string oc_to_string(const SourceSet& sources,
const std::string& indent=empty_string);
}
#endif