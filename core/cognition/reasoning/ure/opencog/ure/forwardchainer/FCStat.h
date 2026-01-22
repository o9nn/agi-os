#ifndef _OPENCOG_FCSTAT_H_
#define _OPENCOG_FCSTAT_H_
#include <map>
#include <opencog/atoms/base/Handle.h>
#include <opencog/ure/Rule.h>
namespace opencog {
struct InferenceRecord
{
const Handle hsource;
const Rule& rule;
HandleSet product;
InferenceRecord(Handle h, const Rule& r, const HandleSet& p)
: hsource(h), rule(r), product(p) {}
};
class FCStat
{
public:
FCStat(AtomSpace* trace_as) : _trace_as(trace_as) {}
void add_inference_record(unsigned iteration, Handle source,
const Rule& rule, const HandleSet& product);
HandleSet get_all_products() const;
HandleSet get_all_products();
private:
std::vector<InferenceRecord> _inf_rec;
AtomSpace* _trace_as;
mutable std::mutex _whole_mutex;
};
}
#endif