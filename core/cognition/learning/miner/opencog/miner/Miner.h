#ifndef OPENCOG_MINER_H_
#define OPENCOG_MINER_H_
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/core/Variables.h>
#include <opencog/atoms/core/RewriteLink.h>
#include <opencog/atomspace/AtomSpace.h>
#include "HandleTree.h"
#include "Valuations.h"
#include "MinerUtils.h"
class MinerUTest;
namespace opencog
{
struct MinerParameters {
MinerParameters(unsigned minsup=1,
unsigned conjuncts=1,
const Handle& initpat=Handle::UNDEFINED,
int maxdepth=-1);
unsigned minsup;
unsigned initconjuncts;
Handle initpat;
int maxdepth;
};
class Miner
{
friend class ::MinerUTest;
public:
Miner(const MinerParameters& param=MinerParameters());
HandleTree operator()(const AtomSpace& db_as);
HandleTree operator()(const HandleSeq& db);
HandleTree specialize(const Handle& pattern,
const HandleSeq& db,
int maxdepth=-1);
HandleTree specialize(const Handle& pattern,
const HandleSeq& db,
const Valuations& valuations,
int maxdepth);
HandleTree specialize_alt(const Handle& pattern,
const HandleSeq& db,
const Valuations& valuations,
int maxdepth);
MinerParameters param;
private:
mutable AtomSpacePtr tmp_as;
bool terminate(const Handle& pattern,
const HandleSeq& db,
const Valuations& valuations,
int maxdepth) const;
HandleTree specialize_shabs(const Handle& pattern,
const HandleSeq& db,
const Valuations& valuations,
int maxdepth);
HandleTree specialize_shapat(const Handle& pattern,
const HandleSeq& db,
const Handle& var,
const Handle& shapat,
int maxdepth);
bool enough_support(const Handle& pattern,
const HandleSeq& db) const;
unsigned support(const Handle& pattern,
const HandleSeq& db,
unsigned ms) const;
unsigned freq(const std::vector<unsigned>& freqs) const;
HandleSeq filter_db(const Handle& pattern,
const HandleSeq& db) const;
bool match(const Handle& pattern, const Handle& dt) const;
Handle matched_results(const Handle& pattern, const Handle& dt) const;
};
}
#endif