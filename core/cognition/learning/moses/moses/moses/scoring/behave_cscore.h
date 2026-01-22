#ifndef _MOSES_BEHAVE_CSCORE_H
#define _MOSES_BEHAVE_CSCORE_H
#include <opencog/util/lru_cache.h>
#include "scoring_base.h"
namespace opencog { namespace moses {
class behave_cscore
{
public:
behave_cscore(bscore_base& b, size_t initial_cache_size=0);
behavioral_score get_bscore(const combo_tree&) const;
behavioral_score get_bscore(const scored_combo_tree_set&) const;
composite_score get_cscore(const combo_tree&);
composite_score get_cscore(const scored_combo_tree_set&);
score_t best_possible_score() const;
score_t worst_possible_score() const;
score_t min_improv() const
{
return _bscorer.min_improv();
}
void ignore_cols(const std::set<arity_t>& idxs) const
{
_bscorer.ignore_cols(idxs);
}
void ignore_rows(const std::set<unsigned>& idxs) const
{
_bscorer.ignore_rows(idxs);
}
void ignore_rows_at_times(const std::set<TTable::value_type>& timestamps) const
{
_bscorer.ignore_rows_at_times(timestamps);
}
unsigned get_ctable_usize() const
{
return _bscorer.get_ctable_usize();
}
const CTable& get_ctable() const {
return _bscorer.get_ctable();
}
private:
bscore_base& _bscorer;
struct wrapper
{
typedef combo_tree argument_type;
typedef composite_score result_type;
composite_score operator()(const combo_tree&) const;
behave_cscore* self;
};
bool _have_cache;
wrapper _wrapper;
prr_cache_threaded<wrapper> _cscore_cache;
composite_score get_cscore_nocache(const combo_tree&);
public:
bscore_base& get_bscorer() { return _bscorer; }
};
}
}
#endif