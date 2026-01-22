#ifndef _MOSES_TYPES_H
#define _MOSES_TYPES_H
#include <cfloat>
#include <functional>
#include <iomanip>
#include <unordered_set>
#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/iterator/indirect_iterator.hpp>
#include <boost/operators.hpp>
#include <boost/ptr_container/ptr_set.hpp>
#include <opencog/util/empty_string.h>
#include <opencog/asmoses/utils/functional.h>
#include <opencog/asmoses/utils/iostreamContainer.h>
#include <opencog/asmoses/combo/combo/combo.h>
#include "complexity.h"
#include <opencog/asmoses/combo/combo/combo.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/core/NumberNode.h>
namespace opencog { namespace moses {
using combo::vertex;
using boost::indirect_iterator;
using boost::transform_iterator;
typedef float score_t;
static const int io_score_precision = 18;
static const score_t very_best_score = std::numeric_limits<score_t>::max();
static const score_t very_worst_score = std::numeric_limits<score_t>::lowest();
static const score_t epsilon_score = FLT_EPSILON;
struct composite_score:
public boost::less_than_comparable<composite_score>,
public boost::equality_comparable<composite_score>
{
composite_score(score_t scor, complexity_t cpxy,
score_t complexity_penalty_=0.0,
score_t uniformity_penalty_=0.0)
: multiply_diversity(false), score(scor), complexity(cpxy),
complexity_penalty(complexity_penalty_),
uniformity_penalty(uniformity_penalty_)
{
update_penalized_score();
}
composite_score();
composite_score& operator=(const composite_score &r);
score_t get_score() const { return score; }
complexity_t get_complexity() const { return complexity; }
score_t get_penalized_score() const { return penalized_score; }
Handle as_handle() const {
HandleSeq seq;
seq.push_back(multiply_diversity? createLink(TRUE_LINK) : createLink(FALSE_LINK));
seq.push_back(createNode(NUMBER_NODE, std::to_string(score)));
seq.push_back(createNode(NUMBER_NODE, std::to_string(complexity)));
seq.push_back(createNode(NUMBER_NODE, std::to_string(complexity_penalty)));
seq.push_back(createNode(NUMBER_NODE, std::to_string(uniformity_penalty)));
return createLink(seq, LIST_LINK);
}
void set_score(score_t sc)
{
score = sc;
update_penalized_score();
}
score_t get_complexity_penalty() const { return complexity_penalty; }
void set_complexity_penalty(score_t penalty)
{
complexity_penalty = penalty;
update_penalized_score();
}
score_t get_uniformity_penalty() const { return uniformity_penalty; }
void set_uniformity_penalty(score_t penalty)
{
uniformity_penalty = penalty;
update_penalized_score();
}
score_t get_penalty() const
{
return complexity_penalty + uniformity_penalty;
}
bool operator<(const composite_score &r) const;
bool operator==(const composite_score &r) const;
bool multiply_diversity;
protected:
score_t score;
complexity_t complexity;
score_t complexity_penalty;
score_t uniformity_penalty;
score_t penalized_score;
void update_penalized_score()
{
penalized_score = score - complexity_penalty;
if (multiply_diversity)
penalized_score *= uniformity_penalty;
else
penalized_score -= uniformity_penalty;
}
};
extern const composite_score worst_composite_score;
struct demeID_t : public std::string
{
demeID_t(unsigned expansion=0 );
demeID_t(unsigned expansion, unsigned breadth_first);
demeID_t(unsigned expansion, unsigned breadth_first, unsigned ss_deme);
Handle as_handle() const{
return createNode(CONCEPT_NODE, *this);
}
};
struct behavioral_score : public std::vector<score_t>
{
behavioral_score() {}
behavioral_score(size_t sz) : std::vector<score_t>(sz) {}
behavioral_score(size_t sz, score_t val) : std::vector<score_t>(sz, val) {}
behavioral_score(std::initializer_list<score_t> il)
: std::vector<score_t>(il) {}
std::vector<score_t> operator-=(const std::vector<score_t>& rhs)
{
size_t sz = rhs.size();
OC_ASSERT(size() == sz,
"Error: Incompatible behavioral_score sizes, this=%zu rhs=%zu",
size(), sz);
for (size_t i=0; i<sz; i++) {
(*this)[i] -= rhs[i];
}
return *this;
}
Handle as_handle() const {
HandleSeq scores;
for (size_t i=0; i<size(); i++) {
Handle h = createNode(NUMBER_NODE, std::to_string((*this)[i]));
scores.push_back(h);
}
return createLink(scores, LIST_LINK);
}
};
static inline behavioral_score operator-(const behavioral_score& lhs,
const behavioral_score& rhs)
{
size_t sz = rhs.size();
OC_ASSERT(lhs.size() == sz,
"Error: Incompatible behavioral_score sizes, lhs=%zu rhs=%zu",
lhs.size(), sz);
behavioral_score bs;
for (size_t i=0; i<sz; i++) {
bs.push_back(lhs[i] - rhs[i]);
}
return bs;
}
class scored_combo_tree : public boost::equality_comparable<scored_combo_tree>
{
public:
scored_combo_tree(combo::combo_tree tr,
demeID_t id=demeID_t(),
composite_score cs=composite_score(),
behavioral_score bs=behavioral_score())
: _tree(tr), _deme_id(id), _cscore(cs), _bscore(bs), _weight(1.0)
{}
private:
combo::combo_tree _tree;
demeID_t _deme_id;
composite_score _cscore;
behavioral_score _bscore;
double _weight;
public:
const combo::combo_tree& get_tree() const { return _tree; }
combo::combo_tree& get_tree() { return _tree; }
const demeID_t get_demeID() const { return _deme_id; }
demeID_t get_demeID() { return _deme_id; }
const behavioral_score& get_bscore() const
{
return _bscore;
}
void set_bscore(const behavioral_score& bs)
{
_bscore = bs;
}
double get_weight() const
{
return _weight;
}
void set_weight(double w)
{
_weight = w;
}
const composite_score& get_composite_score() const
{
return _cscore;
}
composite_score& get_composite_score()
{
return _cscore;
}
score_t get_score() const { return _cscore.get_score(); }
complexity_t get_complexity() const { return _cscore.get_complexity(); }
score_t get_penalized_score() const { return _cscore.get_penalized_score(); }
score_t get_complexity_penalty() const { return _cscore.get_complexity_penalty(); }
score_t get_uniformity_penalty() const { return _cscore.get_uniformity_penalty(); }
score_t get_penalty() const { return _cscore.get_penalty(); }
bool operator==(const scored_combo_tree& r) const;
};
class scored_atomese : public boost::equality_comparable<scored_atomese>
{
public:
scored_atomese(const Handle &h,
demeID_t id=demeID_t(),
composite_score cs=composite_score(),
behavioral_score bs=behavioral_score())
: _atomese(h), _deme_id(id), _cscore(cs), _bscore(bs), _weight(1.0)
{}
private:
Handle _atomese;
demeID_t _deme_id;
composite_score _cscore;
behavioral_score _bscore;
double _weight;
public:
const Handle& get_handle() const { return _atomese; }
Handle& get_handle() { return _atomese; }
Handle as_scored_handle() const{
HandleSeq seq {_atomese, _deme_id.as_handle(), _cscore.as_handle(),
_bscore.as_handle(), createNode(NUMBER_NODE, std::to_string(_weight))};
return createLink(seq, LIST_LINK);
}
const demeID_t get_demeID() const { return _deme_id; }
demeID_t get_demeID() { return _deme_id; }
const behavioral_score& get_bscore() const
{
return _bscore;
}
void set_bscore(const behavioral_score& bs)
{
_bscore = bs;
}
double get_weight() const
{
return _weight;
}
void set_weight(double w)
{
_weight = w;
}
const composite_score& get_composite_score() const
{
return _cscore;
}
composite_score& get_composite_score()
{
return _cscore;
}
score_t get_score() const { return _cscore.get_score(); }
complexity_t get_complexity() const { return _cscore.get_complexity(); }
score_t get_penalized_score() const { return _cscore.get_penalized_score(); }
score_t get_complexity_penalty() const { return _cscore.get_complexity_penalty(); }
score_t get_uniformity_penalty() const { return _cscore.get_uniformity_penalty(); }
score_t get_penalty() const { return _cscore.get_penalty(); }
bool operator==(const scored_atomese& r) const;
};
struct sct_score_greater
{
bool operator()(const scored_combo_tree&,
const scored_combo_tree&) const;
};
struct sct_tree_greater
{
bool operator()(const scored_combo_tree&,
const scored_combo_tree&) const;
};
struct scored_combo_tree_hash
{
size_t operator()(const scored_combo_tree&) const;
};
struct scored_combo_tree_equal
{
bool operator()(const scored_combo_tree&,
const scored_combo_tree&) const;
};
struct sa_score_greater
{
bool operator()(const scored_atomese&,
const scored_atomese&) const;
};
struct scored_atomese_hash
{
size_t operator()(const scored_atomese&) const;
};
struct scored_atomese_equal
{
bool operator()(const scored_atomese&,
const scored_atomese&) const;
};
typedef std::unordered_set<scored_combo_tree,
scored_combo_tree_hash,
scored_combo_tree_equal> scored_combo_tree_set;
typedef std::unordered_set<scored_atomese,
scored_atomese_hash,
scored_atomese_equal> scored_atomese_set;
typedef boost::ptr_set<scored_combo_tree,
sct_tree_greater> scored_combo_tree_tset;
typedef boost::ptr_set<scored_combo_tree,
sct_score_greater> scored_combo_tree_ptr_set;
typedef scored_combo_tree_ptr_set::iterator scored_combo_tree_ptr_set_it;
typedef scored_combo_tree_ptr_set::const_iterator scored_combo_tree_ptr_set_cit;
typedef boost::ptr_set<scored_atomese,
sa_score_greater> scored_atomese_ptr_set;
typedef scored_atomese_ptr_set::iterator scored_atomese_ptr_set_it;
typedef scored_atomese_ptr_set::const_iterator scored_atomese_ptr_set_cit;
std::ostream& ostream_behavioral_score(std::ostream& out, const behavioral_score&);
std::ostream& ostream_scored_combo_tree(std::ostream& out,
const scored_combo_tree&,
bool output_score=true,
bool output_cscore=true,
bool output_demeID=true,
bool output_bscore=true,
const combo::string_seq& labels=combo::string_seq(),
combo::output_format fmt
= combo::output_format::combo);
std::ostream& ostream_scored_atomese(std::ostream& out,
const scored_atomese& sa,
bool output_score=true,
bool output_cscore=true,
bool output_demeID=true,
bool output_bscore=true);
scored_combo_tree string_to_scored_combo_tree(const std::string& line);
std::istream& istream_scored_combo_trees(std::istream& in,
std::vector<scored_combo_tree>& scts);
inline std::ostream& operator<<(std::ostream& out,
const moses::scored_combo_tree& sct)
{
return moses::ostream_scored_combo_tree(out, sct);
}
inline std::ostream& operator<<(std::ostream& out,
const moses::scored_atomese& sa)
{
return moses::ostream_scored_atomese(out, sa);
}
inline std::ostream& operator<<(std::ostream& out,
const moses::composite_score& ts)
{
return out << "[score="
<< std::setprecision(moses::io_score_precision)
<< ts.get_score()
<< ", penalized score=" << ts.get_penalized_score()
<< ", complexity=" << ts.get_complexity()
<< ", complexity penalty=" << ts.get_complexity_penalty()
<< ", uniformity penalty=" << ts.get_uniformity_penalty()
<< "]";
}
inline std::ostream& operator<<(std::ostream& out,
const moses::behavioral_score& s)
{
return moses::ostream_behavioral_score(out, s);
}
}
std::string oc_to_string(const moses::composite_score& cs,
const std::string &indent=empty_string);
std::string oc_to_string(const moses::behavioral_score& bs,
const std::string &indent=empty_string);
std::string oc_to_string(const moses::scored_combo_tree &sct,
const std::string &indent=empty_string);
std::string oc_to_string(const moses::scored_atomese &sa,
const std::string &indent=empty_string);
std::string oc_to_string(const moses::scored_combo_tree_set& scts,
const std::string &indent=empty_string);
std::string oc_to_string(const moses::scored_atomese_set& sas,
const std::string &indent=empty_string);
std::string oc_to_string(const moses::scored_combo_tree_tset& sctts,
const std::string &indent=empty_string);
std::string oc_to_string(const moses::scored_combo_tree_ptr_set& sctps,
const std::string &indent=empty_string);
std::string oc_to_string(const moses::scored_atomese_ptr_set& saps,
const std::string &indent=empty_string);
}
#endif