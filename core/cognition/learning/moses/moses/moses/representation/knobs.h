#ifndef _MOSES_KNOBS_H
#define _MOSES_KNOBS_H
#include <bitset>
#include <string>
#include <vector>
#include <opencog/util/based_variant.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/numeric.h>
#include <opencog/util/tree.h>
#include <moses/comboreduct/combo/iostream_combo.h>
#include "../moses/complexity.h"
#include "field_set.h"
namespace opencog { namespace moses {
using namespace combo;
struct knob_base
{
knob_base(combo_tree& tr, combo_tree::iterator loc)
: _tr(tr), _loc(loc) {}
knob_base(combo_tree& tr) : _tr(tr), _loc(tr.end()) {}
virtual ~knob_base() { }
virtual bool in_exemplar() const = 0;
virtual void clear_exemplar() = 0;
combo_tree::iterator get_loc() const
{
return _loc;
}
virtual std::string toStr() const = 0;
protected:
combo_tree& _tr;
combo_tree::iterator _loc;
};
struct disc_knob_base : public knob_base
{
disc_knob_base(combo_tree& tr, combo_tree::iterator tgt)
: knob_base(tr, tgt) {}
disc_knob_base(combo_tree& tr)
: knob_base(tr) {}
virtual ~disc_knob_base() {}
virtual void turn(int) = 0;
virtual void disallow(int) = 0;
virtual void allow(int) = 0;
virtual combo_tree::iterator append_to(combo_tree& candidate,
combo_tree::iterator& parent_dst,
int idx) const = 0;
virtual field_set::disc_spec spec() const = 0;
virtual int multiplicity() const = 0;
virtual complexity_t complexity_bound() const = 0;
};
struct contin_knob : public knob_base
{
contin_knob(combo_tree& tr, combo_tree::iterator tgt,
contin_t step_size, contin_t expansion,
field_set::width_t depth);
bool in_exemplar() const;
void clear_exemplar();
void turn(contin_t x);
void append_to(combo_tree& candidate, combo_tree::iterator parent_dst,
contin_t c) const;
const field_set::contin_spec& spec() const;
std::string toStr() const;
protected:
field_set::contin_spec _spec;
};
template<int Multiplicity>
struct discrete_knob : public disc_knob_base
{
discrete_knob(combo_tree& tr, combo_tree::iterator tgt)
: disc_knob_base(tr, tgt), _default(0), _current(0) {}
discrete_knob(combo_tree& tr)
: disc_knob_base(tr), _default(0), _current(0) {}
void disallow(int setting) {
_disallowed[setting] = true;
}
void allow(int setting) {
_disallowed[setting] = false;
}
int multiplicity() const {
return Multiplicity -_disallowed.count();
}
bool in_exemplar() const {
return (_default != 0);
}
protected:
std::bitset<Multiplicity> _disallowed;
int _default;
int _current;
int map_idx(int idx) const
{
if (idx == _default)
idx = 0;
else if (idx == 0)
idx = _default;
return idx + (_disallowed << (Multiplicity - idx)).count();
}
};
struct logical_subtree_knob : public discrete_knob<3>
{
static const int absent = 0;
static const int present = 1;
static const int negated = 2;
static const std::map<int, std::string> pos_str;
logical_subtree_knob(combo_tree& tr, combo_tree::iterator tgt,
const logical_subtree_knob& lsk);
logical_subtree_knob(combo_tree& tr, combo_tree::iterator tgt,
combo_tree::iterator subtree);
complexity_t complexity_bound() const;
void clear_exemplar();
void turn(int idx);
combo_tree::iterator append_to(combo_tree& candidate,
combo_tree::iterator& parent_dst,
int idx) const;
field_set::disc_spec spec() const;
std::string toStr() const;
private:
std::string locStr(bool negated = false) const;
std::string posStr(int pos, bool tag_current = false) const;
};
#define MAX_PERM_ACTIONS 128
struct action_subtree_knob : public discrete_knob<MAX_PERM_ACTIONS>
{
typedef combo_tree::pre_order_iterator pre_it;
action_subtree_knob(combo_tree& tr, combo_tree::iterator tgt,
const std::vector<combo_tree>& perms);
complexity_t complexity_bound() const;
void clear_exemplar();
void turn(int idx);
combo_tree::iterator append_to(combo_tree& candidate,
combo_tree::iterator& parent_dst,
int idx) const;
field_set::disc_spec spec() const;
std::string toStr() const;
protected:
const std::vector<combo_tree> _perms;
};
struct simple_action_subtree_knob : public discrete_knob<2>
{
static const int present = 0;
static const int absent = 1;
simple_action_subtree_knob(combo_tree& tr, combo_tree::iterator tgt);
complexity_t complexity_bound() const;
void clear_exemplar();
void turn(int idx);
combo_tree::iterator append_to(combo_tree& candidate,
combo_tree::iterator& parent_dst,
int idx) const;
field_set::disc_spec spec() const;
std::string toStr() const;
private:
std::string locStr() const;
};
typedef based_variant <boost::variant<logical_subtree_knob,
action_subtree_knob,
simple_action_subtree_knob>,
disc_knob_base> disc_knob;
inline std::ostream& operator<<(std::ostream& out,
const opencog::moses::knob_base& s)
{
return out << s.toStr();
}
}
}
#endif