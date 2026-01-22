#ifndef _REP_FIELD_SET_H
#define _REP_FIELD_SET_H
#include <map>
#include <boost/operators.hpp>
#include <boost/variant.hpp>
#include <opencog/util/dorepeat.h>
#include <opencog/util/Logger.h>
#include <opencog/util/mt19937ar.h>
#include <opencog/util/numeric.h>
#include <opencog/util/oc_assert.h>
#include <opencog/util/RandGen.h>
#include <opencog/util/Counter.h>
#include <opencog/util/empty_string.h>
#include "instance.h"
namespace opencog {
namespace moses {
struct field_set
{
typedef unsigned multiplicity_t;
typedef unsigned width_t;
typedef unsigned breadth_t;
typedef unsigned depth_t;
typedef std::size_t size_t;
struct bit_iterator;
struct const_bit_iterator;
struct disc_iterator;
struct const_disc_iterator;
struct contin_iterator;
struct const_contin_iterator;
struct const_term_iterator;
struct field
{
field() { }
field(width_t w, size_t ma, size_t mi)
: width(w), major_offset(ma), minor_offset(mi) { }
width_t width;
size_t major_offset, minor_offset;
};
typedef std::vector<field>::const_iterator field_iterator;
struct disc_spec
{
disc_spec(multiplicity_t a) : multy(a) { }
multiplicity_t multy;
bool operator<(const disc_spec& rhs) const {
return multy > rhs.multy;
}
bool operator==(const disc_spec& rhs) const {
return multy == rhs.multy;
}
};
struct contin_spec
{
contin_spec(contin_t m, contin_t ss, contin_t ex, depth_t d)
: mean(m), step_size(ss), expansion(ex), depth(d) { }
contin_t mean, step_size, expansion;
depth_t depth;
bool operator<(const contin_spec& rhs) const
{
return (depth > rhs.depth
|| (depth == rhs.depth
&& (expansion > rhs.expansion
|| (expansion == rhs.expansion
&& (step_size > rhs.step_size
|| (step_size == rhs.step_size
&& mean > rhs.mean))))));
}
bool operator==(const contin_spec& rhs) const
{
return (mean == rhs.mean &&
step_size == rhs.step_size &&
expansion == rhs.expansion &&
depth == rhs.depth);
}
contin_t epsilon() const
{
return step_size / contin_t(1UL << depth);
}
static const disc_t Stop;
static const disc_t Left;
static const disc_t Right;
static disc_t switchLR(disc_t lr)
{
if (lr == Left)
return Right;
else if (lr == Right)
return Left;
else {
OC_ASSERT(false);
return disc_t();
}
}
};
struct contin_stepper
{
contin_stepper(const contin_spec& c_)
: c(c_), value(c.mean),
_all_left(true), _all_right(true), _step_size(c.step_size) { }
const contin_spec& c;
contin_t value;
void left()
{
if (_all_left) {
value -= _step_size;
_step_size *= c.expansion;
_all_right = false;
} else {
if (_all_right) {
_all_right = false;
_step_size /= (c.expansion * 2);
}
value -= _step_size;
_step_size /= 2;
}
}
void right()
{
if (_all_right) {
value += _step_size;
_step_size *= c.expansion;
_all_left = false;
} else {
if (_all_left) {
_all_left = false;
_step_size /= (c.expansion * 2);
}
value += _step_size;
_step_size /= 2;
}
}
protected:
bool _all_left;
bool _all_right;
contin_t _step_size;
};
struct term_spec
{
term_spec(const term_tree& t)
: tr(&t), depth(t.max_depth(t.begin())),
branching(next_power_of_two(1 + t.max_branching(t.begin()))) { }
bool operator<(const term_spec& rhs) const {
return (depth*branching > rhs.depth*rhs.branching);
}
const term_tree* tr;
size_t depth, branching;
bool operator==(const term_spec& rhs) const {
return (depth == rhs.depth && branching == rhs.branching && *tr == *(rhs.tr));
}
static const disc_t Stop;
static disc_t to_child_idx(disc_t d) {
return d -1;
}
static disc_t from_child_idx(disc_t d) {
return d + 1;
}
};
typedef boost::variant<term_spec, contin_spec, disc_spec> spec;
field_set() : _nbool(0)
{
compute_starts();
}
field_set(const field_set& x)
: _fields(x._fields), _term(x._term), _contin(x._contin),
_disc(x._disc), _nbool(x._nbool)
{
compute_starts();
}
field_set(const spec& s, size_t n) : _nbool(0)
{
build_spec(s, n);
compute_starts();
}
template<typename It>
field_set(It from, It to) : _nbool(0)
{
Counter<spec, size_t> spec_counts(from, to);
for (const auto& v : spec_counts)
build_spec(v.first, v.second);
compute_starts();
}
field_set& operator=(const field_set&);
bool operator==(const field_set&) const;
size_t packed_width() const {
return _fields.empty() ? 0 : _fields.back().major_offset + 1;
}
bool empty() const {
return _fields.empty();
}
size_t raw_size() const {
return _fields.size();
}
size_t byte_size() const
{
size_t sz = sizeof(field_set);
sz += _fields.size() * sizeof(field);
sz += _term.size() * sizeof(term_spec);
sz += _contin.size() * sizeof(contin_spec);
sz += _disc.size() * sizeof(disc_spec);
sz += _contin_raw_offsets.size() * sizeof(size_t);
return sz;
}
size_t dim_size() const {
return n_bits() + n_disc_fields() + contin().size() + term().size();
}
size_t count(const instance& inst) const
{
return raw_size() - std::count(begin_raw(inst), end_raw(inst), 0);
}
const std::vector<disc_spec>& disc_and_bit() const
{
return _disc;
}
const std::vector<contin_spec>& contin() const
{
return _contin;
}
const std::vector<term_spec>& term() const
{
return _term;
}
disc_t get_raw(const instance& inst, size_t idx) const
{
const field& f = _fields[idx];
return ((inst[f.major_offset] >> f.minor_offset) & ((packed_t(1) << f.width) - 1UL));
}
void set_raw(instance& inst, size_t idx, disc_t v) const
{
const field& f = _fields[idx];
inst[f.major_offset] ^= ((inst[f.major_offset] ^
(packed_t(v) << f.minor_offset)) &
(((packed_t(1) << f.width) - 1UL) << f.minor_offset));
}
const term_t& get_term(const instance& inst, size_t idx) const;
contin_t get_contin(const instance& inst, size_t idx) const;
void set_contin(instance& inst, size_t idx, contin_t v) const;
template<typename It, typename Out>
Out pack(It from, Out out) const;
std::string to_string(const instance&) const;
std::string to_string_raw(const instance&) const;
int hamming_distance(const instance& inst1, const instance& inst2) const
{
OC_ASSERT(inst1.size() == inst1.size());
int d = 0;
for (const_disc_iterator it1 = begin_raw(inst1), it2 = begin_raw(inst2);
it1 != end_raw(inst1); ++it1, ++it2)
d += (*it1 != *it2);
return d;
}
void merge_instance(instance& target,
const instance& base,
const instance& reference) const
{
OC_ASSERT(base.size() == reference.size() and
base.size() == target.size());
disc_iterator tit = begin_raw(target);
for (const_disc_iterator bit = begin_raw(base),
rit = begin_raw(reference);
bit != end_raw(base); ++bit, ++rit, ++tit)
{
if (*bit != *rit) *tit = *rit;
}
}
field_iterator begin_term_fields() const {
return _fields.begin();
}
field_iterator end_term_fields() const {
return _contin_start;
}
field_iterator begin_contin_fields() const {
return _contin_start;
}
field_iterator end_contin_fields() const {
return _disc_start;
}
field_iterator begin_disc_fields() const {
return _disc_start;
}
field_iterator end_disc_fields() const {
return _fields.end() - _nbool;
}
field_iterator begin_bit_fields() const {
return _fields.end() - _nbool;
}
field_iterator end_bit_fields() const {
return _fields.end();
}
field_iterator begin_fields() const {
return _fields.begin();
}
field_iterator end_fields() const {
return _fields.end();
}
size_t begin_term_raw_idx() const {
return 0;
}
size_t end_term_raw_idx() const {
return _end_term_raw_idx;
}
size_t begin_contin_raw_idx() const {
return _begin_contin_raw_idx;
}
size_t end_contin_raw_idx() const {
return _end_contin_raw_idx;
}
size_t begin_disc_raw_idx() const {
return _begin_disc_raw_idx;
}
size_t end_disc_raw_idx() const {
return _end_disc_raw_idx;
}
size_t begin_bit_raw_idx() const {
return _begin_bit_raw_idx;
}
size_t end_bit_raw_idx() const {
return _end_bit_raw_idx;
}
/
size_t contin_length(const instance& inst, size_t idx) const
{
size_t raw_begin = contin_to_raw_idx(idx);
size_t raw_end = raw_begin + _contin[idx].depth;
size_t current = raw_begin;
while (current != raw_end) {
if (get_raw(inst, current) != contin_spec::Stop)
++current;
else break;
}
return current - raw_begin;
}
protected:
std::vector<field> _fields;
std::vector<term_spec> _term;
std::vector<contin_spec> _contin;
std::vector<disc_spec> _disc;
size_t _nbool;
std::vector<size_t> _contin_raw_offsets;
field_iterator _contin_start, _disc_start;
size_t _end_term_raw_idx;
size_t _begin_contin_raw_idx;
size_t _end_contin_raw_idx;
size_t _begin_disc_raw_idx;
size_t _end_disc_raw_idx;
size_t _begin_bit_raw_idx;
size_t _end_bit_raw_idx;
size_t _n_disc_fields;
size_t _n_contin_fields;
size_t _n_term_fields;
void compute_starts()
{
_contin_start = _fields.begin();
for (const term_spec& o : _term)
_contin_start += o.depth;
_disc_start = _contin_start;
for (const contin_spec& c : _contin)
_disc_start += c.depth;
field_iterator term_start = _fields.begin();
_end_term_raw_idx = distance(term_start, end_term_fields());
_begin_contin_raw_idx = distance(term_start, begin_contin_fields());
_end_contin_raw_idx = distance(term_start, end_contin_fields());
_begin_disc_raw_idx = distance(term_start, begin_disc_fields());
_end_disc_raw_idx = distance(term_start, end_disc_fields());
_begin_bit_raw_idx = distance(term_start, begin_bit_fields());
_end_bit_raw_idx = distance(term_start, end_bit_fields());
_n_disc_fields = distance(begin_disc_fields(), end_disc_fields());
_n_contin_fields = distance(begin_contin_fields(), end_contin_fields());
_n_term_fields = distance(begin_term_fields(), end_term_fields());
_contin_raw_offsets.reserve(_contin.size());
size_t raw_idx = begin_contin_raw_idx();
for (const contin_spec& c : _contin) {
_contin_raw_offsets.push_back(raw_idx);
raw_idx += c.depth;
}
}
size_t back_offset() const
{
return _fields.empty() ? 0 :
_fields.back().major_offset*bits_per_packed_t +
_fields.back().minor_offset + _fields.back().width;
}
void build_spec(const spec& s, size_t n);
void build_term_spec(const term_spec& os, size_t n);
void build_contin_spec(const contin_spec& cs, size_t n);
void build_disc_spec(const disc_spec& ds, size_t n);
template<typename Self, typename Iterator>
struct bit_iterator_base
: boost::random_access_iterator_helper<Self, bool>
{
typedef std::ptrdiff_t Distance;
Self& operator++()
{
_mask <<= 1;
if (!_mask) {
_mask = packed_t(1);
++_it;
}
return (*((Self*)this));
}
Self& operator--()
{
static const packed_t reset = packed_t(1UL << (bits_per_packed_t - 1));
_mask >>= 1;
if (!_mask) {
_mask = reset;
--_it;
}
return (*((Self*)this));
}
Self& operator+=(Distance n)
{
if (n < 0)
return (*this) -= (-n);
_it += n / bits_per_packed_t;
dorepeat(n % bits_per_packed_t)
++(*this);
return (*((Self*)this));
}
Self& operator-=(Distance n)
{
if (n < 0)
return (*this) += (-n);
_it -= n / bits_per_packed_t;
dorepeat(n % bits_per_packed_t)
--(*this);
return (*((Self*)this));
}
bool operator<(const Self& x) const
{
return (_it < x._it ? true : integer_log2(_mask) < integer_log2(x._mask));
}
friend Distance operator-(const Self& x, const Self& y)
{
return (bits_per_packed_t*(x._it - y._it) +
integer_log2(x._mask) - integer_log2(y._mask));
}
bool operator==(const Self& rhs) const
{
return (_it == rhs._it && _mask == rhs._mask);
}
protected:
bit_iterator_base(Iterator it, width_t offset)
: _it(it), _mask(packed_t(1) << offset) { }
bit_iterator_base(packed_t mask, Iterator it) : _it(it), _mask(mask) { }
bit_iterator_base() : _it(), _mask(0) { }
Iterator _it;
packed_t _mask;
};
template<typename Iterator, typename Value>
struct iterator_base
: boost::random_access_iterator_helper<Iterator, Value>
{
typedef std::ptrdiff_t Distance;
struct reference
{
reference(const Iterator* it, size_t idx) : _it(it), _idx(idx) { }
operator Value() const {
return do_get();
}
reference& operator=(Value x) {
do_set(x); return *this;
}
reference& operator=(const reference& rhs) {
do_set(rhs);
return *this;
}
reference& operator+=(Value x) {
do_set(do_get() + x); return *this;
}
reference& operator-=(Value x) {
do_set(do_get() - x); return *this;
}
reference& operator*=(Value x) {
do_set(do_get()*x); return *this;
}
reference& operator/=(Value x) {
do_set(do_get() / x); return *this;
}
protected:
const Iterator* _it;
size_t _idx;
Value do_get() const;
void do_set(Value x);
};
Iterator& operator++() {
++_idx;
return (*((Iterator*)this));
}
Iterator& operator--() {
--_idx;
return (*((Iterator*)this));
}
Iterator& operator+=(Distance n) {
_idx += n;
return (*((Iterator*)this));
}
Iterator& operator-=(Distance n) {
_idx -= n;
return (*((Iterator*)this));
}
bool operator<(const Iterator& x) const {
return (_idx < x._idx);
}
friend Distance operator-(const Iterator& x, const Iterator& y) {
return (x._idx -y._idx);
}
bool operator==(const Iterator& rhs) const {
return (_idx == rhs._idx);
}
int idx() const {
return _idx;
}
protected:
iterator_base(const field_set& fs, size_t idx) : _fs(&fs), _idx(idx) { }
iterator_base() : _fs(NULL), _idx(0) { }
const field_set* _fs;
size_t _idx;
};
public:
struct bit_iterator
: public bit_iterator_base<bit_iterator, instance::iterator>
{
friend struct field_set;
struct reference
{
reference(instance::iterator it, packed_t mask)
: _it(it), _mask(mask) {}
operator bool() const {
return (*_it & _mask) != 0;
}
bool operator~() const {
return (*_it & _mask) == 0;
}
reference& flip() {
do_flip(); return *this;
}
reference& operator=(bool x) {
do_assign(x); return *this;
}
reference& operator=(const reference& rhs) {
do_assign(rhs);
return *this;
}
reference& operator|=(bool x) {
if (x) do_set();
return *this;
}
reference& operator&=(bool x) {
if (!x) do_reset();
return *this;
}
reference& operator^=(bool x) {
if (x) do_flip();
return *this;
}
reference& operator-=(bool x) {
if (x) do_reset();
return *this;
}
protected:
instance::iterator _it;
packed_t _mask;
void do_set() {
*_it |= _mask;
}
void do_reset() {
*_it &= ~_mask;
}
void do_flip() {
*_it ^= _mask;
}
void do_assign(bool x) {
x ? do_set() : do_reset();
}
};
reference operator*() const {
return reference(_it, _mask);
}
friend class const_bit_iterator;
bit_iterator() { }
protected:
bit_iterator(instance::iterator it, width_t offset)
: bit_iterator_base<bit_iterator, instance::iterator>(it, offset)
{ }
};
struct const_bit_iterator
: public bit_iterator_base<const_bit_iterator, instance::const_iterator>
{
friend class field_set;
bool operator*() const {
return (*_it & _mask) != 0;
}
const_bit_iterator(const bit_iterator& bi)
: bit_iterator_base < const_bit_iterator,
instance::const_iterator > (bi._mask, bi._it) { }
const_bit_iterator() { }
protected:
const_bit_iterator(instance::const_iterator it, width_t offset)
: bit_iterator_base < const_bit_iterator,
instance::const_iterator > (it, offset) { }
};
struct disc_iterator : public iterator_base<disc_iterator, disc_t>
{
friend struct field_set;
friend struct reference;
friend class const_disc_iterator;
reference operator*() const
{
return reference(this, _idx);
}
disc_iterator() : _inst(NULL) { }
multiplicity_t multy() const
{
size_t spec_idx = _fs->raw_to_disc_idx(_idx);
return _fs->disc_and_bit()[spec_idx].multy;
}
void randomize(opencog::RandGen& rng = randGen())
{
_fs->set_raw(*_inst, _idx, rng.randint(multy()));
}
protected:
disc_iterator(const field_set& fs, size_t idx, instance& inst)
: iterator_base<disc_iterator, disc_t>(fs, idx), _inst(&inst) { }
instance* _inst;
};
struct const_disc_iterator
: public iterator_base<const_disc_iterator, disc_t>
{
friend class field_set;
disc_t operator*() const
{
return _fs->get_raw(*_inst, _idx);
}
const_disc_iterator(const disc_iterator& bi) :
iterator_base<const_disc_iterator, disc_t>(*bi._fs, bi._idx),
_inst(bi._inst) { }
const_disc_iterator() : _inst(NULL) { }
multiplicity_t multy() const
{
size_t spec_idx = _fs->raw_to_disc_idx(_idx);
return _fs->disc_and_bit()[spec_idx].multy;
}
protected:
const_disc_iterator(const field_set& fs, size_t idx, const instance& inst)
: iterator_base<const_disc_iterator, disc_t>(fs, idx), _inst(&inst) { }
const instance* _inst;
};
struct contin_iterator : public iterator_base<contin_iterator, contin_t>
{
friend struct field_set;
friend struct reference;
friend class const_contin_iterator;
reference operator*() const
{
return reference(this, _idx);
}
contin_iterator() : _inst(NULL) { }
protected:
contin_iterator(const field_set& fs, size_t idx, instance& inst)
: iterator_base<contin_iterator, contin_t>(fs, idx), _inst(&inst)
{ }
instance* _inst;
};
struct const_contin_iterator
: public iterator_base<const_contin_iterator, contin_t>
{
friend class field_set;
contin_t operator*() const
{
return _fs->get_contin(*_inst, _idx);
}
const_contin_iterator(const contin_iterator& bi)
: iterator_base<const_contin_iterator, contin_t>(*bi._fs, bi._idx),
_inst(bi._inst) { }
const_contin_iterator() : _inst(NULL) { }
protected:
const_contin_iterator(const field_set& fs, size_t idx,
const instance& inst)
: iterator_base<const_contin_iterator, contin_t>(fs, idx),
_inst(&inst) { }
const instance* _inst;
};
struct term_iterator
: public iterator_base<term_iterator, term_t>
{
friend class field_set;
friend struct reference;
friend class const_term_iterator;
reference operator*() const
{
return reference(this, _idx);
}
term_iterator() : _inst(NULL) { }
protected:
term_iterator(const field_set& fs, size_t idx, instance& inst)
: iterator_base<term_iterator, term_t>(fs, idx),
_inst(&inst) { }
instance* _inst;
};
struct const_term_iterator
: public iterator_base<const_term_iterator, term_t>
{
friend class field_set;
const term_t& operator*() const
{
return _fs->get_term(*_inst, _idx);
}
const_term_iterator(const term_iterator& bi) :
iterator_base<const_term_iterator, term_t>(*bi._fs, bi._idx),
_inst(bi._inst) { }
const_term_iterator() : _inst(NULL) { }
protected:
const_term_iterator(const field_set& fs, size_t idx,
const instance& inst)
: iterator_base<const_term_iterator, term_t>(fs, idx),
_inst(&inst) { }
const instance* _inst;
};
const_bit_iterator begin_bit(const instance& inst) const
{
return (begin_bit_fields() == _fields.end() ? const_bit_iterator() :
const_bit_iterator(inst.begin() + begin_bit_fields()->major_offset,
begin_bit_fields()->minor_offset));
}
const_bit_iterator end_bit(const instance& inst) const
{
return (begin_bit_fields() == _fields.end() ? const_bit_iterator() :
++const_bit_iterator(--inst.end(), _fields.back().minor_offset));
}
bit_iterator begin_bit(instance& inst) const
{
return (begin_bit_fields() == _fields.end() ? bit_iterator() :
bit_iterator(inst.begin() + begin_bit_fields()->major_offset,
begin_bit_fields()->minor_offset));
}
bit_iterator end_bit(instance& inst) const
{
return (begin_bit_fields() == _fields.end() ? bit_iterator() :
++bit_iterator(--inst.end(), _fields.back().minor_offset));
}
const_disc_iterator begin_disc(const instance& inst) const
{
return const_disc_iterator(*this, begin_disc_raw_idx(), inst);
}
const_disc_iterator end_disc(const instance& inst) const
{
return const_disc_iterator(*this, end_disc_raw_idx(), inst);
}
disc_iterator begin_disc(instance& inst) const {
return disc_iterator(*this, begin_disc_raw_idx(), inst);
}
disc_iterator end_disc(instance& inst) const {
return disc_iterator(*this, end_disc_raw_idx(), inst);
}
const_contin_iterator begin_contin(const instance& inst) const {
return const_contin_iterator(*this, 0, inst);
}
const_contin_iterator end_contin(const instance& inst) const {
return const_contin_iterator(*this, _contin.size(), inst);
}
contin_iterator begin_contin(instance& inst) const {
return contin_iterator(*this, 0, inst);
}
contin_iterator end_contin(instance& inst) const {
return contin_iterator(*this, _contin.size(), inst);
}
const_term_iterator begin_term(const instance& inst) const {
return const_term_iterator(*this, 0, inst);
}
const_term_iterator end_term(const instance& inst) const {
return const_term_iterator(*this, _term.size(), inst);
}
term_iterator begin_term(instance& inst) const {
return term_iterator(*this, 0, inst);
}
term_iterator end_term(instance& inst) const {
return term_iterator(*this, _term.size(), inst);
}
const_disc_iterator begin_raw(const instance& inst) const {
return const_disc_iterator(*this, 0, inst);
}
const_disc_iterator end_raw(const instance& inst) const {
return const_disc_iterator(*this, _fields.size(), inst);
}
disc_iterator begin_raw(instance& inst) const {
return disc_iterator(*this, 0, inst);
}
disc_iterator end_raw(instance& inst) const {
return disc_iterator(*this, _fields.size(), inst);
}
std::ostream& ostream_field_set(std::ostream& out) const;
};
template<>
inline disc_t field_set::iterator_base < field_set::disc_iterator,
disc_t >::reference::do_get() const
{
return _it->_fs->get_raw(*_it->_inst, _idx);
}
template<>
inline void field_set::iterator_base < field_set::disc_iterator,
disc_t >::reference::do_set(disc_t x)
{
_it->_fs->set_raw(*_it->_inst, _idx, x);
}
template<>
inline contin_t field_set::iterator_base < field_set::contin_iterator,
contin_t >::reference::do_get() const
{
return _it->_fs->get_contin(*_it->_inst, _idx);
}
template<>
inline void field_set::iterator_base < field_set::contin_iterator,
contin_t >::reference::do_set(contin_t x)
{
_it->_fs->set_contin(*_it->_inst, _idx, x);
}
template<typename It, typename Out>
Out field_set::pack(It from, Out out) const
{
unsigned int offset = 0;
for (const term_spec& o : _term) {
size_t width = nbits_to_pack(o.branching);
size_t total_width = size_t((width * o.depth - 1) /
bits_per_packed_t + 1) * bits_per_packed_t;
dorepeat (o.depth) {
*out |= packed_t(*from++) << offset;
offset += width;
if (offset == bits_per_packed_t) {
offset = 0;
++out;
}
}
offset += total_width - (o.depth * width);
if (offset == bits_per_packed_t) {
offset = 0;
++out;
}
}
for (const contin_spec& c : _contin) {
dorepeat (c.depth) {
*out |= packed_t(*from++) << offset;
offset += 2;
if (offset == bits_per_packed_t) {
offset = 0;
++out;
}
}
}
for (const disc_spec& d : _disc) {
*out |= packed_t(*from++) << offset;
offset += nbits_to_pack(d.multy);
if (offset == bits_per_packed_t) {
offset = 0;
++out;
}
}
if (offset > 0)
++out;
return out;
}
inline std::ostream& operator<<(std::ostream& out,
const field_set& fs)
{
return fs.ostream_field_set(out);
}
}
std::string oc_to_string(const moses::field_set& fs,
const std::string& indent=empty_string);
}
#endif