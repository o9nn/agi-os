#ifndef _OPENCOG_TABLE_H
#define _OPENCOG_TABLE_H
#include <fstream>
#include <boost/lexical_cast.hpp>
#include <boost/range/algorithm/transform.hpp>
#include <boost/range/algorithm/adjacent_find.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <boost/operators.hpp>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <opencog/util/algorithm.h>
#include <opencog/util/Counter.h>
#include <opencog/util/dorepeat.h>
#include <opencog/util/exceptions.h>
#include <opencog/asmoses/utils/KLD.h>
#include <opencog/asmoses/utils/iostreamContainer.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/asmoses/combo/type_checker/type_tree.h>
#include <opencog/asmoses/combo/interpreter/eval.h>
#include <opencog/asmoses/combo/interpreter/interpreter.h>
#include <opencog/asmoses/combo/combo/vertex.h>
#include <opencog/asmoses/combo/combo/common_def.h>
#define COEF_SAMPLE_COUNT 20.0
#define TARGET_DISCRETIZED_BINS_NUM 5
namespace opencog
{
namespace combo
{
contin_seq discretize_contin_feature(contin_t min, contin_t max);
builtin get_discrete_bin(contin_seq disc_intvs, contin_t val);
std::vector<unsigned> get_indices(const string_seq &labels,
const string_seq &header);
template<typename T >
struct push_back_visitor : public boost::static_visitor<>
{
push_back_visitor(const T &value) : _value(value)
{}
void operator()(std::vector<T> &seq) const
{
seq.push_back(_value);
}
void operator()(vertex_seq &seq) const
{
seq.push_back(_value);
}
template<typename Seq>
void operator()(Seq &seq) const
{
std::stringstream ss;
ss << "You can't push_back " << _value << " in container ";
ostream_container(ss, seq);
OC_ASSERT(false, ss.str());
}
const T &_value;
};
struct pop_back_visitor : public boost::static_visitor<>
{
template<typename Seq>
void operator()(Seq &seq) const
{
seq.pop_back();
}
};
struct init_at_visitor : public boost::static_visitor<>
{
init_at_visitor(size_t pos) : _pos(pos)
{}
template<typename Seq>
void operator()(Seq &seq) const
{
typedef typename Seq::value_type vt;
seq[_pos] = vt();
}
size_t _pos;
};
template<typename T>
struct get_at_visitor : public boost::static_visitor<T>
{
get_at_visitor(size_t pos) : _pos(pos)
{}
T operator()(const std::vector<T> &seq) const
{
return seq[_pos];
}
T operator()(const vertex_seq &seq) const
{
return boost::get<T>(seq[_pos]);
}
T operator()(const combo_tree_seq &seq) const
{
return boost::get<T>(*seq[_pos].begin());
}
template<typename Seq>
T operator()(const Seq &seq) const
{
OC_ASSERT(false, "Impossible operation");
return T();
}
size_t _pos;
};
template<>
struct get_at_visitor<vertex> : public boost::static_visitor<vertex>
{
get_at_visitor(size_t pos) : _pos(pos)
{}
vertex operator()(const combo_tree_seq &seq) const
{
return *seq[_pos].begin();
}
template<typename Seq>
vertex operator()(const Seq &seq) const
{
return seq[_pos];
}
size_t _pos;
};
template<>
struct get_at_visitor<combo_tree> : public boost::static_visitor<combo_tree>
{
get_at_visitor(size_t pos) : _pos(pos)
{}
template<typename Seq>
combo_tree operator()(const Seq &seq) const
{
return seq[_pos];
}
size_t _pos;
};
struct erase_at_visitor : public boost::static_visitor<>
{
erase_at_visitor(size_t pos) : _pos(pos)
{}
template<typename Seq>
void operator()(Seq &seq) const
{
seq.erase(seq.begin() + _pos);
}
size_t _pos;
};
template<typename T>
struct insert_at_visitor : public boost::static_visitor<>
{
insert_at_visitor(int pos, const T v) : _pos(pos), _v(v)
{}
void operator()(std::vector<T> &seq) const
{
seq.insert(_pos >= 0 ? seq.begin() + _pos : seq.end(), _v);
}
template<typename Seq>
void operator()(Seq &seq) const
{
std::stringstream ss;
ss << "You can't insert " << _v << " at " << _pos << " in container ";
ostream_container(ss, seq);
OC_ASSERT(false, ss.str());
}
int _pos;
const T &_v;
};
struct size_visitor : public boost::static_visitor<size_t>
{
template<typename Seq>
size_t operator()(const Seq &seq)
{
return seq.size();
}
};
struct empty_visitor : public boost::static_visitor<bool>
{
template<typename Seq>
bool operator()(const Seq &seq)
{
return seq.empty();
}
};
struct equal_visitor : public boost::static_visitor<bool>
{
#define __FALSE_EQ__(seql_t, seqr_t) \
bool operator()(const seql_t& l, const seqr_t& r) const { \
return false; \
}
__FALSE_EQ__(builtin_seq, contin_seq);
__FALSE_EQ__(builtin_seq, string_seq);
__FALSE_EQ__(builtin_seq, combo_tree_seq);
__FALSE_EQ__(contin_seq, builtin_seq);
__FALSE_EQ__(contin_seq, string_seq);
__FALSE_EQ__(contin_seq, combo_tree_seq);
__FALSE_EQ__(string_seq, builtin_seq);
__FALSE_EQ__(string_seq, contin_seq);
__FALSE_EQ__(string_seq, combo_tree_seq);
__FALSE_EQ__(combo_tree_seq, builtin_seq);
__FALSE_EQ__(combo_tree_seq, contin_seq);
__FALSE_EQ__(combo_tree_seq, string_seq);
__FALSE_EQ__(combo_tree_seq, vertex_seq);
__FALSE_EQ__(vertex_seq, combo_tree_seq);
#undef __FALSE_EQ__
template<typename SeqL, typename SeqR>
bool operator()(const SeqL &l, const SeqR &r) const
{
return boost::equal(l, r);
}
};
std::string table_fmt_vertex_to_str(const vertex &v);
std::string table_fmt_builtin_to_str(const builtin &b);
struct to_strings_visitor : public boost::static_visitor<string_seq>
{
string_seq operator()(const string_seq &seq)
{
return seq;
}
string_seq operator()(const vertex_seq &seq)
{
string_seq res;
boost::transform(seq, back_inserter(res), table_fmt_vertex_to_str);
return res;
}
string_seq operator()(const builtin_seq &seq)
{
string_seq res;
boost::transform(seq, back_inserter(res), table_fmt_builtin_to_str);
return res;
}
template<typename Seq>
string_seq operator()(const Seq &seq)
{
string_seq res;
boost::transform(seq, back_inserter(res),
[](const typename Seq::value_type &v) {
std::stringstream ss;
ss << v;
return ss.str();
});
return res;
}
};
struct get_type_tree_at_visitor : public boost::static_visitor<type_tree>
{
get_type_tree_at_visitor(size_t pos) : _pos(pos)
{}
template<typename Seq>
type_tree operator()(const Seq &seq)
{
return get_type_tree(seq[_pos]);
}
size_t _pos;
};
struct interpreter_visitor : public boost::static_visitor<vertex>
{
interpreter_visitor(const combo_tree &tr) : _it(tr.begin())
{
mixed = false;
combo_tree::iterator mit = tr.begin();
combo_tree::iterator mend = tr.end();
for (; mit != mend; ++mit) {
mixed = is_contin_expr(*mit);
if (mixed) break;
mixed = (id::greater_than_zero == *mit);
if (mixed) break;
}
}
interpreter_visitor(const combo_tree::iterator &it) : _it(it)
{
mixed = is_contin_expr(*_it);
if (not mixed) mixed = (id::greater_than_zero == *_it);
}
vertex operator()(const builtin_seq &inputs)
{
if (mixed) return mixed_interpreter(inputs)(_it);
return boolean_interpreter(inputs)(_it);
}
vertex operator()(const contin_seq &inputs)
{
return mixed_interpreter(inputs)(_it);
}
vertex operator()(const vertex_seq &inputs)
{
return mixed_interpreter(inputs)(_it);
}
vertex operator()(const string_seq &inputs)
{
OC_ASSERT(false, "Not implemented");
return vertex();
}
vertex operator()(const combo_tree_seq &inputs)
{
OC_ASSERT(false, "Not implemented");
return vertex();
}
combo_tree::iterator _it;
bool mixed;
};
struct multi_type_seq : public boost::less_than_comparable<multi_type_seq>,
public boost::equality_comparable<multi_type_seq>
{
typedef boost::variant<builtin_seq,
contin_seq,
string_seq,
vertex_seq,
combo_tree_seq> multi_type_variant;
multi_type_seq()
{
}
template<typename T>
multi_type_seq(const std::initializer_list<T> &il)
: _variant(std::vector<T>(il))
{}
template<typename T>
multi_type_seq(const T &v) : _variant(v)
{}
template<typename T>
void push_back(const T &e)
{
boost::apply_visitor(push_back_visitor<T>(e), _variant);
}
void pop_back()
{
pop_back_visitor popbv;
boost::apply_visitor(popbv, _variant);
}
bool operator<(const multi_type_seq &r) const
{
return get_variant() < r.get_variant();
}
bool operator==(const multi_type_seq &r) const
{
equal_visitor ev;
return boost::apply_visitor(ev, get_variant(), r.get_variant());
}
size_t size() const
{
size_visitor sv;
return boost::apply_visitor(sv, _variant);
}
bool empty() const
{
empty_visitor ev;
return boost::apply_visitor(ev, _variant);
}
void erase_at(size_t pos)
{
boost::apply_visitor(erase_at_visitor(pos), _variant);
}
void init_at(size_t pos)
{
boost::apply_visitor(init_at_visitor(pos), _variant);
}
template<typename T>
T get_at(size_t pos) const
{
return boost::apply_visitor(get_at_visitor<T>(pos), _variant);
}
template<typename T>
void insert_at(int pos, const T &v)
{
boost::apply_visitor(insert_at_visitor<T>(pos, v), _variant);
}
string_seq to_strings() const
{
to_strings_visitor tsv;
return boost::apply_visitor(tsv, _variant);
}
multi_type_variant &get_variant()
{ return _variant; }
const multi_type_variant &get_variant() const
{ return _variant; }
template<typename T>
std::vector<T> &get_seq()
{
return boost::get<std::vector<T>>(_variant);
}
template<typename T>
const std::vector<T> &get_seq() const
{
return boost::get<std::vector<T>>(_variant);
}
protected:
mutable multi_type_variant _variant;
};
template<typename F>
struct seq_filtered_visitor : public boost::static_visitor<multi_type_seq>
{
seq_filtered_visitor(const F &filter) : _filter(filter)
{}
template<typename Seq>
multi_type_seq operator()(const Seq &seq)
{
return seq_filtered(seq, _filter);
}
const F &_filter;
};
static const std::string default_timestamp_label("timestamp");
struct TTable : public std::vector<boost::gregorian::date>
{
typedef std::vector<boost::gregorian::date> super;
public:
typedef boost::gregorian::date value_type;
TTable(const std::string &tl=default_timestamp_label);
TTable(const super &tt, const std::string &tl=default_timestamp_label);
void set_label(const std::string &);
const std::string &get_label() const;
static TTable::value_type from_string(const std::string &timestamp_str);
static std::string to_string(const TTable::value_type &timestamp);
protected:
std::string label;
};
struct TimedValue :
public boost::less_than_comparable<TimedValue>,
public boost::equality_comparable<TimedValue>
{
TimedValue(const vertex v, const TTable::value_type t=TTable::value_type())
: value(v), timestamp(t)
{}
vertex value;
TTable::value_type timestamp;
bool operator<(const TimedValue &r) const
{
return (value < r.value) || (timestamp < r.timestamp);
}
bool operator==(const TimedValue &r) const
{
return (value == r.value) && (timestamp == r.timestamp);
}
};
typedef double count_t;
struct TimedCounter : public Counter<TimedValue, count_t>
{
count_t get(const vertex &v) const;
Counter<vertex, count_t> untimedCounter() const;
vertex mode() const;
};
typedef std::map<TTable::value_type, Counter<vertex, count_t>> CompressedTableTime;
class CompressedTable : public std::map<multi_type_seq, TimedCounter>
{
public:
typedef multi_type_seq key_type;
typedef TimedCounter mapped_type;
typedef TimedCounter counter_t;
typedef std::map<key_type, TimedCounter> super;
typedef typename super::value_type value_type;
template<typename Func>
CompressedTable(const Func &func, arity_t arity, int nsamples=-1);
CompressedTable(const std::string &_olabel="output");
CompressedTable(const string_seq &labs, const type_tree &tt);
CompressedTable(const std::string &_olabel, const string_seq &_ilabels,
const type_tree &tt);
arity_t get_arity() const
{ return ilabels.size(); }
vertex_seq get_input_col_data(int offset) const;
count_t uncompressed_size() const;
template<typename F>
CompressedTable filtered(const F &filter) const
{
typedef type_tree::iterator pre_it;
typedef type_tree::sibling_iterator sib_it;
type_tree fsig;
pre_it head_src = tsig.begin();
OC_ASSERT(*head_src == id::lambda_type);
OC_ASSERT((int) tsig.number_of_children(head_src) == get_arity() + 1);
pre_it head_dst = fsig.set_head(*head_src);
sib_it sib_src = head_src.begin();
arity_t a_pre = 0;
for (arity_t a : filter) {
std::advance(sib_src, a - a_pre);
a_pre = a;
fsig.replace(fsig.append_child(head_dst), sib_src);
}
fsig.replace(fsig.append_child(head_dst), head_src.last_child());
CompressedTable res(olabel, seq_filtered(ilabels, filter), fsig);
seq_filtered_visitor <F> sfv(filter);
auto asfv = boost::apply_visitor(sfv);
for (const CompressedTable::value_type& v : *this)
res[asfv(v.first.get_variant())] += v.second;
return res;
}
template<typename F>
multi_type_seq filtered_preserve_idxs(const F &filter,
const multi_type_seq &seq) const
{
multi_type_seq res;
auto it = filter.cbegin();
for (unsigned i = 0; i < seq.size(); ++i) {
if (it != filter.cend() && (typename F::value_type) i == *it) {
res.push_back(seq.get_at<builtin>(i));
++it;
} else {
res.push_back(id::null_vertex);
}
}
return res;
}
template<typename F>
CompressedTable filtered_preserve_idxs(const F &filter) const
{
CompressedTable res(olabel, ilabels, tsig);
for (const CompressedTable::value_type& v : *this)
res[filtered_preserve_idxs(filter, v.first)] += v.second;
return res;
}
void remove_rows(const std::set<unsigned> &idxs);
void remove_rows_at_times(const std::set<TTable::value_type> &timestamps);
void remove_rows_at_time(const TTable::value_type &timestamp);
std::set<TTable::value_type> get_timestamps() const;
void set_labels(const string_seq &labels);
string_seq get_labels() const;
const std::string &get_output_label() const;
const string_seq &get_input_labels() const;
void set_signature(const type_tree &tt);
const type_tree &get_signature() const;
type_node get_output_type() const;
CompressedTableTime ordered_by_time() const;
void balance();
protected:
type_tree tsig;
std::string olabel;
string_seq ilabels;
};
class OTable;
class ITable : public std::vector<multi_type_seq>
{
public:
typedef std::vector<multi_type_seq> super;
typedef super::value_type value_type;
typedef type_node_seq type_seq;
ITable();
ITable(const type_seq &ts, const string_seq &il=string_seq());
ITable(const super &mat, const string_seq &il=string_seq());
ITable(const OTable &);
ITable(const type_tree &tt, int nsamples=-1,
contin_t min_contin=-1.0, contin_t max_contin=1.0);
arity_t get_arity() const
{
return super::front().size();
}
bool operator==(const ITable &rhs) const;
void set_labels(const string_seq &);
const string_seq &get_labels() const;
void set_types(const type_seq &);
const type_seq &get_types() const;
type_node get_type(const std::string &) const;
void insert_col(const std::string &clab, const vertex_seq &col, int off=-1);
std::string delete_column(const std::string &feature);
void delete_columns(const string_seq &ignore_features);
vertex_seq get_column_data(const std::string &name) const;
vertex_seq get_column_data(int offset) const;
template<typename F>
ITable filtered(const F &filter) const
{
ITable res;
res.set_labels(seq_filtered(get_labels(), filter));
res.set_types(seq_filtered(get_types(), filter));
seq_filtered_visitor <F> sfv(filter);
auto asf = boost::apply_visitor(sfv);
for (const value_type &row : *this)
res.push_back(asf(row.get_variant()));
return res;
}
int get_column_offset(const std::string &col_name) const;
protected:
mutable type_seq types;
mutable string_seq labels;
private:
string_seq get_default_labels() const;
unsigned sample_count(arity_t contin_arity)
{
if (contin_arity == 0)
return 1;
else return COEF_SAMPLE_COUNT * log(contin_arity + M_E);
}
};
static const std::string default_output_label("output");
class OTable : public vertex_seq
{
typedef vertex_seq super;
public:
typedef vertex value_type;
OTable(const std::string &ol=default_output_label);
OTable(const super &ot, const std::string &ol=default_output_label);
OTable(const combo_tree &tr, const ITable &itable,
const std::string &ol=default_output_label);
OTable(const combo_tree &tr, const CompressedTable &ctable,
const std::string &ol=default_output_label);
template<typename Func>
OTable(const Func &f, const ITable &it,
const std::string &ol=default_output_label)
: label(ol)
{
for (const multi_type_seq &vs : it)
push_back(f(vs.get_seq<vertex>().begin(),
vs.get_seq<vertex>().end()));
}
void set_label(const std::string &);
const std::string &get_label() const;
void set_type(type_node);
type_node get_type() const;
bool operator==(const OTable &rhs) const;
contin_t abs_distance(const OTable &) const;
contin_t sum_squared_error(const OTable &) const;
contin_t mean_squared_error(const OTable &) const;
contin_t root_mean_square_error(const OTable &) const;
vertex get_enum_vertex(const std::string &token);
protected:
std::string label;
type_node type;
};
struct Table : public boost::equality_comparable<Table>
{
typedef vertex value_type;
Table();
Table(const OTable &otable_, const ITable &itable_);
template<typename Func>
Table(const Func &func, arity_t a, int nsamples=-1) :
itable(gen_signature(type_node_of<bool>(),
type_node_of<bool>(), a)),
otable(func, itable), target_pos(0), timestamp_pos(0)
{}
Table(const combo_tree &tr, int nsamples=-1,
contin_t min_contin=-1.0, contin_t max_contin=1.0);
size_t size() const
{ return itable.size(); }
arity_t get_arity() const
{ return itable.get_arity(); }
type_tree get_signature() const
{
type_tree tt(id::lambda_type);
auto root = tt.begin();
for (type_node tn : itable.get_types())
tt.append_child(root, tn);
tt.append_child(root, otable.get_type());
return tt;
}
string_seq get_labels() const;
string_seq get_input_labels() const;
const std::string &get_target() const
{ return otable.get_label(); }
template<typename F>
unsigned update_pos(unsigned pos, const F &f) const
{
unsigned filtered_out_count = 0,
last = 0;
for (unsigned v : f) {
if (v < pos)
filtered_out_count += v - last;
else {
filtered_out_count += pos - last;
break;
}
last = v;
}
return pos - filtered_out_count;
}
template<typename F>
Table filtered(const F &f) const
{
Table res;
res.itable = itable.filtered(f);
res.otable = otable;
res.ttable = ttable;
res.target_pos = update_pos(target_pos, f);
if (!ttable.empty())
res.timestamp_pos = update_pos(timestamp_pos, f);
return res;
}
CompressedTable compressed(const std::string= "") const;
ITable itable;
OTable otable;
TTable ttable;
unsigned target_pos;
unsigned timestamp_pos;
bool operator==(const Table &rhs) const;
};
template<typename Func>
CompressedTable::CompressedTable(const Func &func, arity_t arity, int nsamples)
{
Table table(func, arity, nsamples);
*this = table.compressed();
}
void subsampleTable(float ratio, Table &table);
void subsampleCompressedTable(float ratio, CompressedTable &ctable);
double OTEntropy(const OTable &ot);
template<typename FeatureSet>
double mutualInformation(const ITable &it, const OTable &ot, const FeatureSet &fs)
{
type_node otype = ot.get_type();
OC_ASSERT(id::boolean_type == otype, "Only boolean types supported");
seq_filtered_visitor <FeatureSet> sfv(fs);
auto asf = boost::apply_visitor(sfv);
typedef Counter<multi_type_seq, count_t> VSCounter;
VSCounter ic,
ioc;
ITable::const_iterator i_it = it.begin();
OTable::const_iterator o_it = ot.begin();
for (; i_it != it.end(); ++i_it, ++o_it) {
multi_type_seq ic_vec = asf(i_it->get_variant());
ic[ic_vec] += 1.0;
multi_type_seq ioc_vec(ic_vec);
ioc_vec.push_back(get_builtin(*o_it));
ioc[ioc_vec] += 1.0;
}
std::vector<double> ip(ic.size()), iop(ioc.size());
double total = it.size();
auto div_total = [&](count_t c) { return c / total; };
transform(ic | map_values, ip.begin(), div_total);
transform(ioc | map_values, iop.begin(), div_total);
return entropy(ip) + OTEntropy(ot) - entropy(iop);
}
template<typename FeatureSet>
double mutualInformation(const Table &table, const FeatureSet &fs)
{
return mutualInformation(table.itable, table.otable, fs);
}
template<typename FeatureSet>
double mutualInformation(const CompressedTable &ctable, const FeatureSet &fs)
{
seq_filtered_visitor <FeatureSet> sfv(fs);
auto asf = boost::apply_visitor(sfv);
type_node otype = ctable.get_output_type();
const type_tree &tsig = ctable.get_signature();
bool all_discrete_inputs = true;
for (const type_tree &in_tt : get_signature_inputs(tsig)) {
type_node tn = get_type_node(in_tt);
if (tn != id::boolean_type and tn != id::enum_type) {
all_discrete_inputs = false;
break;
}
}
if (all_discrete_inputs
and (id::enum_type == otype
or id::boolean_type == otype
or id::contin_type == otype)) {
typedef Counter<CompressedTable::key_type, count_t> VSCounter;
VSCounter ic;
VSCounter ioc;
double total = 0.0;
contin_seq disc_intvs;
if (id::contin_type == otype) {
contin_t min = 100000.0;
contin_t max = 0.0;
for (const auto &row : ctable) {
for (const auto &val_pair : row.second) {
const vertex &v = val_pair.first.value;
if (get_contin(v) < min)
min = get_contin(v);
if (get_contin(v) > max)
max = get_contin(v);
}
}
disc_intvs = discretize_contin_feature(min, max);
}
Counter<vertex, count_t> ycount;
for (const auto &row : ctable) {
CompressedTable::key_type vec = asf(row.first.get_variant());
count_t row_total = row.second.total_count();
ic[vec] += row_total;
for (const auto &val_pair : row.second) {
const vertex &v = val_pair.first.value;
count_t count = row.second.get(v);
builtin b;
switch (otype) {
case id::enum_type:
vec.push_back(get_enum_type(v));
ycount[v] += count;
break;
case id::boolean_type:
vec.push_back(get_builtin(v));
ycount[v] += count;
break;
case id::contin_type:
b = get_discrete_bin(disc_intvs, get_contin(v));
vec.push_back(b);
ycount[b] += count;
break;
default: OC_ASSERT(false, "case not implemented");
}
ioc[vec] += count;
vec.pop_back();
}
total += row_total;
}
std::vector<double> yprob(ycount.size()), ip(ic.size()), iop(ioc.size());
auto div_total = [&](count_t c) { return c / total; };
transform(ycount | map_values, yprob.begin(), div_total);
transform(ic | map_values, ip.begin(), div_total);
transform(ioc | map_values, iop.begin(), div_total);
return entropy(ip) + entropy(yprob) - entropy(iop);
}
else if (id::contin_type == otype) {
if (1 < fs.size()) {
OC_ASSERT(0, "Contin MI currently supports only 1 feature.");
}
std::multimap<contin_t, contin_t> sorted_list;
for (const auto &row : ctable) {
CompressedTable::key_type vec = asf(row.first.get_variant());
contin_t x = vec.get_at<contin_t>(0);
for (const auto &val_pair : row.second) {
const auto &v = val_pair.first.value;
contin_t y = get_contin(v);
unsigned flt_count = val_pair.second;
dorepeat(flt_count) {
auto pr = std::make_pair(x, y);
sorted_list.insert(pr);
}
}
}
contin_seq p, q;
for (auto pr : sorted_list) {
p.push_back(pr.first);
q.push_back(pr.second);
}
contin_t ic = -KLD(p, q);
unsigned idx = *(fs.begin());
logger().debug() << "Contin MI for feat=" << idx << " ic=" << ic;
return ic;
}
else {
std::stringstream ss;
ss << "Mutual Information not implemented for the following type signature"
<< std::endl << tsig;
OC_ASSERT(0, ss.str());
return 0.0;
}
}
template<typename FeatureSet>
double mutualInformationBtwSets(const CompressedTable &ctable,
const FeatureSet &fs_l,
const FeatureSet &fs_r)
{
FeatureSet fs_u = set_union(fs_l, fs_r);
OC_ASSERT(std::all_of(fs_u.begin(), fs_u.end(),
[&](const typename FeatureSet::value_type& f) {
return f < ctable.get_arity();}));
seq_filtered_visitor <FeatureSet> sfv_u(fs_u), sfv_l(fs_l), sfv_r(fs_r);
auto asf_u = boost::apply_visitor(sfv_u),
asf_l = boost::apply_visitor(sfv_l),
asf_r = boost::apply_visitor(sfv_r);
type_node otype = ctable.get_output_type();
if (id::enum_type == otype or id::boolean_type == otype or id::contin_type == otype) {
typedef Counter<CompressedTable::key_type, count_t> VSCounter;
VSCounter
uc,
lc,
rc;
double total = 0.0;
for (const auto &row : ctable) {
CompressedTable::key_type vec_u = asf_u(row.first.get_variant()),
vec_l = asf_l(row.first.get_variant()),
vec_r = asf_r(row.first.get_variant());
count_t row_total = row.second.total_count();
uc[vec_u] += row_total;
lc[vec_l] += row_total;
rc[vec_r] += row_total;
total += row_total;
}
std::vector<double> up(uc.size()), lp(lc.size()), rp(rc.size());
auto div_total = [&](count_t c) { return c / total; };
transform(uc | map_values, up.begin(), div_total);
transform(lc | map_values, lp.begin(), div_total);
transform(rc | map_values, rp.begin(), div_total);
return entropy(lp) + entropy(rp) - entropy(up);
}
else {
OC_ASSERT(0, "Unsupported type for mutual information");
return 0.0;
}
}
void subsampleTable(ITable &it, OTable &ot, unsigned nsamples);
void subsampleTable(Table &table, unsigned nsamples);
void subsampleTable(ITable &it, unsigned nsamples);
typedef std::vector<bool> bool_seq;
class complete_truth_table : public bool_seq
{
public:
typedef bool_seq super;
complete_truth_table()
{}
template<typename It>
complete_truth_table(It from, It to) : super(from, to)
{}
template<typename T>
complete_truth_table(const tree<T> &tr, arity_t arity)
: super(pow2(arity)), _arity(arity)
{
populate(tr);
}
template<typename T>
complete_truth_table(const tree<T> &tr)
{
_arity = arity(tr);
this->resize(pow2(_arity));
populate(tr);
}
complete_truth_table(const Handle &)
{
OC_ASSERT(false, "Truth table from Handle not implemented yet");
}
complete_truth_table(const Handle &handle, arity_t arity)
: super(pow2(arity)), _arity(arity)
{
populate(handle);
}
template<typename Func>
complete_truth_table(const Func &f, arity_t arity)
: super(pow2(arity)), _arity(arity)
{
iterator it = begin();
for (int i = 0; it != end(); ++i, ++it) {
bool_seq v(_arity);
for (arity_t j = 0; j < _arity; ++j)
v[j] = (i >> j) % 2;
(*it) = f(v.begin(), v.end());
}
}
template<typename It>
bool operator()(It from, It to)
{
const_iterator it = begin();
for (int i = 1; from != to; ++from, i = i << 1)
if (*from)
it += i;
return *it;
}
size_type hamming_distance(const complete_truth_table &other) const;
bool same_complete_truth_table(const combo_tree &tr) const;
protected:
template<typename T>
void populate(const tree<T> &tr)
{
inputs.resize(_arity);
iterator it = begin();
for (int i = 0; it != end(); ++i, ++it) {
for (int j = 0; j < _arity; ++j)
inputs[j] = bool_to_builtin((i >> j) % 2);
*it = builtin_to_bool(boolean_interpreter(inputs)(tr));
}
}
void setup_features(const Handle &handle, const std::vector<ValueSeq>& features);
void populate(const Handle &handle);
void populate_features(std::vector<ValueSeq> &features);
arity_t _arity;
mutable builtin_seq inputs;
};
}
}
#endif