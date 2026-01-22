#ifndef _OPENCOG_KLD_H
#define _OPENCOG_KLD_H
#include <boost/range/adaptor/map.hpp>
#include <boost/range/numeric.hpp>
#include <opencog/util/dorepeat.h>
#include <opencog/util/Logger.h>
#include <opencog/util/Counter.h>
namespace opencog {
using boost::adaptors::map_values;
template<typename FloatT>
struct KLDS {
typedef std::map<FloatT, FloatT> pdf_t;
typedef typename pdf_t::iterator pdf_it;
typedef typename pdf_t::const_iterator pdf_cit;
void set_p_pdf(const pdf_t& p_counter, FloatT p_s_ = -1) {
p_pdf = p_counter;
p_s = p_s_ < 0 ? boost::accumulate(p_pdf | map_values, 0) : p_s_;
x_very_first = p_pdf.cbegin()->first - margin;
x_very_last = p_pdf.crbegin()->first + margin;
precompute_delta_p();
}
template<typename SortedSeq>
void set_p(const SortedSeq& p) {
set_p_pdf(Counter<FloatT, FloatT>(p), p.size());
}
KLDS() : margin(1.0) {}
template<typename SortedSeq>
KLDS(const SortedSeq& p) : margin(1.0) {
set_p(p);
}
KLDS(const pdf_t& p_pdf_, FloatT p_s_ = -1) : margin(1.0) {
set_p_pdf(p_pdf_, p_s_);
}
size_t p_size() const {
return p_s;
}
size_t p_pdf_size() const {
return p_pdf.size();
}
FloatT next(const pdf_t& q_counter, FloatT q_s, FloatT& q_x_pre,
pdf_cit& cit_p, pdf_cit& cit_q) {
OC_ASSERT(cit_p != p_pdf.end());
FloatT p_x = cit_p->first,
q_x = cit_q == q_counter.cend()? x_very_last : cit_q->first;
while (q_x < p_x) {
q_x_pre = q_x;
++cit_q;
q_x = cit_q == q_counter.cend()? x_very_last : cit_q->first;
}
FloatT delta_p = cit_p->second,
delta_q_x = q_x - q_x_pre,
n_duplicates = cit_q == q_counter.cend()? 1.0 : cit_q->second,
delta_q = (p_s / q_s) * n_duplicates / delta_q_x;
++cit_p;
return std::log(delta_p / delta_q);
}
FloatT operator()(const pdf_t& q_counter) {
FloatT q_s = boost::accumulate(q_counter | map_values, 0),
q_x_pre = x_very_first,
res = 0;
pdf_cit cit_p = p_pdf.begin();
pdf_cit cit_q = q_counter.begin();
dorepeat(p_pdf.size())
res += next(q_counter, q_s, q_x_pre, cit_p, cit_q);
return res / p_s - 1;
}
template<typename Out>
void operator()(const pdf_t& q_counter, Out out) {
FloatT q_s = boost::accumulate(q_counter | map_values, 0),
q_x_pre = x_very_first;
pdf_cit cit_p = p_pdf.begin(),
cit_q = q_counter.begin();
dorepeat(p_pdf.size())
*out++ = next(q_counter, q_s, q_x_pre, cit_p, cit_q) / p_s;
}
private:
void precompute_delta_p() {
FloatT p_x_pre(x_very_first);
for (typename pdf_t::value_type& v : p_pdf) {
FloatT p_x = v.first,
delta_p_x = p_x - p_x_pre,
delta_p = v.second / delta_p_x;
v.second = delta_p;
p_x_pre = p_x;
}
}
FloatT p_s,
margin;
pdf_t p_pdf;
FloatT x_very_first, x_very_last;
};
template<typename SortedSeq>
typename SortedSeq::value_type KLD(const SortedSeq& p, const SortedSeq& q) {
typedef typename SortedSeq::value_type FloatT;
KLDS<FloatT> klds(p);
return klds(Counter<FloatT, FloatT>(q));
}
}
#endif