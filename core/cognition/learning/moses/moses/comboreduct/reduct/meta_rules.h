#ifndef _META_RULES_H
#define _META_RULES_H
#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/repetition/enum_trailing_params.hpp>
#include <boost/preprocessor/repetition/repeat.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <opencog/util/functional.h>
#include "../type_checker/type_tree.h"
#include "reduct.h"
namespace opencog { namespace reduct {
using std::string;
struct when : public crule<when> {
when(const rule& r_, bool cond_, string name = "when")
: crule<when>::crule(name), r(r_.clone()),
cond(cond_) {}
when(const when& w)
: crule<when>::crule(w.get_name()),
r(w.r->clone()), cond(w.cond) { }
void operator()(combo_tree&, combo_tree::iterator) const;
protected:
std::shared_ptr<const rule> r;
bool cond;
};
struct ignore_size_increase : public crule<ignore_size_increase> {
explicit ignore_size_increase(const rule& r_,
string name = "ignore_size_increase")
: crule<ignore_size_increase>::crule(name),
r(r_.clone()) {}
ignore_size_increase(const ignore_size_increase& i)
: crule<ignore_size_increase>::crule(i.get_name()), r(i.r->clone()) { }
void operator()(combo_tree&, combo_tree::iterator) const;
protected:
std::shared_ptr<const rule> r;
};
struct downwards : public crule<downwards>
{
explicit downwards(const rule& r_, string name = "downwards")
: crule<downwards>::crule(name), r(r_.clone()),
input(combo::id::unknown_type), output(combo::id::unknown_type) { }
downwards(const rule& r_, combo::type_node t, string name = "downwards")
: crule<downwards>::crule(name),
r(r_.clone()), input(t), output(t) { }
downwards(const rule& r_, combo::type_node input_, combo::type_node output_,
string name = "downwards")
: crule<downwards>::crule(name),
r(r_.clone()), input(input_), output(output_) { }
downwards(const downwards& d)
: crule<downwards>::crule(d.get_name()),
r(d.r->clone()), input(d.input), output(d.output) { }
void operator()(combo_tree&, combo_tree::iterator) const;
protected:
std::shared_ptr<const rule> r;
combo::type_tree input;
combo::type_node output;
};
struct upwards : public crule<upwards> {
explicit upwards(const rule& r_, string name = "upwards")
: crule<upwards>::crule(name), r(r_.clone()) {}
upwards(const upwards& u)
: crule<upwards>::crule(u.get_name()), r(u.r->clone()) {}
void operator()(combo_tree&, combo_tree::iterator) const;
protected:
std::shared_ptr<const rule> r;
};
struct iterative : public crule<iterative> {
iterative(string name = "iterative")
: crule<iterative>::crule(name) {}
explicit iterative(const rule& r_, string name = "iterative")
: crule<iterative>::crule(name), r(r_.clone()) {}
iterative(const iterative& i)
: crule<iterative>::crule(i.get_name()), r(i.r->clone()) { }
void operator()(combo_tree& tr, combo_tree::iterator it) const;
protected:
std::shared_ptr<const rule> r;
};
struct assum_iterative : public crule<assum_iterative> {
assum_iterative(string name = "assum_iterative")
: crule<assum_iterative>::crule(name) {}
explicit assum_iterative(const rule& r_, string name = "assum_iterative") :
crule<assum_iterative>::crule(name), r(r_.clone()) {}
assum_iterative(const assum_iterative& i)
: crule<assum_iterative>::crule(i.get_name()), r(i.r->clone()) { }
void operator()(combo_tree& tr, combo_tree::iterator it) const;
protected:
std::shared_ptr<const rule> r;
};
struct sequential : public crule<sequential> {
sequential(const sequential& rhs)
: crule<sequential>::crule(rhs.get_name()),
rules(rhs.rules.begin(), rhs.rules.end()) { }
#define OC_RULES_PUSH_BACK(z, n, name) rules.push_back(BOOST_PP_CAT(name, n).clone());
#define OC_SEQ_CONSTRUCTOR(z, n, unused) \
sequential(const rule &r BOOST_PP_ENUM_TRAILING_PARAMS_Z(z, n, const rule& r), string name = "sequential") \
: crule<sequential>::crule(name) \
{ \
rules.push_back(r.clone()); \
BOOST_PP_CAT(BOOST_PP_REPEAT_, z)(n, OC_RULES_PUSH_BACK, r) \
}
BOOST_PP_REPEAT(50, OC_SEQ_CONSTRUCTOR, unused)
#undef OC_SEQ_CONSTRUCTOR
#undef OC_RULES_PUSH_BACK
void operator()(combo_tree& tr, combo_tree::iterator it) const;
boost::ptr_vector<rule> rules;
};
}
}
#endif