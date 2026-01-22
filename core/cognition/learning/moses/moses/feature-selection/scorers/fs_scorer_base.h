#ifndef _OPENCOG_BASE_SCORER_H
#define _OPENCOG_BASE_SCORER_H
#include <moses/comboreduct/table/table.h>
namespace opencog {
using namespace combo;
template<typename FeatureSet>
struct fs_scorer_base
{
fs_scorer_base(const CTable& ctable, double confi)
: _ctable(ctable), _confi(confi), _usize(_ctable.uncompressed_size()) {}
virtual ~fs_scorer_base() {};
virtual double operator()(const FeatureSet& features) const = 0;
protected:
double confidence(unsigned fs_size) const {
return _usize / (_usize + exp(-_confi*fs_size));
}
const CTable& _ctable;
double _confi;
unsigned _usize;
};
}
#endif