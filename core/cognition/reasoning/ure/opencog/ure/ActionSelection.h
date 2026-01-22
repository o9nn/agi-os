#ifndef _OPENCOG_ACTIONSELECTION_H_
#define _OPENCOG_ACTIONSELECTION_H_
#include <opencog/util/empty_string.h>
#include <opencog/util/Counter.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
#include "ThompsonSampling.h"
namespace opencog
{
typedef std::map<Handle, TruthValuePtr> HandleTVMap;
typedef Counter<Handle, double> HandleCounter;
typedef Counter<Handle, unsigned> HandleUCounter;
class ActionSelection
{
public:
const HandleTVMap& action2tv;
ActionSelection(const HandleTVMap& action2tv);
HandleCounter distribution();
Handle operator()();
std::string to_string(const std::string& indent=empty_string) const;
private:
double Pi(size_t i, const std::vector<std::vector<double>>& cdfs) const;
TruthValueSeq _tvs;
ThompsonSampling _tsmp;
};
std::string	oc_to_string(const ActionSelection& asel,
const std::string& indent=empty_string);
std::string	oc_to_string(const HandleTVMap& h2tv,
const std::string& indent=empty_string);
std::string oc_to_string(const HandleCounter& hc,
const std::string& indent=empty_string);
std::string oc_to_string(const HandleUCounter& huc,
const std::string& indent=empty_string);
}
#endif