#ifndef _OPENCOG_URE_FITNESS_H
#define _OPENCOG_URE_FITNESS_H
#include <functional>
#include <opencog/atoms/base/Handle.h>
namespace opencog
{
class BITNode;
class AndBIT;
class BITNodeFitness
{
public:
enum FitnessType {
MaximizeConfidence
};
BITNodeFitness(FitnessType ft=MaximizeConfidence);
FitnessType type;
std::function<double(const BITNode&)> function;
double lower;
double upper;
double operator()(const BITNode& bitnode) const;
};
class AndBITFitness
{
public:
enum FitnessType {
Uniform,
Trace
};
AndBITFitness(FitnessType ft=Uniform,
const std::set<ContentHash>& tr=std::set<ContentHash>());
const FitnessType type;
std::function<double(const AndBIT&)> function;
double lower;
double upper;
double operator()(const AndBIT& andbit) const;
private:
std::set<ContentHash> _trace;
};
}
#endif