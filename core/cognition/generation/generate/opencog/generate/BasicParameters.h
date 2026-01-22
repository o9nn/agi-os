#ifndef _OPENCOG_BASIC_PARAMETERS_H
#define _OPENCOG_BASIC_PARAMETERS_H
#include <random>
#include <opencog/generate/RandomParameters.h>
namespace opencog
{
class BasicParameters : public RandomParameters
{
public:
BasicParameters();
virtual ~BasicParameters();
virtual bool connect_existing(const OdoFrame&);
virtual bool step(const OdoFrame&);
double close_fraction;
};
}
#endif