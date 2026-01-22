#ifndef _OPENCOG_RANDOM_PARAMETERS_H
#define _OPENCOG_RANDOM_PARAMETERS_H
#include <opencog/generate/Odometer.h>
namespace opencog
{
class RandomParameters
{
public:
RandomParameters() {}
virtual ~RandomParameters() {}
virtual bool connect_existing(const OdoFrame&) = 0;
virtual bool step(const OdoFrame&) = 0;
};
}
#endif