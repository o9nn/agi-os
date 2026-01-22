#ifndef _MOSES_TIME_DISPERSION_H
#define _MOSES_TIME_DISPERSION_H
#include "scoring_base.h"
namespace opencog { namespace moses {
using combo::CTableTime;
enum class TemporalGranularity {day, month};
struct bscore_ctable_time_dispersion : public bscore_ctable_base
{
bscore_ctable_time_dispersion(const CTable& ctable,
float time_dispersion_pressure = 0.0,
float time_dispersion_exponent = 1.0,
TemporalGranularity granularity =
TemporalGranularity::day,
unsigned multiplier = 1);
protected:
TemporalGranularity _granularity;
unsigned _multiplier;
float _pressure,
_exponent,
_Hmax;
TTable::value_type get_timestamp_class(const TTable::value_type& timestamp) const;
score_t get_time_dispersion_penalty(const CTableTime& ctt) const;
};
}
}
#endif