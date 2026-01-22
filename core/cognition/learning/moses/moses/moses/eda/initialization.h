#ifndef _EDA_INITIALIZATION_H
#define _EDA_INITIALIZATION_H
#include "../representation/field_set.h"
namespace opencog {
namespace moses {
void occam_randomize_contin(const field_set&, instance&,
field_set::contin_iterator,
opencog::RandGen& rng = randGen());
void occam_randomize_term(const field_set&, instance&,
field_set::const_term_iterator,
opencog::RandGen& rng = randGen());
void occam_randomize_term(const field_set&, instance&,
opencog::RandGen& rng = randGen());
void occam_randomize_contin(const field_set&, instance&,
opencog::RandGen& rng = randGen());
void uniform_randomize_bit(const field_set&, instance&,
opencog::RandGen& rng = randGen());
void uniform_randomize_disc(const field_set&, instance&,
opencog::RandGen& rng = randGen());
void randomize(const field_set&, instance&,
opencog::RandGen& rng = randGen());
}
}
#endif