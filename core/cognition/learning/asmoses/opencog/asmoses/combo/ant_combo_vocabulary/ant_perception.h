#ifndef _ANT_PERCEPTION_H
#define _ANT_PERCEPTION_H
#include <opencog/util/numeric.h>
#include <opencog/asmoses/combo/combo/perception.h>
#include "ant_operator.h"
namespace opencog { namespace combo {
namespace id {
enum ant_perception_enum {
is_food_ahead,
ant_perception_count
};
}
typedef id::ant_perception_enum ant_perception_enum;
namespace ant_perception_properties {
typedef ant_operator<ant_perception_enum, id::ant_perception_count>::basic_description perception_basic_description;
struct perception_property_description {
ant_perception_enum perception;
bool ultrametric;
bool transitive;
bool irreflexive;
bool reflexive;
bool symmetric;
bool identity_of_indiscernibles;
};
static const perception_basic_description pbd[] = {
{ id::is_food_ahead,     "is_food_ahead",     "boolean" }
};
static const perception_property_description ppd[] = {
{ id::is_food_ahead,     false,      false,     false,      false,    false,    false }
};
}
class ant_perception : public ant_operator<ant_perception_enum, id::ant_perception_count>, public perception_base {
private:
bool _ultrametric;
bool _transitive;
bool _irreflexive;
bool _reflexive;
bool _symmetric;
bool _identity_of_indiscernibles;
ant_perception();
const basic_description * get_basic_description_array() const;
unsigned int get_basic_description_array_count() const;
static const ant_perception* init_perceptions();
void set_perception(ant_perception_enum);
public:
static const ant_perception* get_instance(const std::string& name);
static const ant_perception* get_instance(ant_perception_enum);
const std::string& get_name() const;
const type_tree& get_type_tree() const;
arity_t arity() const;
type_tree get_output_type_tree() const;
const type_tree& get_input_type_tree(arity_t i) const;
bool is_ultrametric() const;
bool is_transitive() const;
bool is_irreflexive() const;
bool is_reflexive() const;
bool is_symmetric() const;
bool is_identity_of_indiscernibles() const;
};
}}
#endif