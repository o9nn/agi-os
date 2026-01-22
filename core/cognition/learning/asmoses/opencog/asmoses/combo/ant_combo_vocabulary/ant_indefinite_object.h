#ifndef _ANT_INDEFINITE_OBJECT_H
#define _ANT_INDEFINITE_OBJECT_H
#include <opencog/util/numeric.h>
#include <opencog/asmoses/combo/combo/indefinite_object.h>
#include "ant_operator.h"
namespace opencog { namespace combo {
namespace id {
enum ant_indefinite_object_enum {
ant_indefinite_object_count
};
}
typedef id::ant_indefinite_object_enum ant_indefinite_object_enum;
namespace ant_indefinite_object_properties {
typedef ant_operator<ant_indefinite_object_enum, id::ant_indefinite_object_count>::basic_description indefinite_object_basic_description;
static const indefinite_object_basic_description iobd[] = {
};
}
class ant_indefinite_object : public ant_operator<ant_indefinite_object_enum, id::ant_indefinite_object_count>, public indefinite_object_base {
private:
ant_indefinite_object();
const basic_description * get_basic_description_array() const;
unsigned int get_basic_description_array_count() const;
static const ant_indefinite_object* init_indefinite_object();
void set_indefinite_object(ant_indefinite_object_enum);
public:
const std::string& get_name() const;
const type_tree& get_type_tree() const;
arity_t arity() const;
type_tree get_output_type_tree() const;
const type_tree& get_input_type_tree(arity_t i) const;
static indefinite_object get_instance(const std::string& name);
static indefinite_object get_instance(ant_indefinite_object_enum);
};
}}
#endif