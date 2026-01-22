#ifndef _MOSES_KNOB_MAPPER_H
#define _MOSES_KNOB_MAPPER_H
#include <map>
#include <opencog/asmoses/utils/hashing.h>
#include "field_set.h"
#include "knobs.h"
namespace opencog { namespace moses {
struct knob_mapper
{
typedef combo_tree::iterator pre_it;
typedef std::multimap<field_set::disc_spec, disc_knob> disc_map;
typedef disc_map::value_type disc_v;
typedef disc_map::const_iterator disc_map_cit;
typedef disc_map::iterator disc_map_it;
typedef std::multimap<field_set::contin_spec, contin_knob> contin_map;
typedef contin_map::value_type contin_v;
typedef contin_map::const_iterator contin_map_cit;
typedef contin_map::iterator contin_map_it;
disc_map disc;
contin_map contin;
typedef std::map<pre_it, disc_map_cit, obj_ptr_cmp<pre_it>>
it_disc_knob_map;
typedef std::map<pre_it, contin_map_cit, obj_ptr_cmp<pre_it>>
it_contin_knob_map;
typedef std::map<pre_it, int, obj_ptr_cmp<pre_it>> it_disc_idx_map;
typedef std::map<pre_it, int, obj_ptr_cmp<pre_it>> it_contin_idx_map;
it_disc_knob_map it_disc_knob;
it_contin_knob_map it_contin_knob;
it_disc_idx_map it_disc_idx;
it_contin_idx_map it_contin_idx;
disc_map_cit find_disc_knob(const pre_it& it) const {
it_disc_knob_map::const_iterator res = it_disc_knob.find(it);
return res == it_disc_knob.end() ? disc.cend() : res->second;
}
contin_map_cit find_contin_knob(const pre_it& it) const {
it_contin_knob_map::const_iterator res = it_contin_knob.find(it);
return res == it_contin_knob.end() ? contin.cend() : res->second;
}
};
}
}
#endif