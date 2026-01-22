#ifndef _OPENCOG_NEIGHBORHOOD_SAMPLING_H
#define _OPENCOG_NEIGHBORHOOD_SAMPLING_H
#include <iostream>
#include <algorithm>
#include <limits>
#include <boost/math/special_functions/binomial.hpp>
#include <boost/numeric/conversion/cast.hpp>
#include <opencog/util/dorepeat.h>
#include <opencog/util/lazy_random_selector.h>
#include "../eda/initialization.h"
#include "../representation/instance_set.h"
#include "../moses/types.h"
namespace opencog { namespace moses {
template<typename Out>
void generate_initial_sample(const field_set& fs, int n, Out out, Out end)
{
dorepeat(n) {
instance inst(fs.packed_width());
randomize(fs, inst);
for (field_set::bit_iterator it = fs.begin_bit(inst);
it != fs.end_bit(inst); ++it)
if (randGen().randbool())
*it = false;
for (field_set::disc_iterator it = fs.begin_disc(inst);
it != fs.end_disc(inst); ++it)
if (randGen().randbool())
*it = 0;
OC_ASSERT(out != end);
*out++ = inst;
}
}
void flip_LR(field_set::disc_iterator itr);
void twiddle_contin_bit(field_set::disc_iterator itr,
field_set::disc_iterator next_itr,
opencog::RandGen& rng = randGen());
void generate_contin_neighbor(const field_set& fs,
instance& inst,
field_set::contin_iterator it,
unsigned dist,
opencog::RandGen& rng = randGen());
template<typename Out>
void sample_from_neighborhood(const field_set& fs, unsigned dist,
unsigned sample_size, Out out, Out end,
const instance & center_inst)
{
OC_ASSERT(center_inst.size() == fs.packed_width(),
"Please make sure that the center_inst"
" have the same size with the field_set");
unsigned dim = fs.dim_size();
OC_ASSERT(dist <= dim,
"the sampling distance %u"
" cannot be greater than the field dimension %u", dist, dim);
dorepeat(sample_size) {
instance new_inst(center_inst);
lazy_random_selector select(dim, randGen());
for (unsigned i = 1; i <= dist; ) {
size_t r = select();
if (r < fs.n_bits()) {
field_set::bit_iterator itb = fs.begin_bit(new_inst);
itb += r;
*itb = !(*itb);
i++;
} else if (r >= fs.n_bits() && (r < (fs.n_bits() + fs.n_disc_fields()))) {
field_set::disc_iterator itd = fs.begin_disc(new_inst);
itd += r - fs.n_bits();
disc_t temp = 1 + randGen().randint(itd.multy() - 1);
if ( *itd == temp)
*itd = 0;
else
*itd = temp;
i++;
} else if ( r >= (fs.n_bits() + fs.n_disc_fields())) {
field_set::contin_iterator itc = fs.begin_contin(new_inst);
itc += r - fs.n_bits() - fs.n_disc_fields();
generate_contin_neighbor(fs, new_inst, itc, 1);
i++;
}
}
OC_ASSERT(out != end);
*out++ = new_inst;
}
}
template<typename Out>
void generate_all_in_neighborhood(const field_set& fs, unsigned dist,
Out out, Out end,
const instance& center_inst)
{
OC_ASSERT(center_inst.size() == fs.packed_width(),
"the size of center_instance should be equal to the width of fs");
vary_n_knobs(fs, center_inst, dist, 0, out, end);
}
template<typename Out>
void generate_all_in_neighborhood(const field_set& fs,
unsigned dist, Out out, Out end)
{
instance inst(fs.packed_width());
generate_all_in_neighborhood(fs, dist, out, end, inst);
}
template<typename Out>
Out vary_n_knobs(const field_set& fs,
const instance& inst,
unsigned dist,
unsigned starting_index,
Out out, Out end)
{
if (dist == 0) {
OC_ASSERT(out != end);
*out++ = inst;
return out;
}
instance tmp_inst = inst;
if ((fs.begin_term_raw_idx() <= starting_index) &&
(starting_index < fs.end_term_raw_idx()))
{
out = vary_n_knobs(fs, tmp_inst, dist,
starting_index + fs.end_term_raw_idx(),
out, end);
}
else
if ((fs.begin_contin_raw_idx() <= starting_index) &&
(starting_index < fs.end_contin_raw_idx()))
{
field_set::contin_iterator itc = fs.begin_contin(tmp_inst);
size_t contin_idx = fs.raw_to_contin_idx(starting_index);
itc += contin_idx;
size_t depth = fs.contin()[itc.idx()].depth;
size_t length = fs.contin_length(tmp_inst, contin_idx);
field_set::disc_iterator itr = fs.begin_raw(tmp_inst);
itr += starting_index;
size_t relative_raw_idx = starting_index - fs.contin_to_raw_idx(contin_idx);
if (*itr == field_set::contin_spec::Stop) {
size_t next_contin = starting_index + depth - relative_raw_idx;
out = vary_n_knobs(fs, tmp_inst, dist, next_contin, out, end);
*itr = field_set::contin_spec::Left;
out = vary_n_knobs(fs, tmp_inst, dist - 1, starting_index + 1, out, end);
*itr = field_set::contin_spec::Right;
out = vary_n_knobs(fs, tmp_inst, dist - 1, starting_index + 1, out, end);
}
else
{
out = vary_n_knobs(fs, tmp_inst, dist, starting_index + 1, out, end);
*itr = field_set::contin_spec::switchLR(*itr);
out = vary_n_knobs(fs, tmp_inst, dist - 1, starting_index + 1, out, end);
unsigned remRLs = length - relative_raw_idx;
if (remRLs <= dist) {
for(; relative_raw_idx < length; --length, ++itr) {
*itr = field_set::contin_spec::Stop;
}
size_t next_contin = starting_index + depth - relative_raw_idx;
out = vary_n_knobs(fs, tmp_inst, dist - remRLs, next_contin,
out, end);
}
}
}
else
if ((fs.begin_disc_raw_idx() <= starting_index) &&
(starting_index < fs.end_disc_raw_idx()))
{
field_set::disc_iterator itd = fs.begin_disc(tmp_inst);
itd += fs.raw_to_disc_idx(starting_index);
#define UNROLL_TAIL_CALL_DISC 1
#ifdef UNROLL_TAIL_CALL_DISC
if (1 == dist) {
unsigned end_idx = fs.end_disc_raw_idx();
out = vary_n_knobs(fs, tmp_inst, dist, end_idx, out, end);
for ( ; starting_index < end_idx; starting_index++) {
OC_ASSERT(out != end, "Write past end of array!");
disc_t tmp_val = *itd;
for (unsigned i = 1; i <= itd.multy() - 1; ++i) {
if (tmp_val == i)
*itd = 0;
else
*itd = i;
*out++ = tmp_inst;
}
*itd = tmp_val;
itd ++;
}
return out;
}
#endif
out = vary_n_knobs(fs, tmp_inst, dist, starting_index + 1, out, end);
disc_t tmp_val = *itd;
for (unsigned i = 1; i <= itd.multy() - 1; ++i) {
if (tmp_val == i)
*itd = 0;
else
*itd = i;
out = vary_n_knobs(fs, tmp_inst, dist - 1, starting_index + 1, out, end);
}
}
else
if ((fs.begin_bit_raw_idx() <= starting_index) &&
(starting_index < fs.end_bit_raw_idx()))
{
field_set::bit_iterator itb = fs.begin_bit(tmp_inst);
itb += starting_index - fs.begin_bit_raw_idx();
#define UNROLL_TAIL_CALL 1
#ifdef UNROLL_TAIL_CALL
if (1 == dist) {
unsigned end_idx = fs.end_bit_raw_idx();
for ( ; starting_index < end_idx; starting_index++) {
OC_ASSERT(out != end, "Write past end of array!");
*itb = !(*itb);
*out++ = tmp_inst;
*itb = !(*itb);
itb ++;
}
return out;
}
#endif
out = vary_n_knobs(fs, tmp_inst, dist, starting_index + 1, out, end);
*itb = !(*itb);
out = vary_n_knobs(fs, tmp_inst, dist - 1, starting_index + 1, out, end);
}
else
{
}
return out;
}
size_t safe_binomial_coefficient(unsigned k, unsigned n);
size_t count_neighborhood_size_from_index(const field_set& fs,
const instance& inst,
unsigned dist,
unsigned starting_index,
size_t max_count
= std::numeric_limits<size_t>::max());
size_t count_neighborhood_size(const field_set& fs,
const instance& inst,
unsigned dist,
size_t max_count
= std::numeric_limits<size_t>::max());
size_t count_neighborhood_size(const field_set& fs,
unsigned dist,
size_t max_count
= std::numeric_limits<size_t>::max());
size_t sample_new_instances(size_t total_number_of_neighbours,
size_t number_of_new_instances,
size_t current_number_of_instances,
const instance& center_inst,
instance_set<composite_score>& deme,
unsigned dist);
size_t sample_new_instances(size_t number_of_new_instances,
size_t current_number_of_instances,
const instance& center_inst,
instance_set<composite_score>& deme,
unsigned dist);
}
}
#endif