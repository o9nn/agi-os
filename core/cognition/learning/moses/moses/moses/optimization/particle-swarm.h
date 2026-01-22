#ifndef _MOSES_PARTICLE_SWARM_H
#define _MOSES_PARTICLE_SWARM_H
#include <opencog/util/oc_assert.h>
#include "../representation/instance_set.h"
#include "optimization.h"
namespace opencog { namespace moses {
typedef std::vector<double> velocity;
struct ps_parameters
{
ps_parameters()
: max_parts(50),
bit_c1(0.7),
disc_c1(2.05),
cont_c1(0.7),
bit_c2(1.43),
disc_c2(2.05),
cont_c2(1.43),
inertia(0.7),
bit_min_value(0),
bit_max_value(1),
disc_min_value(0),
disc_max_value(1),
cont_min_value(std::numeric_limits<int>::min()),
cont_max_value(std::numeric_limits<int>::max()),
bit_min_vel(-6),
bit_max_vel(6),
disc_min_vel(-disc_max_value/2),
disc_max_vel(disc_max_value/2),
cont_min_vel(std::numeric_limits<int>::min() / 2),
cont_max_vel(std::numeric_limits<int>::max() / 2),
range_bit_vel(bit_max_vel - bit_min_vel),
range_cont_vel(cont_max_vel - cont_min_vel)
{
double fi = disc_c1 + disc_c2;
constriction_disc = 2 / (2 - fi - std::sqrt((fi * fi) - (4 * fi)));
}
unsigned max_parts;
double bit_c1, disc_c1, cont_c1;
double bit_c2, disc_c2, cont_c2;
double inertia;
double bit_min_value, bit_max_value,
disc_min_value, disc_max_value,
cont_min_value, cont_max_value;
double bit_min_vel, bit_max_vel,
disc_min_vel, disc_max_vel,
cont_min_vel, cont_max_vel;
double range_bit_vel, range_cont_vel;
double constriction_disc;
};
struct particle_swarm : optimizer_base
{
particle_swarm(const optim_parameters& op = optim_parameters(),
const ps_parameters& ps = ps_parameters())
: optimizer_base(op), _total_RAM_bytes(getTotalRAM()), ps_params(ps) {}
protected:
const uint64_t _total_RAM_bytes;
size_t _instance_bytes;
const ps_parameters ps_params;
struct discrete_particles {
std::vector<std::vector<double>> temp, best_personal;
discrete_particles(unsigned part_size, unsigned disc_size) :
temp(part_size, std::vector<double>(disc_size)),
best_personal(part_size, std::vector<double>(disc_size)) {}
};
void log_stats_legend();
unsigned calc_swarm_size (const field_set& fs);
void initialize_particles (const unsigned& swarm_size,
deme_t& best_parts, std::vector<velocity>& velocities,
discrete_particles& disc_parts, const field_set& fields);
void initialize_random_particle (instance& new_inst, velocity& vel,
std::vector<double>& dist_values, const field_set& fs);
void check_bit_vel(double &vel) {
vel = clamp(vel, ps_params.bit_min_vel, ps_params.bit_max_vel); }
void check_disc_vel(double &vel) {
vel = clamp(vel, ps_params.disc_min_vel, ps_params.disc_max_vel); }
void check_cont_vel(double &vel) {
vel = clamp(vel, ps_params.cont_min_vel, ps_params.cont_max_vel); }
double gen_bit_vel() {
return (randGen().randdouble() *
ps_params.range_bit_vel) - ps_params.bit_max_vel;
}
double gen_disc_vel() {
return (randGen().randdouble() - ps_params.disc_max_vel);
}
double gen_cont_vel() {
return (randGen().randdouble() *
ps_params.range_cont_vel ) - ps_params.cont_max_vel;
}
bool gen_bit_value() {
return randGen().randbool(); }
double gen_disc_value() {
return randGen().randdouble(); }
double gen_cont_value() {
return (randGen().randdouble() *
ps_params.range_cont_vel) + ps_params.cont_min_vel;
}
void confinement_disc(double& value) {
value = clamp(value, ps_params.disc_min_value, ps_params.disc_max_value);
}
void confinement_cont(double& value) {
value = clamp(value, ps_params.cont_min_value, ps_params.cont_max_value);
}
void update_particles(deme_t& temp_parts, const deme_t& best_parts, const int& best_index,
std::vector<velocity>& velocities, discrete_particles& disc_parts, const field_set& fields);
void update_bit_vel(double& vel, int&& temp,
int&& personal, int&& global) {
vel += (randGen().randdouble() * (personal - temp)) +
(randGen().randdouble() * (global - temp));
check_bit_vel(vel);
}
bool new_bit_value(const double& vel){
return (randGen().randdouble() <
(1 / (1 + std::exp(-vel))));
}
void update_bit_particle(instance& temp, const instance& personal,
const instance& global, velocity::iterator vel, const field_set& fs);
void update_disc_vel(double& vel, const double& temp,
const double& personal, const double& global) {
vel += (ps_params.disc_c1 * randGen().randdouble() * (personal - temp)) +
(ps_params.disc_c2 * randGen().randdouble() * (global - temp));
vel = vel * ps_params.constriction_disc;
check_disc_vel(vel);
}
disc_t cont2disc(double& cvalue, const unsigned max_dvalue){
return (disc_t) std::round(cvalue * (max_dvalue - 1));
}
disc_t new_disc_value(double& cvalue,
const double& vel, const unsigned max_dvalue){
cvalue += vel;
confinement_disc(cvalue);
return cont2disc(cvalue, max_dvalue);
}
void update_disc_particle(instance& dtemp, std::vector<double>& temp,
const std::vector<double>& personal, const std::vector<double>& global,
velocity::iterator vel, const field_set& fs);
void update_cont_vel(double& vel, const double& temp,
const double& personal, const double& global) {
vel += ps_params.inertia * ((ps_params.cont_c1 * randGen().randdouble() * (personal - temp)) +
(ps_params.cont_c2 * randGen().randdouble() * (global - temp)));
check_cont_vel(vel);
}
contin_t new_cont_value(const contin_t& value, const double& vel){
contin_t res = value + vel;
confinement_cont(res);
return res;
}
void update_cont_particle(instance& temp, const instance& personal,
const instance& global, velocity::iterator vel, const field_set& fs);
public:
void operator()(deme_t& deme,
const instance& init_inst,
const iscorer_base& iscorer,
unsigned max_evals,
time_t max_time);
void operator()(deme_t& deme,
const iscorer_base& iscorer,
unsigned max_evals,
time_t max_time)
{
instance init_inst(deme.fields().packed_width());
operator()(deme, init_inst, iscorer, max_evals, max_time);
}
};
}
}
#endif