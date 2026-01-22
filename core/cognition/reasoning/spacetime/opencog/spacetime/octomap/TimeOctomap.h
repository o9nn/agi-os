#ifndef TimeOctomap_H
#define TimeOctomap_H
#include <math.h>
#include <chrono>
#include <cmath>
#include <iostream>
#include <list>
#include <map>
#include <string>
#include <thread>
#include <mutex>
#include <boost/circular_buffer.hpp>
#include "AtomOcTree.h"
namespace opencog
{
using namespace octomap;
typedef std::chrono::system_clock::time_point time_pt;
typedef std::chrono::system_clock::duration duration_c;
typedef std::list<time_pt> time_list;
#define DEG2RAD(deg) ( (M_PI/180.0)*deg )
#define TOUCH_ANGLE DEG2RAD(10.0)
#define NEAR_ANGLE DEG2RAD(20.0)
template <typename T>
struct TimeSlice
{
time_pt t;
duration_c duration;
AtomOcTree<T> map_tree;
std::vector<T> temporal;
TimeSlice(time_pt tp, duration_c d): t(tp), duration(d) {}
bool operator==(time_pt tp)
{
return (t <= tp and tp < t + duration);
}
TimeSlice& operator=(const TimeSlice<T>& tu)
{
t = tu.t;
duration = tu.duration;
map_tree.clear();
return *this;
}
void insert_atom(const point3d& location, const T& ato)
{
map_tree.updateNode(location, true);
map_tree.setNodeData(location, ato);
}
void insert_atom(const T& ato)
{
temporal.push_back(ato);
}
void remove_atom(const T& ato)
{
for (auto it = temporal.begin(); it != temporal.end(); ++it) {
if (*it == ato) {
temporal.erase(it);
}
}
point3d_list pl;
for (typename AtomOcTree<T>::tree_iterator it2 = map_tree.begin_tree(),
endit2 = map_tree.end_tree();
it2 != endit2;
++it2)
{
if (it2->getData() == ato) {
pl.push_back(it2.getCoordinate());
it2->setData(T());
}
}
for (auto& p : pl)
map_tree.deleteNode(p);
}
void remove_atoms_at_location(const point3d& location)
{
map_tree.updateNode(location, false);
}
T get_atom_at_location(const point3d& location)
{
OcTreeNode* result = map_tree.search(location);
if (result == nullptr or not map_tree.isNodeOccupied(result)) return T();
return (static_cast<AtomOcTreeNode<T>*>(result))->getData();
}
point3d_list get_locations(const T& ato)
{
point3d_list pl;
for (typename AtomOcTree<T>::tree_iterator ita = map_tree.begin_tree(),
end = map_tree.end_tree(); ita != end; ++ita) {
if (ita->getData() == ato)
pl.push_back(ita.getCoordinate());
}
return pl;
}
};
template <typename T>
class TimeOctomap
{
public:
double get_space_resolution()
{
return map_res;
}
duration_c get_time_resolution()
{
return time_res;
}
int get_time_units() { return time_circle.capacity(); }
time_pt get_current_time() { return curr_time; }
void step_time_unit()
{
std::lock_guard<std::mutex> lgm(mtx);
curr_time += time_res;
TimeSlice<T> tu(curr_time, time_res);
tu.map_tree.setResolution(map_res);
time_circle.push_back(tu);
}
TimeSlice<T> *find(const time_pt& time_p)
{
for (TimeSlice<T>& tu : time_circle)
if (tu == time_p) return &tu;
return nullptr;
}
TimeSlice<T>& get_current_timeslice()
{
int i = time_circle.capacity() - 1;
if (time_circle.size() < time_circle.capacity())
i = time_circle.size() - 1;
return time_circle[i];
}
bool is_auto_step_time_on()
{
return auto_step;
}
void auto_step_time(bool astep)
{
std::lock_guard<std::mutex> t_mtx(mtx_auto);
if (auto_step == astep) return;
auto_step = astep;
if (astep) auto_timer();
else g_thread.join();
}
void insert_atom(const point3d& location, const T& ato)
{
std::lock_guard<std::mutex> lgm(mtx);
TimeSlice<T>& tu = get_current_timeslice();
tu.insert_atom(location, ato);
}
void insert_atom(const T& ato)
{
std::lock_guard<std::mutex> lgm(mtx);
TimeSlice<T>& tu = get_current_timeslice();
tu.insert_atom(ato);
}
void remove_atoms_at_location(const point3d& location)
{
std::lock_guard<std::mutex> lgm(mtx);
TimeSlice<T>& tu = get_current_timeslice();
tu.remove_atoms_at_location(location);
}
void remove_atom_at_time_by_location(time_pt tp,
const point3d& location)
{
std::lock_guard<std::mutex> lgm(mtx);
auto tu = find(tp);
if (tu == nullptr) return;
tu->remove_atoms_at_location(location);
}
void remove_atom_at_current_time(const T& ato)
{
std::lock_guard<std::mutex> lgm(mtx);
TimeSlice<T>& tu = get_current_timeslice();
tu.remove_atom(ato);
}
void remove_atom_at_time(const time_pt& time_p, const T& ato)
{
std::lock_guard<std::mutex> lgm(mtx);
auto tu = find(time_p);
if (tu == nullptr) return;
tu->remove_atom(ato);
}
void remove_atom(const T& ato)
{
std::lock_guard<std::mutex> lgm(mtx);
for (auto& tu : time_circle) tu.remove_atom(ato);
}
T get_atom_at_location(const point3d& location)
{
std::lock_guard<std::mutex> lgm(mtx);
TimeSlice<T>& tu = get_current_timeslice();
return tu.get_atom_at_location(location);
}
T get_atom_at_time_by_location(const time_pt& time_p,
const point3d& location)
{
std::lock_guard<std::mutex> lgm(mtx);
auto tu = find(time_p);
if (tu == nullptr) return T();
return tu->get_atom_at_location(location);
}
time_list get_times_of_atom_occurence_at_location(const point3d& location,
const T& ato)
{
std::lock_guard<std::mutex> lgm(mtx);
time_list tl;
for (auto& tu : time_circle)
{
T ato_t = tu.get_atom_at_location(location);
if (ato_t != ato) continue;
tl.push_back(tu.t);
}
return tl;
}
time_list get_timeline(const T& ato)
{
std::lock_guard<std::mutex> lgm(mtx);
time_list tl;
for (auto& tu : time_circle) {
for (auto& nod : tu.map_tree) {
if (nod.getData() == ato) {
tl.push_back(tu.t);
break;
}
}
for (auto& data : tu.temporal) {
if (data == ato) {
tl.push_back(tu.t);
break;
}
}
}
return tl;
}
point3d_list get_locations_of_atom(const T& ato)
{
std::lock_guard<std::mutex> lgm(mtx);
TimeSlice<T>& tu = get_current_timeslice();
return tu.get_locations(ato);
}
point3d_list get_locations_of_atom_at_time(const time_pt& time_p,
const T& ato)
{
std::lock_guard<std::mutex> lgm(mtx);
TimeSlice<T> * it = find(time_p);
if (it == nullptr) return point3d_list();
return it->get_locations(ato);
}
bool get_oldest_time_elapse_atom_observed(const T& ato,
const time_pt& from_d,
time_pt& result)
{
time_list tl = get_timeline(ato);
tl.sort();
for (auto& tp : tl) {
if (tp >= from_d) {
result = tp;
return true;
}
}
return false;
}
bool get_last_time_elapse_atom_observed(const T& ato,
const time_pt& from_d,
time_pt& result)
{
time_list tl = get_timeline(ato);
if (0 == tl.size()) return false;
tl.sort();
if (from_d > tl.back() and tl.back() != from_d)
{
return false;
}
result = tl.back();
return true;
}
bool get_last_time_before_elapse_atom_observed(const T& ato,
const time_pt& till_d,
time_pt& result)
{
time_list tl = get_timeline(ato);
if (0 == tl.size()) return false;
tl.sort();
if (till_d < tl.front()) return false;
for (auto& tp : tl) {
if (tp <= till_d) {
result = tp;
return true;
}
}
return false;
}
point3d_list get_oldest_locations(const T& ato, const time_pt& from_d)
{
time_pt tpt;
if (not get_oldest_time_elapse_atom_observed(ato, from_d, tpt))
return point3d_list();
return get_locations_of_atom_at_time(tpt, ato);
}
point3d_list get_newest_locations(const T& ato, const time_pt& till_d)
{
time_pt tpt;
if (not get_last_time_elapse_atom_observed(ato, till_d, tpt))
return point3d_list();
return get_locations_of_atom_at_time(tpt, ato);
}
point3d get_spatial_relations(const time_pt& time_p, const T& ato_obs,
const T& ato_target, const T& ato_ref)
{
point3d res(-1.0, -1.0, -1.0);
point3d v1, v2, v3;
double eps = map_res*0.1;
if (!get_a_location(time_p, ato_obs, v1))
return res;
if (!get_a_location(time_p, ato_target, v2))
return res;
if (!get_a_location(time_p, ato_ref, v3))
return res;
point3d orv = v3 - v1;
if (abs(orv.x()) <= eps && abs(orv.y()) <= eps && abs(orv.z()) <= eps)
return res;
point3d otv = v2 - v1;
double th = atan2(orv.y(), orv.x());
double cx, cy, dx, dy;
rot2d(orv.x(), orv.y(), -1.0*th, cx, cy);
orv = point3d(cx, 0.0, orv.z());
rot2d(otv.x(), otv.y(), -1.0*th, dx, dy);
otv = point3d(dx, dy, otv.z());
th = atan2(orv.z(), orv.x());
rot2d(orv.x(), orv.z(), -1.0*th, cx, cy);
orv = point3d(cx, 0.0, 0.0);
rot2d(otv.x(), otv.z(), -1.0*th, dx, dy);
otv = point3d(dx, otv.y(), dy);
res = otv - orv;
double px, py, pz;
if (res.x() > eps)
px = 1.0;
else if (res.x() < -1.0*eps)
px = 2.0;
else
px = 0.0;
if (res.y() > eps)
py = 2.0;
else if (res.y() < -1.0*eps)
py = 1.0;
else
py = 0.0;
if (res.z() > eps)
pz = 2.0;
else if (res.z() < -1.0*eps)
pz = 1.0;
else
pz = 0.0;
res = point3d(px, py, pz);
return res;
}
bool get_direction_vector(const time_pt& time_p, const T& ato_obs,
const T& ato_target, point3d& dir)
{
point3d tarh;
point3d refh;
if (!get_a_location(time_p, ato_target, tarh))
return false;
if (!get_a_location(time_p, ato_obs, refh))
return false;
dir = tarh - refh;
return true;
}
int get_angular_nearness(const time_pt& time_p, const T& ato_obs,
const T& ato_target, const T& ato_ref)
{
point3d dir1, dir2;
if (not get_direction_vector(time_p, ato_obs, ato_target, dir1))
return -1;
if (not get_direction_vector(time_p, ato_obs, ato_ref, dir2))
return -1;
double ang = ang_vec(dir1, dir2);
if (ang <= TOUCH_ANGLE)
return 0;
else if (ang <= NEAR_ANGLE)
return 1;
return 2;
}
double get_distance_between(const time_pt& time_p, const T& ato_target,
const T& ato_ref)
{
point3d tarh;
point3d refh;
if (!get_a_location(time_p, ato_target, tarh))
return (-1.0);
if (!get_a_location(time_p, ato_ref, refh))
return (-1.0);
double dist = sqrt(sqr(tarh.x()-refh.x())+sqr(tarh.y()-refh.y())+sqr(tarh.z()-refh.z()));
return dist;
}
bool get_a_location(const time_pt& time_p, const T& ato_target, point3d& location)
{
point3d_list target_list = get_locations_of_atom_at_time(time_p, ato_target);
if (target_list.size() < 1)
return false;
location = target_list.front();
return true;
}
public:
TimeOctomap(unsigned int num_time_units,
double map_res_meters,
duration_c time_resolution) :
map_res(map_res_meters),
time_res(time_resolution),
time_circle(num_time_units),
auto_step(false)
{
curr_time = std::chrono::system_clock::now();
TimeSlice<T> tu(curr_time, time_res);
tu.map_tree.setResolution(map_res);
time_circle.push_back(tu);
}
~TimeOctomap()
{
auto_step_time(false);
}
inline double sqr(double a) { return (a*a); }
inline double dot(point3d a, point3d b) {
return (a.x()*b.x()+a.y()*b.y()+a.z()*b.z());
}
inline double mag(point3d a) {
return sqrt(sqr(a.x())+sqr(a.y())+sqr(a.z()));
}
inline double ang_vec(point3d a, point3d b)
{
double num = dot(a, b);
double den = mag(a)*mag(b);
double diff = abs(mag(a) - mag(b));
if (den < 1e-9)
{
if (diff < 1e-3)
return 0.0;
else
return M_PI;
}
return acos(num/den);
}
inline void rot2d(double x, double y, double th, double &rx, double &ry)
{
rx = x*cos(th) - y*sin(th);
ry = x*sin(th) + y*cos(th);
}
private:
double map_res;
duration_c time_res;
boost::circular_buffer<TimeSlice<T> > time_circle;
time_pt curr_time;
void auto_timer()
{
duration_c tr = time_res;
g_thread = std::thread(
[tr, this] () {
while (this->is_auto_step_time_on()) {
std::this_thread::sleep_for(tr);
this->step_time_unit(); } });
}
bool auto_step;
std::mutex mtx, mtx_auto;
std::thread g_thread;
};
}
namespace std
{
ostream& operator<<(ostream&, const opencog::time_pt&);
ostream& operator<<(ostream&, const opencog::duration_c&);
ostream& operator<<(ostream&, const opencog::time_list&);
ostream& operator<<(ostream&, const octomap::point3d_list&);
}
#endif