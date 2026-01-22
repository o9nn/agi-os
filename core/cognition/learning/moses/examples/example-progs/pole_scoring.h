#ifndef _POLE_SCORING_H
#define _POLE_SCORING_H
#include <opencog/util/numeric.h>
#include <opencog/util/mt19937ar.h>
#include <moses/comboreduct/combo/vertex.h>
#include <moses/comboreduct/combo/simple_nn.h>
#include <moses/comboreduct/reduct/ann_rules.h>
#include "pole_balancing.h"
using namespace opencog;
using namespace combo;
using namespace std;
using namespace moses;
#define MIN_FITNESS -1.0e10
struct AnnPole2NVFitnessFunction
{
double operator()(const combo_tree& tr) const
{
bool velocity = false;
if (tr.empty())
return MIN_FITNESS;
tree_transform tt;
ann nn = tt.decodify_tree(tr);
CartPole the_cart(true,velocity);
the_cart.nmarkov_long=false;
the_cart.generalization_test=false;
double fitness = -100000.0+the_cart.evalNet(&nn);
return fitness;
}
};
struct AnnPole2FitnessFunction
{
double operator()(const combo_tree& tr) const
{
bool velocity = true;
if (tr.empty())
return MIN_FITNESS;
tree_transform tt;
ann nn = tt.decodify_tree(tr);
CartPole the_cart(true,velocity);
the_cart.nmarkov_long=false;
the_cart.generalization_test=false;
double fitness = -100000+the_cart.evalNet(&nn);
return fitness;
}
};
struct AnnPoleFitnessFunction
{
double operator()(const combo_tree& tr) const
{
if (tr.empty())
return MIN_FITNESS;
tree_transform tt;
ann nn = tt.decodify_tree(tr);
return go_cart(&nn,100000);
}
int go_cart(ann *net,int max_steps) const
{
float x,
x_dot,
theta,
theta_dot;
int steps=0,y;
int random_start=1;
double in[5];
double out1;
double out2;
double twelve_degrees=0.2094384;
if (random_start) {
x = (lrand48()%4800)/1000.0 - 2.4;
x_dot = (lrand48()%2000)/1000.0 - 1;
theta = (lrand48()%400)/1000.0 - .2;
theta_dot = (lrand48()%3000)/1000.0 - 1.5;
}
else
x = x_dot = theta = theta_dot = 0.0;
while (steps++ < max_steps) {
in[0]=1.0;
in[1]=(x + 2.4) / 4.8;;
in[2]=(x_dot + .75) / 1.5;
in[3]=(theta + twelve_degrees) / .41;
in[4]=(theta_dot + 1.0) / 2.0;
net->load_inputs(in);
int depth = net->feedforward_depth();
dorepeat(depth)
net->propagate();
out1=net->outputs[0]->activation;
out2=net->outputs[1]->activation;
if (out1 > out2)
y = 0;
else
y = 1;
cart_pole(y, &x, &x_dot, &theta, &theta_dot);
if (x < -2.4 || x > 2.4 || theta < -twelve_degrees ||
theta > twelve_degrees)
return steps;
}
return steps;
}
void cart_pole(int action, float *x,float *x_dot, float *theta, float *theta_dot) const {
float xacc,thetaacc,force,costheta,sintheta,temp;
const float GRAVITY=9.8;
const float MASSCART=1.0;
const float MASSPOLE=0.1;
const float TOTAL_MASS=(MASSPOLE + MASSCART);
const float LENGTH=0.5;
const float POLEMASS_LENGTH=(MASSPOLE * LENGTH);
const float FORCE_MAG=10.0;
const float TAU=0.02;
const float FOURTHIRDS=1.3333333333333;
force = (action>0)? FORCE_MAG : -FORCE_MAG;
costheta = cos(*theta);
sintheta = sin(*theta);
temp = (force + POLEMASS_LENGTH * *theta_dot * *theta_dot * sintheta)
/ TOTAL_MASS;
thetaacc = (GRAVITY * sintheta - costheta* temp)
/ (LENGTH * (FOURTHIRDS - MASSPOLE * costheta * costheta
/ TOTAL_MASS));
xacc = temp - POLEMASS_LENGTH * thetaacc* costheta / TOTAL_MASS;
*x += TAU * *x_dot;
*x_dot += TAU * xacc;
*theta += TAU * *theta_dot;
*theta_dot += TAU * thetaacc;
}
};
#define CPXY_RATIO 1.0
struct ann_pole_bscore : public bscore_base
{
behavioral_score operator()(const combo_tree& tr) const
{
behavioral_score bs;
bs.push_back(pff(tr));
return bs;
}
behavioral_score best_possible_bscore() const
{
return {0.0};
}
complexity_t get_complexity(const combo_tree& tr) const
{
return tr.size();
}
score_t get_complexity_coef() const { return 1.0/CPXY_RATIO; }
AnnPoleFitnessFunction pff;
};
struct ann_pole2_bscore : public bscore_base
{
behavioral_score operator()(const combo_tree& tr) const
{
behavioral_score bs;
bs.push_back(p2ff(tr));
return bs;
}
behavioral_score best_possible_bscore() const
{
return {0.0};
}
complexity_t get_complexity(const combo_tree& tr) const
{
return tr.size();
}
score_t get_complexity_coef() const { return 1.0/CPXY_RATIO; }
AnnPole2FitnessFunction p2ff;
};
struct ann_pole2nv_bscore : public bscore_base
{
behavioral_score operator()(const combo_tree& tr) const
{
behavioral_score bs;
bs.push_back(p2nvff(tr));
return bs;
}
behavioral_score best_possible_bscore() const
{
return {0.0};
}
complexity_t get_complexity(const combo_tree& tr) const
{
return tr.size();
}
score_t get_complexity_coef() const { return 1.0/CPXY_RATIO; }
AnnPole2NVFitnessFunction p2nvff;
};
#endif