#ifndef POLEBALANCINGINC
#define POLEBALANCINGINC
#include <moses/comboreduct/combo/simple_nn.h>
using namespace opencog;
class CartPole {
public:
CartPole(bool randomize,bool velocity);
virtual ~CartPole() {}
virtual void simplifyTask();
virtual void nextTask();
virtual double evalNet(combo::ann *net);
double maxFitness;
bool MARKOV;
bool last_hundred;
bool nmarkov_long;
bool generalization_test;
double state[6];
double jigglestep[1000];
protected:
virtual void init(bool randomize);
private:
void performAction(double output,int stepnum);
void step(double action, double *state, double *derivs);
void rk4(double f, double y[], double dydx[], double yout[]);
bool outsideBounds();
const static int NUM_INPUTS;
const static double MUP ;
const static double MUC ;
const static double GRAVITY;
const static double MASSCART;
const static double MASSPOLE_1;
const static double LENGTH_1;
const static double FORCE_MAG;
const static double TAU;
const static double one_degree;
const static double six_degrees;
const static double twelve_degrees;
const static double fifteen_degrees;
const static double thirty_six_degrees;
const static double fifty_degrees;
double LENGTH_2;
double MASSPOLE_2;
double MIN_INC;
double POLE_INC;
double MASS_INC;
int balanced_sum;
double cartpos_sum;
double cartv_sum;
double polepos_sum;
double polev_sum;
};
#endif