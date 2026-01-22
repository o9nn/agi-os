#ifndef _ANT_SCORING_H
#define _ANT_SCORING_H
#include <opencog/util/numeric.h>
#include <opencog/util/mt19937ar.h>
#include <opencog/asmoses/combo/combo/vertex.h>
#include <opencog/asmoses/combo/ant_combo_vocabulary/ant_combo_vocabulary.h>
#include <opencog/asmoses/moses/scoring/scoring_base.h>
using namespace ant_combo;
using namespace opencog::moses;
#define MIN_FITNESS -1.0e10
static const int ANT_X = 32;
static const int ANT_Y = 32;
static const char init_trail[ANT_Y][ANT_X+1] =
{
" 888                            ",
"   8                            ",
"   8                     888    ",
"   8                    8    8  ",
"   8                    8    8  ",
"   8888 88888        88         ",
"            8                8  ",
"            8       8           ",
"            8       8           ",
"            8       8        8  ",
"                    8           ",
"            8                   ",
"            8                8  ",
"            8       8           ",
"            8       8     888   ",
"                 8     8        ",
"                                ",
"            8                   ",
"            8   8       8       ",
"            8   8          8    ",
"            8   8               ",
"            8   8               ",
"            8             8     ",
"            8          8        ",
"   88  88888    8               ",
" 8              8               ",
" 8              8               ",
" 8      8888888                 ",
" 8     8                        ",
"       8                        ",
"  8888                          ",
"                                "
};
struct AntFitnessFunction
{
typedef combo_tree::iterator pre_it;
typedef combo_tree::sibling_iterator sib_it;
enum Direction { north = 0, east = 1, south = 2, west = 3 };
static void turn_left(Direction& d);
static void turn_right(Direction& d);
static void reverse(Direction& d);
AntFitnessFunction(int steps = 600);
score_t operator()(const combo_tree& tr) const;
bool is_turn_left(builtin_action a) const;
bool is_turn_right(builtin_action a) const;
bool is_move_forward(builtin_action a) const;
int eval(sib_it it, int& x, int& y, Direction& facing, int& at_time,
char trail[ANT_Y][ANT_X+1]) const;
private:
const int _steps;
};
struct AntFitnessEstimator : public AntFitnessFunction
{
AntFitnessEstimator(int steps = 600, int noise = 0);
score_t operator()(const combo_tree& tr) const;
private:
const int _noise;
};
struct ant_bscore : public bscore_base
{
ant_bscore() {}
behavioral_score operator()(const combo_tree& tr) const;
behavioral_score best_possible_bscore() const;
private:
AntFitnessFunction _aff;
};
#endif