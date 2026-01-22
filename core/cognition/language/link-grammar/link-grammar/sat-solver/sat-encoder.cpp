#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <vector>
#include <algorithm>
#include <iterator>
using std::cout;
using std::cerr;
using std::endl;
using std::vector;
extern "C" {
#include "sat-encoder.h"
}
#include "minisat/core/Solver.h"
#include "sat-encoder.hpp"
#include "variables.hpp"
#include "word-tag.hpp"
#include "matrix-ut.hpp"
#include "clock.hpp"
#include "fast-sprintf.hpp"
extern "C" {
#include "disjunct-utils.h"
#include "error.h"
#include "linkage/analyze-linkage.h"
#include "linkage/linkage.h"
#include "linkage/sane.h"
#include "linkage/score.h"
#include "parse/prune.h"
#include "prepare/build-disjuncts.h"
#include "post-process/post-process.h"
#include "post-process/pp-structures.h"
#include "tokenize/word-structures.h"
#include "tokenize/tok-structures.h"
}
#ifdef SAT_DEBUG
#define DEBUG_print(x) (cout << x << endl)
#else
#define DEBUG_print(x)
#endif
#define D_SAT 5
#define N(s) ((s) ? (s) : "(null)")
void SATEncoder::generate_literal(Lit l) {
vec<Lit> clause(1);
clause[0] = l;
add_clause(clause);
}
void SATEncoder::generate_and_definition(Lit lhs, vec<Lit>& rhs) {
vec<Lit> clause(2);
for (int i = 0; i < rhs.size(); i++) {
clause[0] = ~lhs;
clause[1] = rhs[i];
add_clause(clause);
}
for (int i = 0; i < rhs.size(); i++) {
clause[0] = ~rhs[i];
clause[1] = lhs;
add_clause(clause);
}
}
void SATEncoder::generate_classical_and_definition(Lit lhs, vec<Lit>& rhs) {
{
vec<Lit> clause(2);
for (int i = 0; i < rhs.size(); i++) {
clause[0] = ~lhs;
clause[1] = rhs[i];
add_clause(clause);
}
}
{
vec<Lit> clause(rhs.size() + 1);
for (int i = 0; i < rhs.size(); i++) {
clause[i] = ~rhs[i];
}
clause[rhs.size()] = lhs;
add_clause(clause);
}
}
void SATEncoder::generate_or_definition(Lit lhs, vec<Lit>& rhs) {
{
vec<Lit> clause(2);
for (int i = 0; i < rhs.size(); i++) {
clause[0] = lhs;
clause[1] = ~rhs[i];
add_clause(clause);
}
}
{
vec<Lit> clause(rhs.size() + 1);
for (int i = 0; i < rhs.size(); i++) {
clause[i] = rhs[i];
}
clause[rhs.size()] = ~lhs;
add_clause(clause);
}
}
#if 0
void SATEncoder::generate_conditional_lr_implication_or_definition(Lit condition, Lit lhs, vec<Lit>& rhs) {
{
vec<Lit> clause(2);
for (int i = 0; i < rhs.size(); i++) {
clause[0] = lhs;
clause[1] = ~rhs[i];
add_clause(clause);
}
}
{
vec<Lit> clause(rhs.size() + 2);
for (int i = 0; i < rhs.size(); i++) {
clause[i] = rhs[i];
}
clause[rhs.size()] = ~lhs;
clause[rhs.size()+1] = ~condition;
add_clause(clause);
}
}
#endif
#if 0
void SATEncoder::generate_conditional_lr_implication_or_definition(Lit condition1, Lit condition2, Lit lhs, vec<Lit>& rhs) {
{
vec<Lit> clause(2);
for (int i = 0; i < rhs.size(); i++) {
clause[0] = lhs;
clause[1] = ~rhs[i];
add_clause(clause);
}
}
{
vec<Lit> clause(rhs.size() + 3);
for (int i = 0; i < rhs.size(); i++) {
clause[i] = rhs[i];
}
clause[rhs.size()] = ~lhs;
clause[rhs.size()+1] = ~condition1;
clause[rhs.size()+2] = ~condition2;
add_clause(clause);
}
}
#endif
void SATEncoder::generate_xor_conditions(vec<Lit>& vect) {
vec<Lit> clause(2);
for (int i = 0; i < vect.size(); i++) {
for (int j = i + 1; j < vect.size(); j++) {
if (vect[i] == vect[j])
continue;
clause[0] = ~vect[i];
clause[1] = ~vect[j];
add_clause(clause);
}
}
}
void SATEncoder::generate_and(vec<Lit>& vect) {
for (int i = 0; i < vect.size(); i++) {
generate_literal(vect[i]);
}
}
void SATEncoder::generate_or(vec<Lit>& vect) {
add_clause(vect);
}
void SATEncoder::generate_equivalence_definition(Lit l1, Lit l2) {
vec<Lit> clause(2);
{
clause[0] = ~l1;
clause[1] = l2;
add_clause(clause);
}
{
clause[0] = l1;
clause[1] = ~l2;
add_clause(clause);
}
}
void SATEncoder::encode() {
Clock clock;
generate_satisfaction_conditions();
clock.print_time(verbosity, "Generated satisfaction conditions");
generate_linked_definitions();
clock.print_time(verbosity, "Generated linked definitions");
generate_planarity_conditions();
clock.print_time(verbosity, "Generated planarity conditions");
#ifdef _CONNECTIVITY_
generate_connectivity();
clock.print_time(verbosity, "Generated connectivity");
#endif
generate_encoding_specific_clauses();
pp_prune();
clock.print_time(verbosity, "PP pruned");
power_prune();
clock.print_time(verbosity, "Power pruned");
_variables->setVariableParameters(_solver);
}
void SATEncoder::build_word_tags()
{
char name[MAX_VARIABLE_NAME];
name[0] = 'w';
for (size_t w = 0; w < _sent->length; w++) {
fast_sprintf(name+1, (int)w);
Var var = _variables->string(name);
assert((Var)w == var, "Word %zu: var %d", w, var);
}
for (size_t w = 0; w < _sent->length; w++) {
_word_tags.push_back(WordTag(w, _variables, _sent, _opts));
int dfs_position = 0;
if (_sent->word[w].x == NULL) continue;
bool join = _sent->word[w].x->next != NULL;
Exp* exp = join ? join_alternatives(w) : _sent->word[w].x->exp;
#ifdef SAT_DEBUG
cout << "Word ." << w << ".: " << N(_sent->word[w].unsplit_word) << endl;
cout << lg_exp_stringify(exp);
cout << endl;
#endif
fast_sprintf(name+1, (int)w);
bool leading_right = true;
bool leading_left = true;
std::vector<int> eps_right, eps_left;
_word_tags[w].insert_connectors(exp, dfs_position, leading_right,
leading_left, eps_right, eps_left, name, true, 0, NULL, _sent->word[w].x);
}
for (size_t wl = 0; wl < _sent->length - 1; wl++) {
for (size_t wr = wl + 1; wr < _sent->length; wr++) {
_word_tags[wl].add_matches_with_word(_word_tags[wr]);
}
}
}
#if 0
void SATEncoder::find_all_matches_between_words(size_t w1, size_t w2,
std::vector<std::pair<const PositionConnector*, const PositionConnector*> >& matches) {
const std::vector<PositionConnector>& w1_right = _word_tags[w1].get_right_connectors();
std::vector<PositionConnector>::const_iterator i;
for (i = w1_right.begin(); i != w1_right.end(); i++) {
const std::vector<PositionConnector*>& w2_left_c = (*i).matches;
std::vector<PositionConnector*>::const_iterator j;
for (j = w2_left_c.begin(); j != w2_left_c.end(); j++) {
if ((*j)->word == w2) {
matches.push_back(std::pair<const PositionConnector*, const PositionConnector*>(&(*i), *j));
}
}
}
}
bool SATEncoder::matches_in_interval(int wi, int pi, int l, int r) {
for (int w = l; w < r; w++) {
if (_word_tags[w].match_possible(wi, pi))
return true;
}
return false;
}
#endif
void SATEncoder::generate_satisfaction_conditions()
{
char name[MAX_VARIABLE_NAME] = "w";
for (size_t w = 0; w < _sent->length; w++) {
#ifdef SAT_DEBUG
cout << "Word " << w << ": " << N(_sent->word[w].unsplit_word);
if (_sent->word[w].optional) cout << " (optional)";
cout << endl;
cout << lg_exp_stringify(exp);
cout << endl;
#endif
fast_sprintf(name+1, w);
if (_sent->word[w].optional)
_variables->string(name);
else
determine_satisfaction(w, name);
if (_sent->word[w].x == NULL) {
if (!_sent->word[w].optional) {
lgdebug(+D_SAT, "Word%zu '%s': Null X_node\n", w, _sent->word[w].unsplit_word);
handle_null_expression(w);
}
continue;
}
bool join = _sent->word[w].x->next != NULL;
Exp* exp = join ? join_alternatives(w) : _sent->word[w].x->exp;
int dfs_position = 0;
generate_satisfaction_for_expression(w, dfs_position, exp, name, 0);
}
}
void SATEncoder::generate_satisfaction_for_expression(int w, int& dfs_position, Exp* e,
char* var, double parent_cost)
{
Exp *opd;
double total_cost = parent_cost + e->cost;
if (e->type == CONNECTOR_type) {
dfs_position++;
generate_satisfaction_for_connector(w, dfs_position, e, var);
if (total_cost > _cost_cutoff) {
Lit lhs = Lit(_variables->string_cost(var, e->cost));
generate_literal(~lhs);
}
} else {
if (e->type == AND_type) {
if (e->operand_first == NULL) {
_variables->string_cost(var, e->cost);
if (total_cost > _cost_cutoff) {
generate_literal(~Lit(_variables->string_cost(var, e->cost)));
}
} else if (e->operand_first->operand_next == NULL) {
generate_satisfaction_for_expression(w, dfs_position, e->operand_first, var, total_cost);
} else {
int i;
char new_var[MAX_VARIABLE_NAME];
char* last_new_var = new_var;
char* last_var = var;
while((*last_new_var = *last_var)) {
last_new_var++;
last_var++;
}
vec<Lit> rhs;
for (i = 0, opd=e->operand_first; opd!=NULL; opd=opd->operand_next, i++) {
char* s = last_new_var;
*s++ = 'c';
fast_sprintf(s, i);
rhs.push(Lit(_variables->string(new_var)));
}
Lit lhs = Lit(_variables->string_cost(var, e->cost));
generate_and_definition(lhs, rhs);
int dfs_position_tmp = dfs_position;
for (opd = e->operand_first; opd->operand_next != NULL; opd = opd->operand_next) {
Exp e_tmp, *e_rhs = &e_tmp;
if (opd->operand_next->operand_next == NULL) {
e_rhs = opd->operand_next;
}
else {
e_rhs->type = AND_type;
e_rhs->operand_first = opd->operand_next;
e_rhs->cost = 0.0;
};
generate_conjunct_order_constraints(w, opd, e_rhs, dfs_position_tmp);
}
for (i = 0, opd=e->operand_first; opd!=NULL; opd=opd->operand_next, i++) {
char* s = last_new_var;
*s++ = 'c';
fast_sprintf(s, i);
generate_satisfaction_for_expression(w, dfs_position, opd, new_var, total_cost);
}
}
} else if (e->type == OR_type) {
if (e->operand_first == NULL) {
cerr << "Zeroary OR" << endl;
exit(EXIT_FAILURE);
} else if (e->operand_first->operand_next == NULL) {
generate_satisfaction_for_expression(w, dfs_position, e->operand_first, var, total_cost);
} else {
int i;
char new_var[MAX_VARIABLE_NAME];
char* last_new_var = new_var;
char* last_var = var;
while ((*last_new_var = *last_var)) {
last_new_var++;
last_var++;
}
vec<Lit> rhs;
for (i = 0, opd=e->operand_first; opd!=NULL; opd=opd->operand_next, i++) {
char* s = last_new_var;
*s++ = 'd';
fast_sprintf(s, i);
rhs.push(Lit(_variables->string(new_var)));
}
Lit lhs = Lit(_variables->string_cost(var, e->cost));
generate_or_definition(lhs, rhs);
generate_xor_conditions(rhs);
for (i = 0, opd=e->operand_first; opd!=NULL; opd=opd->operand_next, i++) {
char* s = last_new_var;
*s++ = 'd';
fast_sprintf(s, i);
generate_satisfaction_for_expression(w, dfs_position, opd, new_var, total_cost);
}
}
}
}
}
Exp* SATEncoder::join_alternatives(int w)
{
Exp* exp = Exp_create(_sent->Exp_pool);
exp->type = OR_type;
exp->cost = 0.0;
Exp** opdp = &exp->operand_first;
for (X_node* x = _sent->word[w].x; x != NULL; x = x->next)
{
*opdp = Exp_create_dup(_sent->Exp_pool, x->exp);
opdp = &(*opdp)->operand_next;
}
*opdp = NULL;
return exp;
}
void SATEncoder::generate_link_cw_ordinary_definition(size_t wi, int pi,
Exp* e, size_t wj)
{
const char* Ci = e->condesc->more->string;
char dir = e->dir;
double cost = e->cost;
Lit lhs = Lit(_variables->link_cw(wj, wi, pi, Ci));
vec<Lit> rhs;
std::vector<PositionConnector*>& matches = _word_tags[wi].get(pi)->matches;
std::vector<PositionConnector*>::const_iterator i;
for (i = matches.begin(); i != matches.end(); i++) {
if ((*i)->word != wj)
continue;
if (dir == '+') {
rhs.push(Lit(_variables->link_cost(wi, pi, Ci, e,
(*i)->word, (*i)->position,
connector_string(&(*i)->connector),
(*i)->exp,
cost + (*i)->cost)));
} else if (dir == '-'){
rhs.push(Lit(_variables->link((*i)->word, (*i)->position,
connector_string(&(*i)->connector),
(*i)->exp,
wi, pi, Ci, e)));
}
}
DEBUG_print("--------- link_cw as ordinary down");
generate_or_definition(lhs, rhs);
generate_xor_conditions(rhs);
DEBUG_print("--------- end link_cw as ordinary down");
}
int SATEncoder::num_connectors(Exp* e)
{
if (e->type == CONNECTOR_type)
return 1;
else {
int num = 0;
for (Exp* opd = e->operand_first; opd != NULL; opd = opd->operand_next) {
num += num_connectors(opd);
}
return num;
}
}
int SATEncoder::empty_connectors(Exp* e, char dir)
{
if (e->type == CONNECTOR_type) {
return e->dir != dir;
} else if (e->type == OR_type) {
for (Exp* opd = e->operand_first; opd != NULL; opd = opd->operand_next) {
if (empty_connectors(opd, dir))
return true;
}
return false;
} else if (e->type == AND_type) {
for (Exp* opd = e->operand_first; opd != NULL; opd = opd->operand_next) {
if (!empty_connectors(opd, dir))
return false;
}
return true;
} else
throw std::string("Unknown connector type");
}
#if 0
int SATEncoder::non_empty_connectors(Exp* e, char dir)
{
if (e->type == CONNECTOR_type) {
return e->dir == dir;
} else if (e->type == OR_type) {
for (E_list* l = e->operand_first; l != NULL; l = l->next) {
if (non_empty_connectors(l->e, dir))
return true;
}
return false;
} else if (e->type == AND_type) {
for (E_list* l = e->operand_first; l != NULL; l = l->next) {
if (non_empty_connectors(l->e, dir))
return true;
}
return false;
} else
throw std::string("Unknown connector type");
}
#endif
bool SATEncoder::trailing_connectors_and_aux(int w, Exp *opd, char dir, int& dfs_position,
std::vector<PositionConnector*>& connectors)
{
if (opd == NULL) {
return true;
} else {
int dfs_position_in = dfs_position;
dfs_position += num_connectors(opd);
if (trailing_connectors_and_aux(w, opd->operand_next, dir, dfs_position, connectors)) {
trailing_connectors(w, opd, dir, dfs_position_in, connectors);
}
return empty_connectors(opd, dir);
}
}
void SATEncoder::trailing_connectors(int w, Exp* exp, char dir, int& dfs_position,
std::vector<PositionConnector*>& connectors)
{
if (exp->type == CONNECTOR_type) {
dfs_position++;
if (exp->dir == dir) {
connectors.push_back(_word_tags[w].get(dfs_position));
}
} else if (exp->type == OR_type) {
for (Exp* opd = exp->operand_first; opd != NULL; opd = opd->operand_next) {
trailing_connectors(w, opd, dir, dfs_position, connectors);
}
} else if (exp->type == AND_type) {
trailing_connectors_and_aux(w, exp->operand_first, dir, dfs_position, connectors);
}
}
#if 0
void SATEncoder::certainly_non_trailing(int w, Exp* exp, char dir, int& dfs_position,
std::vector<PositionConnector*>& connectors, bool has_right) {
if (exp->type == CONNECTOR_type) {
dfs_position++;
if (exp->dir == dir && has_right) {
connectors.push_back(_word_tags[w].get(dfs_position));
}
} else if (exp->type == OR_type) {
for (E_list* l = exp->operand_first; l != NULL; l = l->next) {
certainly_non_trailing(w, l->e, dir, dfs_position, connectors, has_right);
}
} else if (exp->type == AND_type) {
if (has_right) {
for (E_list* l = exp->operand_first; l != NULL; l = l->next) {
certainly_non_trailing(w, l->e, dir, dfs_position, connectors, true);
}
} else {
for (E_list* l = exp->operand_first; l != NULL; l = l->next) {
has_right = false;
for (E_list* m = l->next; m != NULL; m = m->next) {
if (non_empty_connectors(m->e, dir) && !empty_connectors(m->e, dir)) {
has_right = true;
break;
}
}
certainly_non_trailing(w, l->e, dir, dfs_position, connectors, has_right);
}
}
}
}
#endif
void SATEncoder::leading_connectors(int w, Exp* exp, char dir, int& dfs_position, vector<PositionConnector*>& connectors) {
if (exp->type == CONNECTOR_type) {
dfs_position++;
if (exp->dir == dir) {
connectors.push_back(_word_tags[w].get(dfs_position));
}
} else if (exp->type == OR_type) {
for (Exp* opd = exp->operand_first; opd != NULL; opd = opd->operand_next) {
leading_connectors(w, opd, dir, dfs_position, connectors);
}
} else if (exp->type == AND_type) {
Exp* opd;
for (opd = exp->operand_first; opd != NULL; opd = opd->operand_next) {
leading_connectors(w, opd, dir, dfs_position, connectors);
if (!empty_connectors(opd, dir))
break;
}
if (opd != NULL) {
for (opd = opd->operand_next; opd != NULL; opd = opd->operand_next)
dfs_position += num_connectors(opd);
}
}
}
void SATEncoder::generate_conjunct_order_constraints(int w, Exp *e1, Exp* e2, int& dfs_position) {
DEBUG_print("----- conjunct order constraints");
int dfs_position_e1 = dfs_position;
std::vector<PositionConnector*> last_right_in_e1, first_right_in_e2;
trailing_connectors(w, e1, '+', dfs_position_e1, last_right_in_e1);
int dfs_position_e2 = dfs_position_e1;
leading_connectors(w, e2, '+', dfs_position_e2, first_right_in_e2);
vec<Lit> clause(2);
if (!last_right_in_e1.empty() && !first_right_in_e2.empty()) {
std::vector<PositionConnector*>::iterator i, j;
for (i = last_right_in_e1.begin(); i != last_right_in_e1.end(); i++) {
std::vector<PositionConnector*>& matches_e1 = (*i)->matches;
for (j = first_right_in_e2.begin(); j != first_right_in_e2.end(); j++) {
std::vector<PositionConnector*>& matches_e2 = (*j)->matches;
std::vector<PositionConnector*>::const_iterator m1, m2;
std::vector<int> mw1;
for (m1 = matches_e1.begin(); m1 != matches_e1.end(); m1++) {
if ((m1+1) == matches_e1.end() || (*m1)->word != (*(m1 + 1))->word)
mw1.push_back((*m1)->word);
}
std::vector<int> mw2;
for (m2 = matches_e2.begin(); m2 != matches_e2.end(); m2++) {
if ((m2+1) == matches_e2.end() || (*m2)->word != (*(m2 + 1))->word)
mw2.push_back((*m2)->word);
}
std::vector<int>::const_iterator mw1i, mw2i;
for (mw1i = mw1.begin(); mw1i != mw1.end(); mw1i++) {
for (mw2i = mw2.begin(); mw2i != mw2.end(); mw2i++) {
if (*mw1i >= *mw2i) {
clause[0] = ~Lit(_variables->link_cw(*mw1i, w, (*i)->position, connector_string(&(*i)->connector)));
clause[1] = ~Lit(_variables->link_cw(*mw2i, w, (*j)->position, connector_string(&(*j)->connector)));
add_clause(clause);
}
}
}
}
}
}
DEBUG_print("----");
dfs_position_e1 = dfs_position;
std::vector<PositionConnector*> last_left_in_e1, first_left_in_e2;
trailing_connectors(w, e1, '-', dfs_position_e1, last_left_in_e1);
dfs_position_e2 = dfs_position_e1;
leading_connectors(w, e2, '-', dfs_position_e2, first_left_in_e2);
if (!last_left_in_e1.empty() && !first_left_in_e2.empty()) {
std::vector<PositionConnector*>::iterator i, j;
for (i = last_left_in_e1.begin(); i != last_left_in_e1.end(); i++) {
std::vector<PositionConnector*>& matches_e1 = (*i)->matches;
for (j = first_left_in_e2.begin(); j != first_left_in_e2.end(); j++) {
std::vector<PositionConnector*>& matches_e2 = (*j)->matches;
std::vector<PositionConnector*>::const_iterator m1, m2;
std::vector<int> mw1;
for (m1 = matches_e1.begin(); m1 != matches_e1.end(); m1++) {
if ((m1+1) == matches_e1.end() || (*m1)->word != (*(m1 + 1))->word)
mw1.push_back((*m1)->word);
}
std::vector<int> mw2;
for (m2 = matches_e2.begin(); m2 != matches_e2.end(); m2++) {
if ((m2+1) == matches_e2.end() || (*m2)->word != (*(m2 + 1))->word)
mw2.push_back((*m2)->word);
}
std::vector<int>::const_iterator mw1i, mw2i;
for (mw1i = mw1.begin(); mw1i != mw1.end(); mw1i++) {
for (mw2i = mw2.begin(); mw2i != mw2.end(); mw2i++) {
if (*mw1i <= *mw2i) {
clause[0] = ~Lit(_variables->link_cw(*mw1i, w, (*i)->position, connector_string(&(*i)->connector)));
clause[1] = ~Lit(_variables->link_cw(*mw2i, w, (*j)->position, connector_string(&(*j)->connector)));
add_clause(clause);
}
}
}
}
}
}
dfs_position = dfs_position_e1;
DEBUG_print("---end conjunct order constraints");
}
#ifdef _CONNECTIVITY_
void SATEncoder::generate_connectivity() {
for (int w = 1; w < _sent->length; w++) {
if (!_linkage_possible(0, w)) {
vec<Lit> clause;
clause.push(~Lit(_variables->con(w, 1)));
generate_and(clause);
}
else {
generate_equivalence_definition(Lit(_variables->con(w, 1)),
Lit(_variables->linked(0, w)));
}
}
for (int d = 2; d < _sent->length; d++) {
for (int w = 1; w < _sent->length; w++) {
Lit lhs = Lit(_variables->con(w, d));
vec<Lit> rhs;
rhs.push(Lit(_variables->con(w, d-1)));
for (int w1 = 1; w1 < _sent->length; w1++) {
if (w == w1)
continue;
if (!_linkage_possible(std::min(w, w1), std::max(w, w1))) {
continue;
}
rhs.push(Lit(_variables->l_con(w, w1, d-1)));
}
generate_or_definition(lhs, rhs);
for (int w1 = 1; w1 < _sent->length; w1++) {
if (w == w1)
continue;
int mi = std::min(w, w1), ma = std::max(w, w1);
if (!_linked_possible(mi, ma))
continue;
Lit lhs = Lit(_variables->l_con(w, w1, d-1));
vec<Lit> rhs(2);
rhs[0] = Lit(_variables->linked(mi, ma));
rhs[1] = Lit(_variables->con(w1, d-1));
generate_classical_and_definition(lhs, rhs);
}
}
}
for (int w = 1; w < _sent->length; w++) {
generate_literal(Lit(_variables->con(w, _sent->length-1)));
}
}
#endif
void SATEncoder::dfs(int node, const MatrixUpperTriangle<int>& graph, int component, std::vector<int>& components) {
if (components[node] != -1)
return;
components[node] = component;
for (size_t other_node = node + 1; other_node < components.size(); other_node++) {
if (graph(node, other_node))
dfs(other_node, graph, component, components);
}
for (int other_node = 0; other_node < node; other_node++) {
if (graph(other_node, node))
dfs(other_node, graph, component, components);
}
}
#ifdef CONNECTIVITY_DEBUG
#undef CONNECTIVITY_DEBUG
#define CONNECTIVITY_DEBUG(x) x
static void debug_generate_disconnectivity_prohibiting(vector<int> components,
std::vector<int> different_components) {
printf("generate_disconnectivity_prohibiting: ");
for (auto c: components) cout << c << " ";
cout << endl;
for (auto c: different_components) cout << c << " ";
cout << endl;
}
#else
#define debug_generate_disconnectivity_prohibiting(x, y)
#define CONNECTIVITY_DEBUG(x)
#endif
bool SATEncoder::connectivity_components(std::vector<int>& components) {
const std::vector<int>& linked_variables = _variables->linked_variables();
std::vector<int> satisfied_linked_variables;
for (std::vector<int>::const_iterator i = linked_variables.begin(); i != linked_variables.end(); i++) {
int var = *i;
if (_solver->model[var] == l_True) {
satisfied_linked_variables.push_back(var);
}
}
std::vector<bool> is_linked_word(_sent->length, false);
for (size_t node = 0; node < _sent->length; node++) {
is_linked_word[node] = _solver->model[node] == l_True;
}
MatrixUpperTriangle<int> graph(_sent->length, 0);
std::vector<int>::const_iterator i;
CONNECTIVITY_DEBUG(printf("connectivity_components words:\n"));
for (i = satisfied_linked_variables.begin(); i != satisfied_linked_variables.end(); i++) {
const int lv_l = _variables->linked_variable(*i)->left_word;
const int lv_r = _variables->linked_variable(*i)->right_word;
if (!is_linked_word[lv_l] || !is_linked_word[lv_r]) continue;
graph.set(lv_l, lv_r, 1);
CONNECTIVITY_DEBUG(printf("L%d R%d\n", lv_l, lv_r));
}
components.resize(_sent->length);
std::fill(components.begin(), components.end(), -1);
for (size_t node = 0; node < _sent->length; node++)
dfs(node, graph, node, components);
CONNECTIVITY_DEBUG(printf("connectivity_components: "));
bool connected = true;
for (size_t node = 0; node < _sent->length; node++) {
CONNECTIVITY_DEBUG(
if (is_linked_word[node]) printf("%d ", components[node]);
else                      printf("[%d] ", components[node]);
)
if (is_linked_word[node]) {
if (components[node] != 0) {
connected = false;
}
} else {
components[node] = -1;
}
}
CONNECTIVITY_DEBUG(printf(" connected=%d\n", connected));
return connected;
}
#ifdef SAT_DEBUG
#define MVALUE(s, v) (s->model[v] == l_True?'T':(s->model[v] == l_False?'f':'u'))
GNUC_UNUSED static void pmodel(Solver *solver, int var) {
printf("%c\n", MVALUE(solver, var));
}
GNUC_UNUSED static void pmodel(Solver *solver, vec<Lit> &clause) {
vector<int> t;
for (int i = 0; i < clause.size(); i++) {
int v = var(clause[i]);
printf("%d:%c ", v, MVALUE(solver, v));
if (solver->model[v] == l_True) t.push_back(v);
}
printf("\n");
if (t.size()) {
printf("l_True:");
for (auto i: t) printf(" %d", i);
printf("\n");
}
}
#endif
void SATEncoder::generate_disconnectivity_prohibiting(std::vector<int> components) {
std::vector<int> different_components = components;
std::sort(different_components.begin(), different_components.end());
different_components.erase(std::unique(different_components.begin(), different_components.end()),
different_components.end());
debug_generate_disconnectivity_prohibiting(components, different_components);
std::vector<int>::const_iterator c;
bool missing_word = false;
if (*different_components.begin() == -1) {
different_components.erase(different_components.begin());
missing_word = true;
}
for (c = different_components.begin(); c != different_components.end(); c++) {
vec<Lit> clause;
const std::vector<int>& linked_variables = _variables->linked_variables();
for (std::vector<int>::const_iterator i = linked_variables.begin(); i != linked_variables.end(); i++) {
int var = *i;
const Variables::LinkedVar* lv = _variables->linked_variable(var);
if ((components[lv->left_word] == *c && components[lv->right_word] != *c) ||
(components[lv->left_word] != *c && components[lv->right_word] == *c)) {
CONNECTIVITY_DEBUG(printf(" %d(%d-%d)", var, lv->left_word, lv->right_word));
bool optlw_exists = _sent->word[lv->left_word].optional &&
_solver->model[lv->left_word] == l_True;
bool optrw_exists = _sent->word[lv->right_word].optional &&
_solver->model[lv->right_word] == l_True;
if (optlw_exists || optrw_exists) {
int conditional_link_var;
bool conditional_link_var_exists;
CONNECTIVITY_DEBUG(printf("R "));
char name[MAX_VARIABLE_NAME] = "0";
sprintf(name+1, "%dw%dw%d", var, optlw_exists?lv->left_word:255,
optrw_exists?lv->right_word:255);
conditional_link_var_exists = _variables->var_exists(name);
conditional_link_var = _variables->string(name);
if (!conditional_link_var_exists) {
Lit lhs = Lit(conditional_link_var);
vec<Lit> rhs;
rhs.push(Lit(var));
if (optlw_exists) rhs.push(~Lit(lv->left_word));
if (optrw_exists) rhs.push(~Lit(lv->right_word));
generate_or_definition(lhs, rhs);
}
var = conditional_link_var;
}
clause.push(Lit(var));
}
}
if (missing_word) {
for (WordIdx w = 0; w < _sent->length; w++) {
if (_solver->model[w] == l_False)
clause.push(Lit(w));
}
}
CONNECTIVITY_DEBUG(printf("\n"));
_solver->addClause(clause);
if (different_components.size() == 2)
break;
}
}
void SATEncoder::generate_planarity_conditions()
{
vec<Lit> clause(2);
for (size_t wi1 = 0; wi1 < _sent->length; wi1++) {
for (size_t wj1 = wi1+1; wj1 < _sent->length; wj1++) {
for (size_t wi2 = wj1+1; wi2 < _sent->length; wi2++) {
if (!_linked_possible(wi1, wi2))
continue;
for (size_t wj2 = wi2+1; wj2 < _sent->length; wj2++) {
if (!_linked_possible(wj1, wj2))
continue;
clause[0] = ~Lit(_variables->linked(wi1, wi2));
clause[1] = ~Lit(_variables->linked(wj1, wj2));
add_clause(clause);
}
}
}
}
}
void SATEncoder::generate_epsilon_definitions()
{
for (size_t w = 0; w < _sent->length; w++) {
if (_sent->word[w].x == NULL) {
continue;
}
bool join = _sent->word[w].x->next != NULL;
Exp* exp = join ? join_alternatives(w) : _sent->word[w].x->exp;
char name[MAX_VARIABLE_NAME];
name[0] = 'w';
fast_sprintf(name+1, w);
int dfs_position;
dfs_position = 0;
generate_epsilon_for_expression(w, dfs_position, exp, name, true, '+');
dfs_position = 0;
generate_epsilon_for_expression(w, dfs_position, exp, name, true, '-');
}
}
bool SATEncoder::generate_epsilon_for_expression(int w, int& dfs_position, Exp* e,
char* var, bool root, char dir)
{
if (e->type == CONNECTOR_type) {
dfs_position++;
if (var != NULL) {
if (e->dir == dir) {
return false;
} else {
generate_equivalence_definition(Lit(_variables->epsilon(var, dir)),
Lit(_variables->string(var)));
return true;
}
}
} else if (e->type == AND_type) {
if (e->operand_first == NULL) {
generate_equivalence_definition(Lit(_variables->string(var)),
Lit(_variables->epsilon(var, dir)));
return true;
} else
if (e->operand_first != NULL && e->operand_first->operand_next == NULL) {
return generate_epsilon_for_expression(w, dfs_position, e->operand_first, var, root, dir);
} else {
Exp* opd;
int i;
bool eps = true;
char new_var[MAX_VARIABLE_NAME];
char* last_new_var = new_var;
char* last_var = var;
while ((*last_new_var = *last_var)) {
last_new_var++;
last_var++;
}
for (i = 0, opd = e->operand_first; opd != NULL; opd = opd->operand_next, i++) {
char* s = last_new_var;
*s++ = 'c';
fast_sprintf(s, i);
if (!generate_epsilon_for_expression(w, dfs_position, opd, new_var, false, dir)) {
eps = false;
break;
}
}
if (opd != NULL) {
for (opd = opd->operand_next; opd != NULL; opd = opd->operand_next)
dfs_position += num_connectors(opd);
}
if (!root && eps) {
Lit lhs = Lit(_variables->epsilon(var, dir));
vec<Lit> rhs;
for (i = 0, opd = e->operand_first; opd != NULL; opd = opd->operand_next, i++) {
char* s = last_new_var;
*s++ = 'c';
fast_sprintf(s, i);
rhs.push(Lit(_variables->epsilon(new_var, dir)));
}
generate_classical_and_definition(lhs, rhs);
}
return eps;
}
} else if (e->type == OR_type) {
if (e->operand_first != NULL && e->operand_first->operand_next == NULL) {
return generate_epsilon_for_expression(w, dfs_position, e->operand_first, var, root, dir);
} else {
Exp* opd;
int i;
bool eps = false;
char new_var[MAX_VARIABLE_NAME];
char* last_new_var = new_var;
char* last_var = var;
while ((*last_new_var = *last_var)) {
last_new_var++;
last_var++;
}
vec<Lit> rhs;
for (i = 0, opd = e->operand_first; opd != NULL; opd = opd->operand_next, i++) {
char* s = last_new_var;
*s++ = 'd';
fast_sprintf(s, i);
if (generate_epsilon_for_expression(w, dfs_position, opd, new_var, false, dir) && !root) {
rhs.push(Lit(_variables->epsilon(new_var, dir)));
eps = true;
}
}
if (!root && eps) {
Lit lhs = Lit(_variables->epsilon(var, dir));
generate_or_definition(lhs, rhs);
}
return eps;
}
}
return false;
}
void SATEncoder::power_prune()
{
generate_epsilon_definitions();
for (size_t wl = 0; wl < _sent->length - 2; wl++) {
const std::vector<PositionConnector>& rc = _word_tags[wl].get_right_connectors();
std::vector<PositionConnector>::const_iterator rci;
for (rci = rc.begin(); rci != rc.end(); rci++) {
if (!(*rci).leading_right || (*rci).connector.multi)
continue;
const std::vector<PositionConnector*>& matches = rci->matches;
for (std::vector<PositionConnector*>::const_iterator lci = matches.begin(); lci != matches.end(); lci++) {
if (!(*lci)->leading_left || (*lci)->connector.multi || (*lci)->word <= wl + 2)
continue;
if (_opts->min_null_count == 0)
if (optional_gap_collapse(_sent, wl, (*lci)->word)) continue;
vec<Lit> clause;
for (std::vector<int>::const_iterator i = rci->eps_right.begin(); i != rci->eps_right.end(); i++) {
clause.push(~Lit(*i));
}
for (std::vector<int>::const_iterator i = (*lci)->eps_left.begin(); i != (*lci)->eps_left.end(); i++) {
clause.push(~Lit(*i));
}
add_additional_power_pruning_conditions(clause, wl, (*lci)->word);
clause.push(~Lit(_variables->link(
wl, rci->position, connector_string(&rci->connector), rci->exp,
(*lci)->word, (*lci)->position, connector_string(&(*lci)->connector), (*lci)->exp)));
add_clause(clause);
}
}
}
#if 0
for (size_t wl = 0; wl < _sent->length - 2; wl++) {
const std::vector<PositionConnector>& rc = _word_tags[wl].get_right_connectors();
std::vector<PositionConnector>::const_iterator rci;
for (rci = rc.begin(); rci != rc.end(); rci++) {
if (!(*rci).leading_right)
continue;
const std::vector<PositionConnector*>& matches = rci->matches;
for (std::vector<PositionConnector*>::const_iterator lci = matches.begin(); lci != matches.end(); lci++) {
if (!(*lci)->leading_left || (*lci)->word != wl + 1)
continue;
int link = _variables->link(wl, rci->position, rci->connector.desc->string, rci->exp,
(*lci)->word, (*lci)->position, (*lci)->connector.desc->string, (*lci)->exp);
vec<Lit> clause(2);
clause.push(~Lit(link));
for (std::vector<int>::const_iterator i = rci->eps_right.begin(); i != rci->eps_right.end(); i++) {
clause.push(Lit(*i));
}
for (std::vector<int>::const_iterator i = (*lci)->eps_left.begin(); i != (*lci)->eps_left.end(); i++) {
clause.push(Lit(*i));
}
add_clause(clause);
}
}
}
#endif
#if 0
std::vector<std::vector<PositionConnector*> > certainly_deep_left(_sent->length), certainly_deep_right(_sent->length);
for (size_t w = 0; w < _sent->length - 2; w++) {
if (_sent->word[w].x == NULL)
continue;
bool join = _sent->word[w].x->next != NULL;
Exp* exp = join ? join_alternatives(w) : _sent->word[w].x->exp;
int dfs_position = 0;
certainly_non_trailing(w, exp, '+', dfs_position, certainly_deep_right[w], false);
dfs_position = 0;
certainly_non_trailing(w, exp, '-', dfs_position, certainly_deep_left[w], false);
}
for (size_t w = 0; w < _sent->length; w++) {
std::vector<PositionConnector*>::const_iterator i;
for (i = certainly_deep_right[w].begin(); i != certainly_deep_right[w].end(); i++) {
const std::vector<PositionConnector*>& matches = (*i)->matches;
std::vector<PositionConnector*>::const_iterator j;
for (j = matches.begin(); j != matches.end(); j++) {
if (std::find(certainly_deep_left[(*j)->word].begin(), certainly_deep_left[(*j)->word].end(),
*j) != certainly_deep_left[(*j)->word].end()) {
generate_literal(~Lit(_variables->link((*i)->word, (*i)->position, (*i)->connector.desc->string, (*i)->exp,
(*j)->word, (*j)->position, (*j)->connector.desc->string, (*j)->exp)));
}
}
}
}
#endif
}
#if 0
static bool string_in_list(const char * s, const char * a[])
{
for (size_t i = 0; a[i] != NULL; i++)
if (post_process_match(a[i], s)) return true;
return false;
}
#endif
void SATEncoder::pp_prune()
{
const std::vector<int>& link_variables = _variables->link_variables();
if (_sent->dict->base_knowledge == NULL)
return;
if (!_opts->perform_pp_prune)
return;
pp_knowledge * knowledge;
knowledge = _sent->dict->base_knowledge;
for (size_t i=0; i<knowledge->n_contains_one_rules; i++)
{
pp_rule rule = knowledge->contains_one_rules[i];
pp_linkset * link_set = rule.link_set;
vec<Lit> triggers;
for (size_t vi = 0; vi < link_variables.size(); vi++) {
const Variables::LinkVar* var = _variables->link_variable(link_variables[vi]);
if (post_process_match(rule.selector, var->label)) {
triggers.push(Lit(link_variables[vi]));
}
}
if (triggers.size() == 0)
continue;
vec<Lit> criterions;
for (size_t j = 0; j < link_variables.size(); j++) {
pp_linkset_node *p;
for (size_t hashval = 0; hashval < link_set->hash_table_size; hashval++) {
for (p = link_set->hash_table[hashval]; p!=NULL; p=p->next) {
const Variables::LinkVar* var = _variables->link_variable(link_variables[j]);
if (post_process_match(p->str, var->label)) {
criterions.push(Lit(link_variables[j]));
}
}
}
}
DEBUG_print("---pp_pruning--");
for (int k = 0; k < triggers.size(); k++) {
vec<Lit> clause(criterions.size() + 1);
for (int ci = 0; ci < criterions.size(); ci++)
clause[ci] = criterions[ci];
clause[criterions.size()] = (~triggers[k]);
add_clause(clause);
}
DEBUG_print("---end pp_pruning--");
}
#if 0
if (test_enabled("no-pp_pruning_1")) return;
for (size_t i=0; i<knowledge->n_contains_none_rules; i++)
{
pp_rule rule = knowledge->contains_none_rules[i];
int root_link;
int wr;
for (size_t vi = 0; vi < link_variables.size(); vi++)
{
const Variables::LinkVar* var_i = _variables->link_variable(link_variables[vi]);
if (!post_process_match(rule.selector, var_i->label)) continue;
root_link = link_variables[vi];
wr = var_i->right_word;
vector<int> must_not_exist;
for (size_t vj = 0; vj < link_variables.size(); vj++)
{
const Variables::LinkVar* var_j = _variables->link_variable(link_variables[vj]);
if ((wr != var_j->left_word) && (wr != var_j->right_word)) continue;
if (var_i == var_j) continue;
if (string_in_list(var_j->label, rule.link_array))
must_not_exist.push_back(link_variables[vj]);
}
DEBUG_print("---pp_pruning 1--");
for (auto c: must_not_exist)
{
vec<Lit> clause(2);
clause[0] = ~Lit(root_link);
clause[1] = ~Lit(c);
add_clause(clause);
}
DEBUG_print("---end pp_pruning 1--");
}
}
#endif
}
bool SATEncoder::create_linkage(Linkage linkage)
{
partial_init_linkage(_sent, linkage, _sent->length);
if (!sat_extract_links(linkage)) return false;
compute_link_names(linkage, _sent->string_set);
return true;
}
void SATEncoder::generate_linkage_prohibiting()
{
vec<Lit> clause;
const std::vector<int>& link_variables = _variables->link_variables();
for (std::vector<int>::const_iterator i = link_variables.begin(); i != link_variables.end(); i++) {
int var = *i;
if (_solver->model[var] == l_True) {
clause.push(~Lit(var));
} else if (_solver->model[var] == l_False) {
clause.push(Lit(var));
}
}
_solver->addClause(clause);
}
Linkage SATEncoder::get_next_linkage()
{
Linkage_s linkage;
bool connected;
bool display_linkage_disconnected = false;
bool linkage_ok;
do {
if (!_solver->solve()) return NULL;
std::vector<int> components;
connected = connectivity_components(components);
if (!connected) {
_num_lkg_disconnected++;
generate_disconnectivity_prohibiting(components);
display_linkage_disconnected = test_enabled("linkage-disconnected");
} else {
generate_linkage_prohibiting();
}
linkage_ok = false;
if (connected || display_linkage_disconnected) {
memset(&linkage, 0, sizeof(struct Linkage_s));
linkage_ok = create_linkage(&linkage);
if (linkage_ok)
linkage_ok = sane_linkage_morphism(_sent, &linkage, _opts);
if (!linkage_ok) {
free_linkage_connectors_and_disjuncts(&linkage);
free_linkage(&linkage);
continue;
}
remove_empty_words(&linkage);
}
if (!connected) {
if (display_linkage_disconnected) {
cout << "Linkage DISCONNECTED" << endl;
} else {
lgdebug(+D_SAT, "Linkage DISCONNECTED (skipped)\n");
}
}
} while (!linkage_ok);
if (NULL == _sent->lnkages)
{
_sent->num_linkages_alloced = _opts->linkage_limit;
size_t nbytes = _sent->num_linkages_alloced * sizeof(struct Linkage_s);
_sent->lnkages = (Linkage) malloc(nbytes);
_next_linkage_index = 0;
}
assert(_next_linkage_index<_sent->num_linkages_alloced, "_sent->lnkages ovl");
Linkage lkg = &_sent->lnkages[_next_linkage_index];
_next_linkage_index++;
*lkg = linkage;
if (_next_linkage_index < _sent->num_linkages_alloced)
lkg[1].lifo.N_violations = 0;
if (NULL != _sent->postprocessor) {
do_post_process(_sent->postprocessor, lkg, false);
if (NULL != _sent->postprocessor->violation) {
_num_pp_violations++;
lkg->lifo.N_violations++;
lkg->lifo.pp_violation_msg = _sent->postprocessor->violation;
lgdebug(+D_SAT, "Postprocessing error: %s\n", lkg->lifo.pp_violation_msg);
} else {
}
post_process_free_data(&_sent->postprocessor->pp_data);
}
linkage_score(lkg, _opts);
return lkg;
}
void SATEncoderConjunctionFreeSentences::generate_encoding_specific_clauses() {
}
void SATEncoderConjunctionFreeSentences::handle_null_expression(int w) {
vec<Lit> clause;
add_clause(clause);
}
void SATEncoderConjunctionFreeSentences::determine_satisfaction(int w, char* name)
{
generate_literal(Lit(_variables->string(name)));
}
void SATEncoderConjunctionFreeSentences::generate_satisfaction_for_connector(
int wi, int pi, Exp *e, char* var)
{
const char* Ci = e->condesc->more->string;
char dir = e->dir;
bool multi = e->multi;
double cost = e->cost;
Lit lhs = Lit(_variables->string_cost(var, cost));
#ifdef SAT_DEBUG
cout << "*** Connector: ." << wi << ". ." << pi << ". " << Ci << dir << endl;
#endif
int low = (dir == '-') ? 0 : wi + 1;
int hi  = (dir == '-') ? wi : _sent->length;
std::vector<int> _w_;
for (int wj = low; wj < hi; wj++) {
if (!_word_tags[wj].match_possible(wi, pi))
continue;
_w_.push_back(wj);
generate_link_cw_ordinary_definition(wi, pi, e, wj);
}
vec<Lit> _link_cw_;
for (size_t i = 0; i < _w_.size(); i++)
_link_cw_.push(Lit(_variables->link_cw(_w_[i], wi, pi, Ci)));
generate_or_definition(lhs, _link_cw_);
DEBUG_print("--------- multi");
if (!multi) {
generate_xor_conditions(_link_cw_);
}
DEBUG_print("--------- end multi");
#ifdef SAT_DEBUG
cout << "*** End Connector: ." << wi << ". ." << pi << ". " <<  Ci << endl;
#endif
}
void SATEncoderConjunctionFreeSentences::generate_linked_definitions()
{
_linked_possible.resize(_sent->length, 1);
vector<vec<Lit>> linked_to_word(_sent->length);
DEBUG_print("------- linked definitions");
for (size_t w1 = 0; w1 < _sent->length; w1++) {
vec<Lit> linked;
for (size_t w2 = w1 + 1; w2 < _sent->length; w2++) {
DEBUG_print("---------- ." << w1 << ". ." << w2 << ".");
Lit lhs = Lit(_variables->linked(w1, w2));
vec<Lit> rhs;
const std::vector<PositionConnector>& w1_connectors = _word_tags[w1].get_right_connectors();
std::vector<PositionConnector>::const_iterator c;
for (c = w1_connectors.begin(); c != w1_connectors.end(); c++) {
assert(c->word == w1, "Connector word must match");
if (_word_tags[w2].match_possible(c->word, c->position)) {
rhs.push(Lit(_variables->link_cw(w2, c->word, c->position, connector_string(&c->connector))));
}
}
_linked_possible.set(w1, w2, rhs.size() > 0);
generate_or_definition(lhs, rhs);
if (rhs.size() > 0) {
if (_sent->word[w1].optional) {
linked_to_word[w1].push(Lit(_variables->linked(w1, w2)));
}
if (_sent->word[w2].optional) {
linked_to_word[w2].push(Lit(_variables->linked(w1, w2)));
}
}
}
if (_sent->word[w1].optional) {
DEBUG_print("------------S not linked -> no word (w" << w1 << ")");
generate_or_definition(Lit(w1), linked_to_word[w1]);
DEBUG_print("------------E not linked -> no word");
}
}
DEBUG_print("------- end linked definitions");
}
Exp* SATEncoderConjunctionFreeSentences::PositionConnector2exp(const PositionConnector* pc)
{
Exp* e = Exp_create(_sent->Exp_pool);
e->type = CONNECTOR_type;
e->dir = pc->dir;
e->multi = pc->connector.multi;
e->condesc = (condesc_t *)pc->connector.desc;
e->cost = pc->cost;
return e;
}
#define D_SEL 8
bool SATEncoderConjunctionFreeSentences::sat_extract_links(Linkage lkg)
{
Disjunct *d;
uint32_t current_link = 0;
Exp **exp_word = (Exp **)alloca(_sent->length * sizeof(Exp *));
memset(exp_word, 0, _sent->length * sizeof(Exp *));
const X_node **xnode_word = (const X_node **)alloca(_sent->length * sizeof(X_node *));
memset(xnode_word, 0, _sent->length * sizeof(X_node *));
const std::vector<int>& link_variables = _variables->link_variables();
std::vector<int>::const_iterator i;
for (i = link_variables.begin(); i != link_variables.end(); i++) {
if (_solver->model[*i] != l_True)
continue;
const Variables::LinkVar* var = _variables->link_variable(*i);
if (_solver->model[_variables->linked(var->left_word, var->right_word)] != l_True)
continue;
assert(current_link < lkg->lasz, "Linkage array too small!");
Link& clink = lkg->link_array[current_link];
current_link++;
clink.lw = var->left_word;
clink.rw = var->right_word;
PositionConnector* lpc = _word_tags[var->left_word].get(var->left_position);
PositionConnector* rpc = _word_tags[var->right_word].get(var->right_position);
const X_node *left_xnode = lpc->word_xnode;
const X_node *right_xnode = rpc->word_xnode;
clink.lc = connector_new(NULL, NULL);
clink.rc = connector_new(NULL, NULL);
*clink.lc = lpc->connector;
*clink.rc = rpc->connector;
Exp* lcexp = PositionConnector2exp(lpc);
Exp* rcexp = PositionConnector2exp(rpc);
add_anded_exp(_sent, exp_word[var->left_word], lcexp);
add_anded_exp(_sent, exp_word[var->right_word], rcexp);
if (verbosity_level(D_SAT)) {
cout<< "w"<<var->left_word<<" LCexp[" <<left_xnode->word->subword <<"]: ", lg_exp_stringify(lcexp);
cout<< "w"<<var->right_word<<" RCexp[" <<right_xnode->word->subword <<"]: ", lg_exp_stringify(rcexp);
cout<< "L+L: ", lg_exp_stringify(exp_word[var->left_word]);
cout<< "R+R: ", lg_exp_stringify(exp_word[var->right_word]);
}
if (xnode_word[var->left_word] && xnode_word[var->left_word] != left_xnode) {
lgdebug(+0, "Warning: Inconsistent X_node for word %d (%s and %s)\n",
var->left_word, xnode_word[var->left_word]->string,
left_xnode->string);
}
if (xnode_word[var->right_word] && xnode_word[var->right_word] != right_xnode) {
lgdebug(+0, "Warning: Inconsistent X_node for word %d (%s and %s)\n",
var->right_word, xnode_word[var->right_word]->string,
right_xnode->string);
}
xnode_word[var->left_word] = left_xnode;
xnode_word[var->right_word] = right_xnode;
}
lkg->num_links = current_link;
for (WordIdx wi = 0; wi < _sent->length; wi++) {
Exp *de = exp_word[wi];
if (xnode_word[wi] == NULL)
{
if (!_sent->word[wi].optional)
{
de = null_exp();
prt_error("Error: Internal error: Non-optional word %zu has no linkage\n", wi);
}
continue;
}
if (de == NULL) {
de = null_exp();
prt_error("Error: Internal error: No expression for word %zu\n", wi);
}
double cost_cutoff;
#if LIMIT_TOTAL_LINKAGE_COST
cost_cutoff = _opts->disjunct_cost;
#else
cost_cutoff = 1000.0;
#endif
d = build_disjuncts_for_exp(_sent, de, xnode_word[wi]->string,
&xnode_word[wi]->word->gword_set_head,
cost_cutoff, _opts);
if (d == NULL)
{
lgdebug(+D_SEL, "Debug: Word %zu: Disjunct cost > cost_cutoff %.2f\n",
wi, cost_cutoff);
#ifdef DEBUG
if (!test_enabled("SAT-cost"))
#endif
return false;
}
const vector<EmptyConnector>& ec = _word_tags[wi].get_empty_connectors();
for (vector<EmptyConnector>::const_iterator j = ec.begin(); j < ec.end(); j++)
{
lgdebug(+D_SEL, "Debug: Word %zu: Costly-null var=%d, found=%d cost=%.2f\n",
wi, j->ec_var, _solver->model[j->ec_var] == l_True, j->ec_cost);
if (_solver->model[j->ec_var] == l_True)
d->cost += j->ec_cost;
}
lkg->chosen_disjuncts[wi] = d;
#if LIMIT_TOTAL_LINKAGE_COST
if (d->cost > cost_cutoff)
{
lgdebug(+D_SEL, "Word %zu: Disjunct cost  %.2f > cost_cutoff %.2f\n",
wi, d->cost, cost_cutoff);
#ifdef DEBUG
if (!test_enabled("SAT-cost"))
#endif
return false;
}
#endif
}
DEBUG_print("Total links: ." <<  lkg->num_links << "." << endl);
return true;
}
#undef D_SEL
extern "C" int sat_parse(Sentence sent, Parse_Options  opts)
{
if (opts->min_null_count > 0) {
if (opts->verbosity >= 1)
prt_error("Info: use-sat: Cannot parse with null links (yet).\n"
"              Set the \"null\" option to 0 to turn off "
"parsing with null links.\n");
sent->num_valid_linkages = 0;
sent->num_linkages_post_processed = 0;
return 0;
}
SATEncoder* encoder = (SATEncoder*) sent->hook;
if (encoder) {
sat_free_linkages(sent, encoder->_next_linkage_index);
delete encoder;
}
encoder = new SATEncoderConjunctionFreeSentences(sent, opts);
sent->hook = encoder;
encoder->encode();
LinkageIdx linkage_limit = opts->linkage_limit;
LinkageIdx k;
Linkage lkg = NULL;
for (k = 0; k < linkage_limit; k++)
{
lkg = encoder->get_next_linkage();
if (lkg == NULL || lkg->lifo.N_violations == 0) break;
}
encoder->print_stats();
if (lkg == NULL || k == linkage_limit) {
sent->num_valid_linkages = 0;
sent->num_linkages_found = k;
sent->num_linkages_post_processed = k;
if (opts->max_null_count > 0) {
if (opts->verbosity >= 1)
prt_error("Info: use-sat: Cannot parse with null links (yet).\n"
"              Set the \"null\" option to 0 to turn off "
"parsing with null links.\n");
}
} else {
sent->num_valid_linkages = linkage_limit;
sent->num_linkages_post_processed = linkage_limit;
}
return 0;
}
extern "C" Linkage sat_create_linkage(LinkageIdx k, Sentence sent, Parse_Options  opts)
{
SATEncoder* encoder = (SATEncoder*) sent->hook;
if (!encoder) return NULL;
if (k >= opts->linkage_limit)
{
encoder->print_stats();
return NULL;
}
if(k > encoder->_next_linkage_index)
{
prt_error("Error: Linkage index %zu is greater than the "
"maximum expected one %zu\n", k, encoder->_next_linkage_index);
return NULL;
}
if (k < encoder->_next_linkage_index)
return &encoder->_sent->lnkages[k];
return encoder->get_next_linkage();
}
extern "C" void sat_sentence_delete(Sentence sent)
{
SATEncoder* encoder = (SATEncoder*) sent->hook;
if (!encoder) return;
encoder->print_stats();
sat_free_linkages(sent, encoder->_next_linkage_index);
delete encoder;
}