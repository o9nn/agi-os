#pragma src "/sys/src/libpln"
#pragma lib "libpln.a"
typedef struct PlnRule PlnRule;
typedef struct PlnFormula PlnFormula;
typedef struct PlnInference PlnInference;
#include <plan9cog/atomspace.h>
enum {
PlnDeduction = 1,
PlnInduction,
PlnAbduction,
PlnRevision,
PlnModus,
PlnAnd,
PlnOr,
PlnNot,
PlnInheritance,
PlnSimilarity,
PlnIntensional,
PlnExtensional,
};
struct PlnFormula {
int type;
TruthValue (*compute)(TruthValue *inputs, int n);
float (*strength)(TruthValue *inputs, int n);
float (*confidence)(TruthValue *inputs, int n);
};
struct PlnRule {
int id;
char *name;
Pattern *premises;
int npremises;
Pattern *conclusion;
PlnFormula *formula;
float weight;
};
struct PlnInference {
AtomSpace *as;
PlnRule **rules;
int nrules;
int maxsteps;
float minconf;
Lock;
};
PlnInference*	plninit(AtomSpace *as);
void		plnfree(PlnInference *pln);
void		plnaddrule(PlnInference *pln, PlnRule *rule);
PlnRule*	plncreaterule(char *name, Pattern *premises, int np, Pattern *conclusion, int formulatype);
Atom**		plnforward(PlnInference *pln, Atom *target, int maxsteps, int *n);
Atom**		plnbackward(PlnInference *pln, Atom *goal, int maxsteps, int *n);
TruthValue	plneval(PlnInference *pln, Atom *query);
TruthValue	plndeduction(TruthValue a, TruthValue b);
TruthValue	plninduction(TruthValue a, TruthValue b);
TruthValue	plnabduction(TruthValue a, TruthValue b);
TruthValue	plnrevision(TruthValue a, TruthValue b);
TruthValue	plnand(TruthValue a, TruthValue b);
TruthValue	plnor(TruthValue a, TruthValue b);
TruthValue	plnnot(TruthValue a);
typedef struct PlnContext PlnContext;
struct PlnContext {
Atom *focus;
Atom **premises;
int npremises;
TruthValue *tvs;
};
PlnContext*	plncontextcreate(Atom *focus);
void		plncontextfree(PlnContext *ctx);
void		plncontextaddpremise(PlnContext *ctx, Atom *premise);
TruthValue	plncontexteval(PlnInference *pln, PlnContext *ctx);
typedef struct PlnStats PlnStats;
struct PlnStats {
ulong inferences;
ulong forward;
ulong backward;
ulong rulematch;
ulong tvcompute;
};
void		plnstats(PlnInference *pln, PlnStats *stats);
void		plnresetstats(PlnInference *pln);
typedef struct UreChainer UreChainer;
struct UreChainer {
PlnInference *pln;
Atom *rulebase;
int maxiter;
float complexity;
};
UreChainer*	ureinit(AtomSpace *as, Atom *rulebase);
void		urefree(UreChainer *ure);
Atom**		urechain(UreChainer *ure, Atom *target, int *n);
void		plnallocattention(PlnInference *pln, Atom *a, short amount);
short		plngetattention(PlnInference *pln, Atom *a);
void		plnspreadattention(PlnInference *pln, Atom *source);