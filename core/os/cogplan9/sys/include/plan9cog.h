#ifndef _PLAN9COG_H_
#define _PLAN9COG_H_ 1
#pragma src "/sys/src/libplan9cog"
#pragma lib "libplan9cog.a"
#include <plan9cog/atomspace.h>
#include <plan9cog/pln.h>
#include <plan9cog/cogvm.h>
typedef struct Plan9Cog Plan9Cog;
struct Plan9Cog {
AtomSpace *atomspace;
PlnInference *pln;
CogMemory *cogmem;
int initialized;
Lock;
};
Plan9Cog*	plan9coginit(void);
void		plan9cogfree(Plan9Cog *p9c);
Plan9Cog*	plan9coginstance(void);
enum {
Tcogatom = 100,
Rcogatom,
Tcogpln,
Rcogpln,
Tcogecan,
Rcogecan,
Tcogpattern,
Rcogpattern,
Tcogmine,
Rcogmine,
};
typedef struct CogFusionReactor CogFusionReactor;
struct CogFusionReactor {
Plan9Cog *p9c;
int nworkers;
Chan **workers;
Lock;
};
CogFusionReactor*	cogreactorinit(Plan9Cog *p9c, int nworkers);
void			cogreactorfree(CogFusionReactor *cfr);
void			cogreactorsubmit(CogFusionReactor *cfr, void *task);
void*			cogreactorresult(CogFusionReactor *cfr);
typedef struct MachSpace MachSpace;
struct MachSpace {
AtomSpace *local;
AtomSpace **remote;
int nremote;
char **hosts;
Lock;
};
MachSpace*	machspaceinit(AtomSpace *local);
void		machspacefree(MachSpace *ms);
int		machspaceconnect(MachSpace *ms, char *host);
Atom*		machspacefind(MachSpace *ms, ulong id);
int		machspacesync(MachSpace *ms);
enum {
GripAtom = 1,
GripPattern,
GripRule,
GripTask,
GripResult,
};
typedef struct CogGrip CogGrip;
struct CogGrip {
int type;
void *object;
int refcount;
Lock;
};
CogGrip*	coggrip(int type, void *object);
void		cogrelease(CogGrip *grip);
void*		cogobject(CogGrip *grip);
CogGrip*	cogretain(CogGrip *grip);
typedef struct CogDashboard CogDashboard;
struct CogDashboard {
Plan9Cog *p9c;
int httpport;
void *httpd;
Lock;
};
CogDashboard*	cogdashboardinit(Plan9Cog *p9c, int port);
void		cogdashboardfree(CogDashboard *cd);
void		cogdashboardstart(CogDashboard *cd);
void		cogdashboardstop(CogDashboard *cd);
typedef struct EcanNetwork EcanNetwork;
struct EcanNetwork {
AtomSpace *as;
short totalsti;
short totallti;
short attentionalfocus;
Atom **focusatoms;
int nfocus;
Lock;
};
EcanNetwork*	ecaninit(AtomSpace *as, short totalsti, short totallti);
void		ecanfree(EcanNetwork *ecan);
void		ecanupdate(EcanNetwork *ecan);
void		ecanallocate(EcanNetwork *ecan, Atom *a, short sti);
void		ecanspread(EcanNetwork *ecan, Atom *source);
Atom**		ecanfocus(EcanNetwork *ecan, int *n);
void		ecandecay(EcanNetwork *ecan, float rate);
typedef struct PatternMiner PatternMiner;
struct PatternMiner {
AtomSpace *as;
int minsupport;
float minconf;
Pattern **patterns;
int npatterns;
Lock;
};
PatternMiner*	patterninit(AtomSpace *as);
void		patternfree(PatternMiner *pm);
void		patternmine(PatternMiner *pm, int minsupport);
Pattern**	patternget(PatternMiner *pm, int *n);
float		patternsupport(PatternMiner *pm, Pattern *pat);
float		patternconfidence(PatternMiner *pm, Pattern *pat);
void		cogprint(char *fmt, ...);
void		cogdebug(int level, char *fmt, ...);
char*		cogatomstr(Atom *a);
char*		cogpatternstr(Pattern *pat);
char*		cogtvstr(TruthValue tv);
typedef struct CogInfo CogInfo;
struct CogInfo {
char version[32];
ulong uptime;
ulong natoms;
ulong nrules;
ulong ninferences;
ulong cogmem;
};
void		coginfo(Plan9Cog *p9c, CogInfo *info);
#endif