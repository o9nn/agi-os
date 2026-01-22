#pragma src "/sys/src/libatomspace"
#pragma lib "libatomspace.a"
typedef struct Atom Atom;
typedef struct AtomSpace AtomSpace;
typedef struct TruthValue TruthValue;
typedef struct AttentionValue AttentionValue;
enum {
AtomNode = 1,
AtomLink,
ConceptNode,
PredicateNode,
EvaluationLink,
InheritanceLink,
SimilarityLink,
ImplicationLink,
ExecutionLink,
ListLink,
};
struct TruthValue {
float strength;
float confidence;
ulong count;
};
struct AttentionValue {
short sti;
short lti;
short vlti;
};
struct Atom {
ulong id;
int type;
char *name;
Atom **outgoing;
int noutgoing;
TruthValue tv;
AttentionValue av;
};
struct AtomSpace {
Atom **atoms;
int natoms;
int maxatoms;
Lock;
};
AtomSpace* atomspacecreate(void);
void atomspacefree(AtomSpace *as);
Atom* atomcreate(AtomSpace *as, int type, char *name);
Atom* linkcreate(AtomSpace *as, int type, Atom **outgoing, int n);
Atom* atomfind(AtomSpace *as, ulong id);
int atomdelete(AtomSpace *as, ulong id);
TruthValue atomgettruth(Atom *a);
void atomsettruth(Atom *a, TruthValue tv);
AttentionValue atomgetattention(Atom *a);
void atomsetattention(Atom *a, AttentionValue av);
typedef int (*AtomPredicate)(Atom *a, void *arg);
Atom** atomquery(AtomSpace *as, AtomPredicate pred, void *arg, int *n);
Atom** atomgetincoming(AtomSpace *as, Atom *a, int *n);
typedef struct Pattern Pattern;
struct Pattern {
int type;
char *name;
Pattern **outgoing;
int noutgoing;
int wildcard;
};
Atom** atommatch(AtomSpace *as, Pattern *pat, int *n);
int atomspaceexport(AtomSpace *as, int fd);
AtomSpace* atomspaceimport(int fd);
enum {
CogAtomCreate = 1,
CogAtomDelete,
CogAtomUpdate,
CogAtomQuery,
CogAtomSync,
CogPlnReason,
CogEcanAlloc,
};
typedef struct CogMsg CogMsg;
struct CogMsg {
int type;
ulong atomid;
int atomtype;
char data[8192];
int ndata;
};
int cogsend(int fd, CogMsg *msg);
int cogrecv(int fd, CogMsg *msg);