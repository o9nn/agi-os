#ifndef _OPENCOG_HOPFIELD_SERVER_H
#define _OPENCOG_HOPFIELD_SERVER_H
#include <sstream>
#include <vector>
#include <math.h>
#include <opencog/attention/ForgettingAgent.h>
#include <opencog/attention/HebbianUpdatingAgent.h>
#include <opencog/attention/ImportanceSpreadingAgent.h>
#include <opencog/attention/ImportanceUpdatingAgent.h>
#include <opencog/attention/ImportanceDiffusionAgent.h>
#include <opencog/cogserver/server/CogServer.h>
#include <opencog/util/RandGen.h>
#include "StorkeyAgent.h"
#include "ImprintAgent.h"
#include "Pattern.h"
#define HDEMO_DEFAULT_WIDTH 3
#define HDEMO_DEFAULT_HEIGHT 3
#define HDEMO_DEFAULT_LINKS 15
#define HDEMO_DEFAULT_PATTERN_STIM 1000
namespace opencog
{
class HopfieldOptions;
class ForgettingAgent;
class HebbianUpdatingAgent;
class ImportanceDiffusionAgent;
class ImportanceSpreadingAgent;
class ImportanceUpdatingAgent;
class StorkeyAgent;
class ImprintAgent;
#ifdef HAVE_UBIGRAPH
class HopfieldUbigrapher;
#endif
class HopfieldServer : public CogServer
{
private:
RandGen* rng;
Handle findKeyNode();
void updateKeyNodeLinks(Handle keyHandle, float density = 1.0f);
void chooseKeyNodes();
HandleMap getDestinationsFrom(Handle src, Type linkType);
#ifdef HAVE_UBIGRAPH
HopfieldUbigrapher* ubi;
#endif
HandleSeq recentlyAddedLinks;
public:
static opencog::BaseServer* derivedCreateInstance(AtomSpace* as = nullptr);
stim_t patternStimulus;
ForgettingAgentPtr forgetAgent;
HebbianUpdatingAgentPtr hebUpdateAgent;
StorkeyAgentPtr storkeyAgent;
ImprintAgentPtr imprintAgent;
ImportanceDiffusionAgentPtr diffuseAgent;
ImportanceSpreadingAgentPtr spreadAgent;
ImportanceUpdatingAgentPtr importUpdateAgent;
HopfieldOptions *options;
int width, height, links;
float density;
HandleSeq hGrid;
std::vector<bool> hGridKey;
HandleSeq keyNodes;
HopfieldServer();
virtual ~HopfieldServer();
void init(int width, int height, int numLinks);
void encodePattern(Pattern pattern, stim_t stimulus);
float totalEnergy();
Pattern retrievePattern(Pattern pattern, int numCycles, int spreadCycles,
Pattern originalPattern = Pattern(0,0));
std::vector<bool> checkNeighbourStability(Pattern p, float tolerance);
void updateAtomSpaceForRetrieval(int spreadCycles, Pattern originalPattern
= Pattern(0,0));
template<typename Number> std::string patternToString(std::vector<Number> p) {
std::stringstream ss;
Number col = 0;
typename std::vector<Number>::iterator it = p.begin();
while (it != p.end()) {
ss << *it << " ";
col++;
if (col == width) {
ss << std::endl;
col = 0;
}
++it;
}
return ss.str();
}
Pattern getGridSTIAsPattern(bool blankKeys = true);
std::vector<stim_t> getGridStimVector();
void reset();
void resetNodes(bool toDefault=false);
void imprintPattern(Pattern pattern, int cycles);
void doForgetting(float proportion);
void addRandomLinks();
std::string printMatrixResult(std::vector< Pattern > p1);
std::vector<float> imprintAndTestPattern(Pattern p, int imprint, int retrieve, Pattern c, float mutate);
float singleImprintAndTestPattern(Pattern p, int retrieve, float mutate, Pattern c);
void printStatus();
void printLinks();
};
}
#endif