#include <opencog/visualization/ubigraph/Ubigrapher.h>
#include "Pattern.h"
#ifndef _OPENCOG_HOPFIELD_UBIGRAPHER
#define _OPENCOG_HOPFIELD_UBIGRAPHER
using namespace std;
namespace opencog
{
class HopfieldUbigrapher : public Ubigrapher
{
int labelVertex, labelEdge;
Handle groundNode;
protected:
void setStyles();
public:
HopfieldUbigrapher();
bool showText;
void setText(string s);
void setAsKeyNode(Handle kn);
void setAsActiveKeyNode(Handle kn);
void setAsPatternNode(Handle kn);
void setAsNewRandomLink(Handle kn);
void setGroundNode(Handle h) ;
void showDiff(HandleSeq h, Pattern cur, Pattern original);
int patternStyle, patternAddErrStyle, patternMissErrStyle, notPatternStyle;
int keyNodeActiveStyle, keyNodeStyle;
int randomLinkStyle;
};
}
#endif