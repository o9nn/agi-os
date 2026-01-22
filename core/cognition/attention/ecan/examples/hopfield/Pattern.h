#ifndef _OPENCOG_HDEMO_PATTERN_MATRIX_H
#define _OPENCOG_HDEMO_PATTERN_MATRIX_H
#include <vector>
#include <sys/types.h>
#include <opencog/atoms/truthvalue/AttentionValue.h>
#include <opencog/util/RandGen.h>
namespace opencog
{
extern RandGen* patternRng;
class Pattern : public std::vector< int >
{
private:
int width;
int height;
opencog::RandGen *rng;
std::vector<bool> *mask;
public:
Pattern(int w, int h, float density = 0.0f);
Pattern(const Pattern &src) : std::vector<int>(src),
width(src.width), height(src.height), rng(patternRng), mask(NULL) {
if (src.mask) mask = new std::vector<bool>(*(src.mask));
};
~Pattern();
Pattern & operator = (const Pattern & other) {
if (this != &other) {
width = other.width;
height = other.height;
rng = patternRng;
mask = NULL;
if (other.mask) mask = new std::vector<bool>(*(other.mask));
std::vector<int>::operator=(other);
}
return *this;
}
float hammingSimilarity(const Pattern &p);
int bitErrors(const Pattern &p);
Pattern binarisePattern(AttentionValue::sti_t threshold);
Pattern mutatePattern(float error);
Pattern mutatePattern(unsigned int error);
int getWidth();
int getHeight();
bool isEmpty();
void setMask(const std::vector<bool>& _mask);
bool isMasked(uint i) const;
int activity();
static std::vector< Pattern > generateRandomPatterns(int amount, int w, int h, float density);
static std::vector< Pattern > mutatePatterns( std::vector< Pattern > &patterns, float error);
static std::vector< Pattern > loadPatterns( std::string fn, int size);
bool operator==(const Pattern& b) const;
};
}
#endif