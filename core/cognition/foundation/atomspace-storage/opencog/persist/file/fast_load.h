#ifndef FAST_LOAD_H
#define FAST_LOAD_H
#include <istream>
#include <string>
#include <opencog/atomspace/AtomSpace.h>
namespace opencog
{
void load_file(const std::string& file_name, AtomSpacePtr);
static inline void load_file(const std::string& file_name, AtomSpace& asr)
{ load_file(file_name, AtomSpaceCast(&asr)); }
Handle parseExpression(const std::string& expr, AtomSpacePtr);
static inline Handle parseExpression(const std::string& expr, AtomSpace& asr)
{ return parseExpression(expr, AtomSpaceCast(&asr)); }
Handle parseStream(std::istream&, AtomSpacePtr);
static inline Handle parseStream(std::istream& stm, AtomSpace& asr)
{ return parseStream(stm, AtomSpaceCast(&asr)); }
}
#endif