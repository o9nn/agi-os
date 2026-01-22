#pragma once
#include <string>
using namespace std;
namespace commons {
class MettaMapping {
private:
MettaMapping();
public:
~MettaMapping() {}
static string EXPRESSION_LINK_TYPE;
static string SYMBOL_NODE_TYPE;
static string AND_QUERY_OPERATOR;
static string OR_QUERY_OPERATOR;
};
}