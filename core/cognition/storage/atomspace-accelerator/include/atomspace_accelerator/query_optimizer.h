#ifndef _ATOMSPACE_ACCELERATOR_QUERY_OPTIMIZER_H
#define _ATOMSPACE_ACCELERATOR_QUERY_OPTIMIZER_H
#include <memory>
#include <string>
namespace atomspace_accelerator {
class QueryOptimizer {
public:
QueryOptimizer();
~QueryOptimizer();
std::string optimizeQuery(const std::string& query);
std::string getOptimizationStats() const;
void setOptimizationLevel(int level);
private:
class Impl;
std::unique_ptr<Impl> pImpl;
};
}
#endif