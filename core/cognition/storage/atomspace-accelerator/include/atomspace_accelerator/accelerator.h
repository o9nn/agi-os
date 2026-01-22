#ifndef _ATOMSPACE_ACCELERATOR_ACCELERATOR_H
#define _ATOMSPACE_ACCELERATOR_ACCELERATOR_H
#include <memory>
#include <string>
namespace atomspace_accelerator {
class Accelerator {
public:
Accelerator();
~Accelerator();
void enable();
void disable();
double getAccelerationFactor() const;
void updateParameters(const std::string& key, double value);
std::string getPerformanceMetrics() const;
private:
class Impl;
std::unique_ptr<Impl> pImpl;
};
}
#endif