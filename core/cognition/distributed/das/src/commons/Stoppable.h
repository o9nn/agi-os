#pragma once
using namespace std;
namespace commons {
class Stoppable {
public:
virtual void stop() = 0;
virtual bool stopped() = 0;
Stoppable() {}
~Stoppable() {}
};
}