#pragma once
#include "cutlass/barrier.h"
namespace flash {
enum class NamedBarriers {
SReady = 1,
SoftmaxReady = 2,
};
}