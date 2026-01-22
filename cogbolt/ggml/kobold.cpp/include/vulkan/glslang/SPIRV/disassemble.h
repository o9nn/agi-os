#pragma once
#ifndef disassembler_H
#define disassembler_H
#include <iostream>
#include <vector>
#include "glslang/Include/visibility.h"
namespace spv {
GLSLANG_EXPORT void Disassemble(std::ostream& out, const std::vector<unsigned int>&);
}
#endif