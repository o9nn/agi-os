#pragma once
#include "Atom.h"
#include "Utils.h"
namespace atoms {
class HandleDecoder {
protected:
HandleDecoder() {}
public:
virtual ~HandleDecoder() {}
virtual shared_ptr<Atom> get_atom(const string& handle) = 0;
};
}