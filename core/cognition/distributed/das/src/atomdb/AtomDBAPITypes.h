#pragma once
#include <algorithm>
#include <optional>
#include "Link.h"
#include "Node.h"
#include "Utils.h"
using namespace std;
using namespace commons;
namespace atomdb {
namespace atomdb_api_types {
class HandleList {
public:
HandleList() {}
virtual ~HandleList() {}
virtual const char* get_handle(unsigned int index) = 0;
virtual unsigned int size() = 0;
};
class HandleSetIterator {
public:
virtual char* next() = 0;
};
class HandleSet {
public:
HandleSet() {}
virtual ~HandleSet() {}
virtual unsigned int size() = 0;
virtual void append(shared_ptr<HandleSet> other) = 0;
virtual shared_ptr<HandleSetIterator> get_iterator() = 0;
virtual map<string, string> get_metta_expressions_by_handle(const string& handle) = 0;
virtual Assignment get_assignments_by_handle(const string& handle) = 0;
};
class AtomDocument {
public:
AtomDocument() {}
virtual ~AtomDocument() {}
virtual const char* get(const string& key) = 0;
virtual const char* get(const string& array_key, unsigned int index) = 0;
virtual bool get_bool(const string& key) = 0;
virtual unsigned int get_size(const string& array_key) = 0;
virtual bool contains(const string& key) = 0;
};
enum ATOMDB_TYPE { REDIS_MONGODB, MORKDB };
}
}