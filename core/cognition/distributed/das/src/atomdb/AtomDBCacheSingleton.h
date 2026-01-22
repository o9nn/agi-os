#pragma once
#include <memory>
#include "AtomDBCache.h"
using namespace std;
namespace atomdb {
class AtomDBCacheSingleton {
public:
~AtomDBCacheSingleton() {}
static void init();
static shared_ptr<AtomDBCache> get_instance();
private:
AtomDBCacheSingleton() {}
static bool initialized;
static shared_ptr<AtomDBCache> atom_db_cache;
};
}