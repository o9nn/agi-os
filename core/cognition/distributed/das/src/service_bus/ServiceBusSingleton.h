#pragma once
#include <memory>
#include <mutex>
#include "ServiceBus.h"
using namespace std;
namespace service_bus {
class ServiceBusSingleton {
public:
~ServiceBusSingleton(){};
static void init(const string& host_id,
const string& known_peer = "",
unsigned int port_lower = 64000,
unsigned int port_upper = 64999);
static void provide(shared_ptr<ServiceBus> service_bus);
static shared_ptr<ServiceBus> get_instance();
private:
ServiceBusSingleton(){};
static bool INITIALIZED;
static shared_ptr<ServiceBus> SERVICE_BUS;
static mutex API_MUTEX;
};
}