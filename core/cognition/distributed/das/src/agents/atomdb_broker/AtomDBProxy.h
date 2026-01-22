#pragma once
#include <memory>
#include <mutex>
#include <string>
#include <vector>
#include "Atom.h"
#include "AtomDB.h"
#include "BaseProxy.h"
#include "SharedQueue.h"
#include "ThreadPool.h"
using namespace std;
using namespace agents;
using namespace atoms;
using namespace atomdb;
namespace atomdb_broker {
class AtomDBProxy : public BaseProxy {
public:
static int THREAD_POOL_SIZE;
static size_t MAX_PENDING_ATOMS;
static string ADD_ATOMS;
static string DELETE_ATOMS;
static string START_STREAM;
static string END_STREAM;
static string NODE;
static string LINK;
AtomDBProxy();
virtual ~AtomDBProxy();
virtual void pack_command_line_args() override;
virtual void tokenize(vector<string>& output) override;
vector<string> add_atoms(const vector<Atom*>& atoms, bool use_streaming = false);
vector<string> add_atoms(const vector<string>& tokens, bool use_streaming = false);
void delete_atoms(const vector<string>& handles, bool delete_link_targets = false);
virtual bool from_remote_peer(const string& command, const vector<string>& args) override;
virtual void untokenize(vector<string>& tokens) override;
void init_server_side();
void shutdown_server_side();
private:
void add_atoms_callback(const vector<string>& args);
void delete_atoms_callback(const vector<string>& args);
template <typename AtomDataType, typename Factory>
std::vector<AtomDataType> build_atoms_from_tokens(const vector<string>& tokens, Factory&& factory) {
std::vector<AtomDataType> atoms;
std::string current;
std::vector<std::string> buffer;
auto flush = [&]() {
if (current.empty()) return;
atoms.emplace_back(factory(current, buffer));
buffer.clear();
};
for (const auto& t : tokens) {
if (t == NODE || t == LINK) {
if (!current.empty()) flush();
current = t;
} else {
buffer.push_back(t);
}
}
if (!current.empty()) flush();
return atoms;
}
static std::shared_ptr<Atom> shared_ptr_atom_factory(const string& type, vector<string>& data) {
if (type == NODE) return std::make_shared<Node>(data);
return std::make_shared<Link>(data);
}
static Atom* raw_ptr_atom_factory(const string& type, vector<string>& data) {
if (type == NODE) return new Node(data);
return new Link(data);
}
void enqueue_request(const vector<string>& tokens);
void process_atom_batches();
mutex api_mutex;
shared_ptr<AtomDB> atomdb;
static const size_t BATCH_SIZE;
static const size_t COMMAND_SIZE_LIMIT;
bool is_processing_buffer = false;
shared_ptr<SharedQueue> processing_queue = nullptr;
size_t pending_atoms_count = 0;
thread processing_thread;
shared_ptr<ThreadPool> thread_pool;
bool stop_processing = false;
};
}