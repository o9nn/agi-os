#pragma once
#include "../atomspace/tensor_atomspace.h"
#include "../operations/ggml_opencog_ops.h"
#include <vector>
#include <queue>
#include <memory>
namespace opencog {
struct AttentionAgent {
std::string name;
float wage;
float activation_level;
int priority;
bool active;
AttentionAgent(const std::string& n, float w = 1.0f, int p = 1)
: name(n), wage(w), activation_level(0.0f), priority(p), active(true) {}
};
class AttentionBank {
private:
float total_stimulus;
float total_funding;
float min_sti;
float max_sti;
size_t attentional_focus_size_;
struct ggml_context* ctx_;
struct ggml_tensor* attention_tensor_;
struct ggml_tensor* stimulus_tensor_;
struct ggml_tensor* wage_tensor_;
public:
explicit AttentionBank(struct ggml_context* ctx);
~AttentionBank();
void set_parameters(float total_funding = 1000.0f,
float min_sti = 0.0f,
float max_sti = 1.0f,
size_t focus_size = 100);
void update_attention_values(TensorAtomSpace* atomspace);
void stimulate_atom(Handle atom, float amount);
void collect_rent(TensorAtomSpace* atomspace);
std::vector<Handle> get_attentional_focus(TensorAtomSpace* atomspace) const;
struct BankStats {
float total_sti;
float total_lti;
float avg_attention;
size_t atoms_in_focus;
float stimulus_spent;
};
BankStats get_statistics(TensorAtomSpace* atomspace) const;
private:
void normalize_attention_values();
void update_tensor_data(TensorAtomSpace* atomspace);
};
class ECANAttentionSystem {
private:
TensorAtomSpace* atomspace_;
std::unique_ptr<AttentionBank> bank_;
std::vector<std::unique_ptr<AttentionAgent>> agents_;
float forgetting_rate_;
float spreading_rate_;
int update_frequency_;
int cycle_count_;
struct ECANStats {
size_t total_cycles;
float avg_focus_size;
float attention_efficiency;
std::map<std::string, float> agent_performance;
};
mutable ECANStats stats_;
public:
explicit ECANAttentionSystem(TensorAtomSpace* atomspace);
~ECANAttentionSystem();
void initialize(float total_funding = 1000.0f,
size_t focus_size = 100);
void run_cycle();
void add_agent(std::unique_ptr<AttentionAgent> agent);
void remove_agent(const std::string& agent_name);
void set_parameters(float forgetting_rate = 0.01f,
float spreading_rate = 0.1f,
int update_freq = 10);
void focus_on_atom(Handle atom, float intensity = 1.0f);
void spread_activation(Handle source_atom, float amount = 0.1f);
std::vector<Handle> get_focus() const;
std::vector<std::pair<Handle, float>> get_importance_ranking(size_t top_n = 20) const;
void external_stimulus(Handle atom, float intensity);
ECANStats get_statistics() const;
void reset();
private:
void run_importance_updating();
void run_importance_diffusion();
void run_forgetting();
void run_rent_collection();
void activate_agents();
void update_agent_wages();
float calculate_attention_efficiency() const;
void update_statistics();
};
class NoveltyDetectionAgent : public AttentionAgent {
public:
NoveltyDetectionAgent() : AttentionAgent("NoveltyDetection", 1.0f, 2) {}
void run(TensorAtomSpace* atomspace, AttentionBank* bank);
private:
std::unordered_map<Handle, float> last_seen_sti_;
float novelty_threshold_ = 0.1f;
};
class ImportanceSpreadingAgent : public AttentionAgent {
public:
ImportanceSpreadingAgent() : AttentionAgent("ImportanceSpreading", 0.8f, 1) {}
void run(TensorAtomSpace* atomspace, AttentionBank* bank);
private:
float spreading_factor_ = 0.1f;
float decay_factor_ = 0.95f;
};
class ForgettingAgent : public AttentionAgent {
public:
ForgettingAgent() : AttentionAgent("Forgetting", 0.5f, 3) {}
void run(TensorAtomSpace* atomspace, AttentionBank* bank);
private:
float forgetting_threshold_ = 0.01f;
float forgetting_rate_ = 0.02f;
};
class ReinforcementAgent : public AttentionAgent {
public:
ReinforcementAgent() : AttentionAgent("Reinforcement", 1.2f, 2) {}
void run(TensorAtomSpace* atomspace, AttentionBank* bank);
void reinforce_successful_inference(const std::vector<Handle>& atoms, float reward);
private:
std::vector<std::vector<Handle>> successful_patterns_;
std::vector<float> pattern_rewards_;
float learning_rate_ = 0.1f;
};
class ECANAgentFactory {
public:
static std::vector<std::unique_ptr<AttentionAgent>> create_standard_agents();
static std::unique_ptr<AttentionAgent> create_agent(const std::string& type);
};
}