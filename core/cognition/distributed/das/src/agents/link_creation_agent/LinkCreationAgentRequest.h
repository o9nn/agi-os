#pragma once
#include <string>
#include <vector>
using namespace std;
namespace link_creation_agent {
struct LinkCreationAgentRequest {
vector<string> query;
vector<string> link_template;
int max_results = 1000;
int repeat = 1;
bool infinite = false;
string context = "";
bool update_attention_broker = false;
bool use_metta_as_query_tokens = false;
string id = "";
string original_id = "";
bool is_running = false;
bool aborting = false;
int processed = 0;
bool completed = false;
bool importance_flag = false;
long last_execution = 0;
int current_interval;
};
}