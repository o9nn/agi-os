#pragma once
#include <memory>
#include <mutex>
#include <string>
#include <unordered_map>
#include <vector>
#include "AtomDBAPITypes.h"
#include "LinkSchema.h"
using namespace std;
using namespace atoms;
namespace atomdb {
class AtomDBCache {
public:
typedef struct {
bool is_cache_hit;
shared_ptr<atomdb_api_types::HandleSet> result;
} QueryForPatternResult;
typedef struct {
bool is_cache_hit;
shared_ptr<atomdb_api_types::AtomDocument> result;
} GetAtomDocumentResult;
typedef struct {
bool is_cache_hit;
shared_ptr<atomdb_api_types::HandleList> result;
} QueryForTargetsResult;
typedef struct {
bool is_cache_hit;
shared_ptr<atomdb_api_types::HandleSet> result;
} QueryForIncomingResult;
AtomDBCache() {}
virtual ~AtomDBCache() {}
GetAtomDocumentResult get_node_document(const string& handle);
void add_node_document(const string& handle, shared_ptr<atomdb_api_types::AtomDocument> document);
void erase_node_document_cache(const string& handle);
GetAtomDocumentResult get_link_document(const string& handle);
void add_link_document(const string& handle, shared_ptr<atomdb_api_types::AtomDocument> document);
void erase_link_document_cache(const string& handle);
QueryForPatternResult query_for_pattern(const string& pattern_handle);
void add_pattern_matching(const string& pattern_handle,
shared_ptr<atomdb_api_types::HandleSet> results);
void erase_pattern_matching_cache(const string& pattern_handle);
void clear_all_pattern_handles();
QueryForTargetsResult query_for_targets(const string& link_handle);
void add_handle_targets(const string& link_handle, shared_ptr<atomdb_api_types::HandleList> results);
void erase_handle_targets_cache(const string& link_handle);
void clear_all_targets_handles();
QueryForIncomingResult query_for_incoming_set(const string& handle);
void add_incoming_set(const string& handle, shared_ptr<atomdb_api_types::HandleSet> results);
void erase_incoming_set_cache(const string& handle);
void clear_all_incoming_handles();
private:
unordered_map<string, shared_ptr<atomdb_api_types::AtomDocument>> node_doc_cache;
mutex node_doc_cache_mutex;
unordered_map<string, shared_ptr<atomdb_api_types::AtomDocument>> link_doc_cache;
mutex link_doc_cache_mutex;
unordered_map<string, shared_ptr<atomdb_api_types::HandleSet>> pattern_matching_cache;
mutex pattern_matching_cache_mutex;
unordered_map<string, shared_ptr<atomdb_api_types::HandleSet>> incoming_set_cache;
mutex incoming_set_cache_mutex;
unordered_map<string, shared_ptr<atomdb_api_types::HandleList>> handle_list_cache;
mutex handle_list_cache_mutex;
};
}