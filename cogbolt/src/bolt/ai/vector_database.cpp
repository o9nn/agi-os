#include "bolt/ai/vector_database.hpp"
#include <cmath>
#include <algorithm>
#include <numeric>
#include <fstream>
#include <stdexcept>
namespace bolt {
namespace ai {
void VectorDatabase::addOrUpdateRecord(const VectorRecord& record) {
std::lock_guard<std::mutex> lock(mutex_);
records_[record.id] = record;
}
bool VectorDatabase::removeRecord(const std::string& id) {
std::lock_guard<std::mutex> lock(mutex_);
return records_.erase(id) > 0;
}
bool VectorDatabase::hasRecord(const std::string& id) const {
std::lock_guard<std::mutex> lock(mutex_);
return records_.find(id) != records_.end();
}
VectorRecord VectorDatabase::getRecord(const std::string& id) const {
std::lock_guard<std::mutex> lock(mutex_);
auto it = records_.find(id);
if (it != records_.end()) {
return it->second;
}
return VectorRecord();
}
float VectorDatabase::cosineSimilarity(
const std::vector<float>& a,
const std::vector<float>& b) {
if (a.size() != b.size() || a.empty()) {
return 0.0f;
}
float dot_product = 0.0f;
float norm_a = 0.0f;
float norm_b = 0.0f;
for (size_t i = 0; i < a.size(); ++i) {
dot_product += a[i] * b[i];
norm_a += a[i] * a[i];
norm_b += b[i] * b[i];
}
if (norm_a == 0.0f || norm_b == 0.0f) {
return 0.0f;
}
return dot_product / (std::sqrt(norm_a) * std::sqrt(norm_b));
}
float VectorDatabase::euclideanDistance(
const std::vector<float>& a,
const std::vector<float>& b) {
if (a.size() != b.size() || a.empty()) {
return std::numeric_limits<float>::max();
}
float sum = 0.0f;
for (size_t i = 0; i < a.size(); ++i) {
float diff = a[i] - b[i];
sum += diff * diff;
}
return std::sqrt(sum);
}
std::vector<SearchResult> VectorDatabase::searchSimilar(
const std::vector<float>& query_embedding,
int top_k,
float min_similarity) const {
std::lock_guard<std::mutex> lock(mutex_);
std::vector<SearchResult> results;
results.reserve(records_.size());
for (const auto& pair : records_) {
const VectorRecord& record = pair.second;
float similarity = cosineSimilarity(query_embedding, record.embedding);
if (similarity >= min_similarity) {
results.emplace_back(
record.id,
similarity,
record.content_preview,
record.file_type
);
}
}
std::sort(results.begin(), results.end(),
[](const SearchResult& a, const SearchResult& b) {
return a.similarity_score > b.similarity_score;
});
if (results.size() > static_cast<size_t>(top_k)) {
results.resize(top_k);
}
return results;
}
size_t VectorDatabase::getRecordCount() const {
std::lock_guard<std::mutex> lock(mutex_);
return records_.size();
}
std::vector<std::string> VectorDatabase::getAllIds() const {
std::lock_guard<std::mutex> lock(mutex_);
std::vector<std::string> ids;
ids.reserve(records_.size());
for (const auto& pair : records_) {
ids.push_back(pair.first);
}
return ids;
}
void VectorDatabase::clear() {
std::lock_guard<std::mutex> lock(mutex_);
records_.clear();
}
bool VectorDatabase::saveToFile(const std::string& file_path) const {
std::lock_guard<std::mutex> lock(mutex_);
try {
std::ofstream file(file_path, std::ios::binary);
if (!file.is_open()) {
return false;
}
const char* magic = "BVDB";
file.write(magic, 4);
uint32_t version = 1;
file.write(reinterpret_cast<const char*>(&version), sizeof(version));
uint64_t num_records = records_.size();
file.write(reinterpret_cast<const char*>(&num_records), sizeof(num_records));
for (const auto& [id, record] : records_) {
uint32_t id_len = id.size();
file.write(reinterpret_cast<const char*>(&id_len), sizeof(id_len));
file.write(id.c_str(), id_len);
uint32_t content_len = record.content.size();
file.write(reinterpret_cast<const char*>(&content_len), sizeof(content_len));
file.write(record.content.c_str(), content_len);
uint32_t embedding_dim = record.embedding.size();
file.write(reinterpret_cast<const char*>(&embedding_dim), sizeof(embedding_dim));
file.write(reinterpret_cast<const char*>(record.embedding.data()),
embedding_dim * sizeof(float));
uint32_t metadata_size = record.metadata.size();
file.write(reinterpret_cast<const char*>(&metadata_size), sizeof(metadata_size));
for (const auto& [key, value] : record.metadata) {
uint32_t key_len = key.size();
file.write(reinterpret_cast<const char*>(&key_len), sizeof(key_len));
file.write(key.c_str(), key_len);
uint32_t value_len = value.size();
file.write(reinterpret_cast<const char*>(&value_len), sizeof(value_len));
file.write(value.c_str(), value_len);
}
file.write(reinterpret_cast<const char*>(&record.timestamp), sizeof(record.timestamp));
}
file.close();
return true;
} catch (const std::exception& e) {
return false;
}
}
bool VectorDatabase::loadFromFile(const std::string& file_path) {
std::lock_guard<std::mutex> lock(mutex_);
try {
std::ifstream file(file_path, std::ios::binary);
if (!file.is_open()) {
return false;
}
char magic[4];
file.read(magic, 4);
if (std::string(magic, 4) != "BVDB") {
return false;
}
uint32_t version;
file.read(reinterpret_cast<char*>(&version), sizeof(version));
if (version != 1) {
return false;
}
records_.clear();
uint64_t num_records;
file.read(reinterpret_cast<char*>(&num_records), sizeof(num_records));
for (uint64_t i = 0; i < num_records; ++i) {
VectorRecord record;
uint32_t id_len;
file.read(reinterpret_cast<char*>(&id_len), sizeof(id_len));
std::string id(id_len, '\0');
file.read(&id[0], id_len);
record.id = id;
uint32_t content_len;
file.read(reinterpret_cast<char*>(&content_len), sizeof(content_len));
record.content.resize(content_len);
file.read(&record.content[0], content_len);
uint32_t embedding_dim;
file.read(reinterpret_cast<char*>(&embedding_dim), sizeof(embedding_dim));
record.embedding.resize(embedding_dim);
file.read(reinterpret_cast<char*>(record.embedding.data()),
embedding_dim * sizeof(float));
uint32_t metadata_size;
file.read(reinterpret_cast<char*>(&metadata_size), sizeof(metadata_size));
for (uint32_t j = 0; j < metadata_size; ++j) {
uint32_t key_len;
file.read(reinterpret_cast<char*>(&key_len), sizeof(key_len));
std::string key(key_len, '\0');
file.read(&key[0], key_len);
uint32_t value_len;
file.read(reinterpret_cast<char*>(&value_len), sizeof(value_len));
std::string value(value_len, '\0');
file.read(&value[0], value_len);
record.metadata[key] = value;
}
file.read(reinterpret_cast<char*>(&record.timestamp), sizeof(record.timestamp));
records_[record.id] = record;
}
file.close();
return true;
} catch (const std::exception& e) {
records_.clear();
return false;
}
}
}
}