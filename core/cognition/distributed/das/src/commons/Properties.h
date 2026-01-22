#pragma once
#include <algorithm>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>
#include "Utils.h"
using namespace std;
namespace commons {
using PropertyValue = variant<string, long, unsigned int, double, bool>;
class Properties : public unordered_map<string, PropertyValue> {
public:
using unordered_map<string,
PropertyValue>::unordered_map;
template <typename K, typename V>
Properties(std::initializer_list<std::pair<K, V>> init)
: unordered_map<string, PropertyValue>(init.begin(), init.end()) {}
template <typename T>
const T* get_ptr(const string& key) const {
auto it = this->find(key);
if (it != this->end()) {
if (auto attr = std::get_if<T>(&it->second)) {
return attr;
}
}
return nullptr;
}
template <typename T>
const T get(const string& key) const {
auto it = this->find(key);
if (it != this->end()) {
if (auto attr = std::get_if<T>(&it->second)) {
return *attr;
}
}
Utils::error("Unkown property key: " + key);
}
template <typename T>
const T get_or(const string& key, const T& default_value) const {
auto it = this->find(key);
if (it != this->end()) {
if (auto attr = std::get_if<T>(&it->second)) {
return *attr;
}
}
return default_value;
}
string to_string() const {
string result = "{";
if (this->empty()) {
return result + "}";
}
vector<string> keys;
for (const auto& pair : *this) {
keys.push_back(pair.first);
}
std::sort(keys.begin(), keys.end());
bool empty_flag = true;
for (const auto& key : keys) {
const auto& value = this->at(key);
result += key + ": ";
if (auto str = std::get_if<string>(&value)) {
result += "'" + *str + "'";
} else if (auto longint = std::get_if<long>(&value)) {
result += std::to_string(*longint);
} else if (auto uinteger = std::get_if<unsigned int>(&value)) {
result += std::to_string(*uinteger);
} else if (auto floating = std::get_if<double>(&value)) {
result += std::to_string(*floating);
} else if (auto boolean = std::get_if<bool>(&value)) {
result += *boolean ? "true" : "false";
}
result += ", ";
empty_flag = false;
}
if (!empty_flag) {
result.pop_back();
result.pop_back();
}
return result + "}";
}
vector<string> tokenize() const {
vector<string> result;
if (!this->empty()) {
vector<string> keys;
for (const auto& pair : *this) {
keys.push_back(pair.first);
}
std::sort(keys.begin(), keys.end());
for (const auto& key : keys) {
result.push_back(key);
const auto& value = this->at(key);
if (auto str = std::get_if<string>(&value)) {
result.push_back("string");
result.push_back(*str);
} else if (auto longint = std::get_if<long>(&value)) {
result.push_back("long");
result.push_back(std::to_string(*longint));
} else if (auto uinteger = std::get_if<unsigned int>(&value)) {
result.push_back("unsigned_int");
result.push_back(std::to_string(*uinteger));
} else if (auto floating = std::get_if<double>(&value)) {
result.push_back("double");
result.push_back(std::to_string(*floating));
} else if (auto boolean = std::get_if<bool>(&value)) {
result.push_back("bool");
result.push_back(*boolean ? "true" : "false");
}
}
}
return result;
}
void untokenize(const vector<string>& tokens) {
if ((tokens.size() % 3) == 0) {
unsigned int cursor = 0;
while (cursor != tokens.size()) {
string key = tokens[cursor++];
string type = tokens[cursor++];
string value = tokens[cursor++];
if (type == "string") {
(*this)[key] = value;
} else if (type == "long") {
(*this)[key] = (long) stoi(value);
} else if (type == "unsigned_int") {
(*this)[key] = (unsigned int) stoi(value);
} else if (type == "double") {
(*this)[key] = stod(value);
} else if (type == "bool") {
if (value == "true") {
(*this)[key] = true;
} else if (value == "false") {
(*this)[key] = false;
} else {
Utils::error("Invalid 'bool' string value: " + value);
}
} else {
Utils::error("Invalid token type: " + type);
}
}
} else {
Utils::error("Invalid tokens vector size: " + std::to_string(tokens.size()));
}
}
};
}