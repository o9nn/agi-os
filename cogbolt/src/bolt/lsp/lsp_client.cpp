#include "bolt/lsp/lsp_client.hpp"
#include <iostream>
#include <sstream>
#include <json/json.h>
namespace bolt {
namespace lsp {
LSPClient::LSPClient(const std::string& server_command)
: server_command_(server_command), initialized_(false), next_request_id_(1) {
}
LSPClient::~LSPClient() {
shutdown();
}
bool LSPClient::initialize(const std::string& root_uri) {
if (initialized_) {
return true;
}
if (!startServer()) {
std::cerr << "Failed to start LSP server" << std::endl;
return false;
}
Json::Value params;
params["processId"] = static_cast<int>(getpid());
params["rootUri"] = root_uri;
params["capabilities"] = buildClientCapabilities();
auto response = sendRequest("initialize", params);
if (response.isNull()) {
std::cerr << "Initialize request failed" << std::endl;
return false;
}
sendNotification("initialized", Json::Value());
initialized_ = true;
return true;
}
void LSPClient::shutdown() {
if (!initialized_) {
return;
}
sendRequest("shutdown", Json::Value());
sendNotification("exit", Json::Value());
stopServer();
initialized_ = false;
}
void LSPClient::didOpen(const std::string& uri, const std::string& language_id, const std::string& text) {
Json::Value params;
params["textDocument"]["uri"] = uri;
params["textDocument"]["languageId"] = language_id;
params["textDocument"]["version"] = 1;
params["textDocument"]["text"] = text;
sendNotification("textDocument/didOpen", params);
}
void LSPClient::didChange(const std::string& uri, int version, const std::vector<TextDocumentContentChangeEvent>& changes) {
Json::Value params;
params["textDocument"]["uri"] = uri;
params["textDocument"]["version"] = version;
Json::Value changesArray(Json::arrayValue);
for (const auto& change : changes) {
Json::Value changeJson;
if (change.range.has_value()) {
const auto& range = change.range.value();
changeJson["range"]["start"]["line"] = range.start.line;
changeJson["range"]["start"]["character"] = range.start.character;
changeJson["range"]["end"]["line"] = range.end.line;
changeJson["range"]["end"]["character"] = range.end.character;
}
changeJson["text"] = change.text;
changesArray.append(changeJson);
}
params["contentChanges"] = changesArray;
sendNotification("textDocument/didChange", params);
}
void LSPClient::didClose(const std::string& uri) {
Json::Value params;
params["textDocument"]["uri"] = uri;
sendNotification("textDocument/didClose", params);
}
std::vector<CompletionItem> LSPClient::completion(const std::string& uri, int line, int character) {
Json::Value params;
params["textDocument"]["uri"] = uri;
params["position"]["line"] = line;
params["position"]["character"] = character;
auto response = sendRequest("textDocument/completion", params);
std::vector<CompletionItem> items;
if (response.isNull()) {
return items;
}
Json::Value itemsArray = response.isArray() ? response : response["items"];
for (const auto& item : itemsArray) {
CompletionItem completion;
completion.label = item["label"].asString();
completion.kind = item.get("kind", 1).asInt();
completion.detail = item.get("detail", "").asString();
completion.documentation = item.get("documentation", "").asString();
completion.insertText = item.get("insertText", completion.label).asString();
items.push_back(completion);
}
return items;
}
std::optional<Location> LSPClient::gotoDefinition(const std::string& uri, int line, int character) {
Json::Value params;
params["textDocument"]["uri"] = uri;
params["position"]["line"] = line;
params["position"]["character"] = character;
auto response = sendRequest("textDocument/definition", params);
if (response.isNull()) {
return std::nullopt;
}
Json::Value locationJson = response.isArray() ? response[0] : response;
Location location;
location.uri = locationJson["uri"].asString();
location.range.start.line = locationJson["range"]["start"]["line"].asInt();
location.range.start.character = locationJson["range"]["start"]["character"].asInt();
location.range.end.line = locationJson["range"]["end"]["line"].asInt();
location.range.end.character = locationJson["range"]["end"]["character"].asInt();
return location;
}
std::vector<Location> LSPClient::findReferences(const std::string& uri, int line, int character, bool include_declaration) {
Json::Value params;
params["textDocument"]["uri"] = uri;
params["position"]["line"] = line;
params["position"]["character"] = character;
params["context"]["includeDeclaration"] = include_declaration;
auto response = sendRequest("textDocument/references", params);
std::vector<Location> locations;
if (response.isNull() || !response.isArray()) {
return locations;
}
for (const auto& locationJson : response) {
Location location;
location.uri = locationJson["uri"].asString();
location.range.start.line = locationJson["range"]["start"]["line"].asInt();
location.range.start.character = locationJson["range"]["start"]["character"].asInt();
location.range.end.line = locationJson["range"]["end"]["line"].asInt();
location.range.end.character = locationJson["range"]["end"]["character"].asInt();
locations.push_back(location);
}
return locations;
}
std::optional<Hover> LSPClient::hover(const std::string& uri, int line, int character) {
Json::Value params;
params["textDocument"]["uri"] = uri;
params["position"]["line"] = line;
params["position"]["character"] = character;
auto response = sendRequest("textDocument/hover", params);
if (response.isNull()) {
return std::nullopt;
}
Hover hover;
Json::Value contents = response["contents"];
if (contents.isString()) {
hover.contents = contents.asString();
} else if (contents.isObject() && contents.isMember("value")) {
hover.contents = contents["value"].asString();
} else if (contents.isArray() && contents.size() > 0) {
hover.contents = contents[0].asString();
}
if (response.isMember("range")) {
Range range;
range.start.line = response["range"]["start"]["line"].asInt();
range.start.character = response["range"]["start"]["character"].asInt();
range.end.line = response["range"]["end"]["line"].asInt();
range.end.character = response["range"]["end"]["character"].asInt();
hover.range = range;
}
return hover;
}
std::vector<Diagnostic> LSPClient::getDiagnostics(const std::string& uri) {
std::lock_guard<std::mutex> lock(diagnostics_mutex_);
auto it = diagnostics_.find(uri);
if (it != diagnostics_.end()) {
return it->second;
}
return {};
}
Json::Value LSPClient::sendRequest(const std::string& method, const Json::Value& params) {
int request_id = next_request_id_++;
Json::Value request;
request["jsonrpc"] = "2.0";
request["id"] = request_id;
request["method"] = method;
request["params"] = params;
std::string message = jsonToString(request);
if (!sendMessage(message)) {
return Json::Value();
}
return waitForResponse(request_id);
}
void LSPClient::sendNotification(const std::string& method, const Json::Value& params) {
Json::Value notification;
notification["jsonrpc"] = "2.0";
notification["method"] = method;
notification["params"] = params;
std::string message = jsonToString(notification);
sendMessage(message);
}
bool LSPClient::startServer() {
std::cout << "Starting LSP server: " << server_command_ << std::endl;
return true;
}
void LSPClient::stopServer() {
std::cout << "Stopping LSP server" << std::endl;
}
bool LSPClient::sendMessage(const std::string& message) {
std::string header = "Content-Length: " + std::to_string(message.length()) + "\r\n\r\n";
std::string full_message = header + message;
std::cout << "Sending LSP message: " << method << std::endl;
return true;
}
Json::Value LSPClient::waitForResponse(int request_id) {
return Json::Value();
}
void LSPClient::handleNotification(const Json::Value& notification) {
std::string method = notification["method"].asString();
if (method == "textDocument/publishDiagnostics") {
handleDiagnostics(notification["params"]);
}
}
void LSPClient::handleDiagnostics(const Json::Value& params) {
std::string uri = params["uri"].asString();
std::vector<Diagnostic> diagnostics;
for (const auto& diagJson : params["diagnostics"]) {
Diagnostic diag;
diag.range.start.line = diagJson["range"]["start"]["line"].asInt();
diag.range.start.character = diagJson["range"]["start"]["character"].asInt();
diag.range.end.line = diagJson["range"]["end"]["line"].asInt();
diag.range.end.character = diagJson["range"]["end"]["character"].asInt();
diag.severity = diagJson.get("severity", 1).asInt();
diag.message = diagJson["message"].asString();
diag.source = diagJson.get("source", "").asString();
diagnostics.push_back(diag);
}
std::lock_guard<std::mutex> lock(diagnostics_mutex_);
diagnostics_[uri] = diagnostics;
}
Json::Value LSPClient::buildClientCapabilities() {
Json::Value capabilities;
capabilities["textDocument"]["completion"]["completionItem"]["snippetSupport"] = true;
capabilities["textDocument"]["completion"]["completionItem"]["commitCharactersSupport"] = true;
capabilities["textDocument"]["hover"]["contentFormat"] = Json::arrayValue;
capabilities["textDocument"]["hover"]["contentFormat"].append("markdown");
capabilities["textDocument"]["hover"]["contentFormat"].append("plaintext");
capabilities["textDocument"]["synchronization"]["dynamicRegistration"] = true;
capabilities["textDocument"]["synchronization"]["willSave"] = true;
capabilities["textDocument"]["synchronization"]["didSave"] = true;
return capabilities;
}
std::string LSPClient::jsonToString(const Json::Value& json) {
Json::StreamWriterBuilder builder;
builder["indentation"] = "";
return Json::writeString(builder, json);
}
}
}