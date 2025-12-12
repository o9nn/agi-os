#ifndef LSP_CLIENT_HPP
#define LSP_CLIENT_HPP

#include <string>
#include <vector>
#include <optional>
#include <unordered_map>
#include <mutex>
#include <json/json.h>

namespace bolt {
namespace lsp {

// LSP data structures

struct Position {
    int line;
    int character;
};

struct Range {
    Position start;
    Position end;
};

struct Location {
    std::string uri;
    Range range;
};

struct TextDocumentContentChangeEvent {
    std::optional<Range> range;
    std::string text;
};

struct CompletionItem {
    std::string label;
    int kind;  // CompletionItemKind
    std::string detail;
    std::string documentation;
    std::string insertText;
};

struct Diagnostic {
    Range range;
    int severity;  // 1=Error, 2=Warning, 3=Information, 4=Hint
    std::string message;
    std::string source;
};

struct Hover {
    std::string contents;
    std::optional<Range> range;
};

// LSP Client implementation

class LSPClient {
public:
    explicit LSPClient(const std::string& server_command);
    ~LSPClient();
    
    // Lifecycle
    bool initialize(const std::string& root_uri);
    void shutdown();
    bool isInitialized() const { return initialized_; }
    
    // Document synchronization
    void didOpen(const std::string& uri, const std::string& language_id, const std::string& text);
    void didChange(const std::string& uri, int version, const std::vector<TextDocumentContentChangeEvent>& changes);
    void didClose(const std::string& uri);
    
    // Language features
    std::vector<CompletionItem> completion(const std::string& uri, int line, int character);
    std::optional<Location> gotoDefinition(const std::string& uri, int line, int character);
    std::vector<Location> findReferences(const std::string& uri, int line, int character, bool include_declaration = false);
    std::optional<Hover> hover(const std::string& uri, int line, int character);
    
    // Diagnostics
    std::vector<Diagnostic> getDiagnostics(const std::string& uri);
    
private:
    std::string server_command_;
    bool initialized_;
    int next_request_id_;
    
    // Diagnostics storage
    std::unordered_map<std::string, std::vector<Diagnostic>> diagnostics_;
    std::mutex diagnostics_mutex_;
    
    // Communication
    Json::Value sendRequest(const std::string& method, const Json::Value& params);
    void sendNotification(const std::string& method, const Json::Value& params);
    
    // Server process management
    bool startServer();
    void stopServer();
    
    // Message handling
    bool sendMessage(const std::string& message);
    Json::Value waitForResponse(int request_id);
    void handleNotification(const Json::Value& notification);
    void handleDiagnostics(const Json::Value& params);
    
    // Utilities
    Json::Value buildClientCapabilities();
    std::string jsonToString(const Json::Value& json);
};

// LSP Manager - manages multiple LSP clients for different languages

class LSPManager {
public:
    static LSPManager& getInstance() {
        static LSPManager instance;
        return instance;
    }
    
    // Register language server
    void registerServer(const std::string& language_id, const std::string& server_command);
    
    // Get client for language
    LSPClient* getClient(const std::string& language_id);
    
    // Initialize all registered servers
    void initializeAll(const std::string& root_uri);
    
    // Shutdown all servers
    void shutdownAll();
    
private:
    LSPManager() = default;
    
    std::unordered_map<std::string, std::unique_ptr<LSPClient>> clients_;
    std::unordered_map<std::string, std::string> server_commands_;
    std::mutex mutex_;
};

} // namespace lsp
} // namespace bolt

#endif // LSP_CLIENT_HPP
