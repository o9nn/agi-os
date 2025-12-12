#ifndef LSP_PROTOCOL_HPP
#define LSP_PROTOCOL_HPP

#include <string>
#include <vector>
#include <map>
#include <optional>
#include <memory>

namespace bolt {
namespace lsp {

// LSP Position
struct Position {
    size_t line;
    size_t character;
    
    Position(size_t l = 0, size_t c = 0) : line(l), character(c) {}
};

// LSP Range
struct Range {
    Position start;
    Position end;
    
    Range() = default;
    Range(const Position& s, const Position& e) : start(s), end(e) {}
};

// LSP Location
struct Location {
    std::string uri;
    Range range;
    
    Location() = default;
    Location(const std::string& u, const Range& r) : uri(u), range(r) {}
};

// LSP TextEdit
struct TextEdit {
    Range range;
    std::string newText;
    
    TextEdit() = default;
    TextEdit(const Range& r, const std::string& text) : range(r), newText(text) {}
};

// LSP Completion Item Kind
enum class CompletionItemKind {
    Text = 1,
    Method = 2,
    Function = 3,
    Constructor = 4,
    Field = 5,
    Variable = 6,
    Class = 7,
    Interface = 8,
    Module = 9,
    Property = 10,
    Unit = 11,
    Value = 12,
    Enum = 13,
    Keyword = 14,
    Snippet = 15,
    Color = 16,
    File = 17,
    Reference = 18
};

// LSP Completion Item
struct CompletionItem {
    std::string label;
    std::optional<CompletionItemKind> kind;
    std::optional<std::string> detail;
    std::optional<std::string> documentation;
    std::optional<std::string> insertText;
    std::optional<TextEdit> textEdit;
    
    CompletionItem() = default;
    CompletionItem(const std::string& l) : label(l) {}
};

// LSP Completion List
struct CompletionList {
    bool isIncomplete = false;
    std::vector<CompletionItem> items;
};

// LSP Diagnostic Severity
enum class DiagnosticSeverity {
    Error = 1,
    Warning = 2,
    Information = 3,
    Hint = 4
};

// LSP Diagnostic
struct Diagnostic {
    Range range;
    std::optional<DiagnosticSeverity> severity;
    std::optional<std::string> code;
    std::optional<std::string> source;
    std::string message;
    
    Diagnostic() = default;
    Diagnostic(const Range& r, const std::string& msg) : range(r), message(msg) {}
};

// LSP Hover
struct Hover {
    std::string contents;
    std::optional<Range> range;
    
    Hover() = default;
    Hover(const std::string& c) : contents(c) {}
};

// LSP Document Symbol Kind
enum class SymbolKind {
    File = 1,
    Module = 2,
    Namespace = 3,
    Package = 4,
    Class = 5,
    Method = 6,
    Property = 7,
    Field = 8,
    Constructor = 9,
    Enum = 10,
    Interface = 11,
    Function = 12,
    Variable = 13,
    Constant = 14,
    String = 15,
    Number = 16,
    Boolean = 17,
    Array = 18
};

// LSP Document Symbol
struct DocumentSymbol {
    std::string name;
    std::optional<std::string> detail;
    SymbolKind kind;
    Range range;
    Range selectionRange;
    std::vector<DocumentSymbol> children;
    
    DocumentSymbol() = default;
    DocumentSymbol(const std::string& n, SymbolKind k, const Range& r, const Range& sr) 
        : name(n), kind(k), range(r), selectionRange(sr) {}
};

// LSP Formatting Options
struct FormattingOptions {
    size_t tabSize = 4;
    bool insertSpaces = true;
    std::map<std::string, std::string> additional;
};

} // namespace lsp
} // namespace bolt

#endif