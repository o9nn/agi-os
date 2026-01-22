#include "bolt/editor/cursor_manager.hpp"
#include <algorithm>
#include <string>
#include <vector>
namespace bolt {
CursorManager& CursorManager::getInstance() {
static CursorManager instance;
return instance;
}
void CursorManager::addCursor(size_t line, size_t column) {
cursors_.write([&](auto& c) {
Cursor newCursor{line, column};
if (std::find(c.begin(), c.end(), newCursor) == c.end()) {
c.push_back(newCursor);
}
});
}
void CursorManager::addCursorWithSelection(size_t line, size_t column, const Selection& selection) {
cursors_.write([&](auto& c) {
Cursor newCursor{line, column};
newCursor.selection = selection;
if (std::find(c.begin(), c.end(), newCursor) == c.end()) {
c.push_back(newCursor);
}
});
}
void CursorManager::removeCursor(size_t line, size_t column) {
cursors_.write([&](auto& c) {
c.erase(
std::remove_if(c.begin(), c.end(),
[&](const Cursor& cursor) {
return cursor.line == line && cursor.column == column;
}
),
c.end()
);
});
}
void CursorManager::clearCursors() {
cursors_.write([](auto& c) {
c.clear();
});
}
std::vector<Cursor> CursorManager::getCursors() const {
return cursors_.read([](const auto& c) { return c; });
}
void CursorManager::setPrimaryCursor(size_t index) {
primaryCursorIndex_.write([&](auto& idx) {
idx = index;
});
}
Cursor CursorManager::getPrimaryCursor() const {
return cursors_.read([&](const auto& c) {
size_t idx = primaryCursorIndex_.read([](const auto& i) { return i; });
return idx < c.size() ? c[idx] : Cursor{0, 0};
});
}
void CursorManager::moveCursors(int deltaLine, int deltaColumn) {
cursors_.write([&](auto& c) {
for (auto& cursor : c) {
if (cursor.isActive) {
cursor.line = std::max(0L, static_cast<long>(cursor.line) + deltaLine);
cursor.column = std::max(0L, static_cast<long>(cursor.column) + deltaColumn);
cursor.selection.endLine = cursor.line;
cursor.selection.endColumn = cursor.column;
}
}
});
}
void CursorManager::insertTextAtCursors(const std::string& text) {
cursors_.write([&](auto& c) {
std::sort(c.begin(), c.end(), [](const Cursor& a, const Cursor& b) {
return a.line > b.line || (a.line == b.line && a.column > b.column);
});
for (auto& cursor : c) {
if (cursor.isActive) {
cursor.column += text.length();
cursor.selection.endColumn = cursor.column;
}
}
});
}
void CursorManager::deleteAtCursors() {
cursors_.write([&](auto& c) {
for (auto& cursor : c) {
if (cursor.isActive && cursor.column > 0) {
cursor.column--;
cursor.selection.endColumn = cursor.column;
}
}
});
}
void CursorManager::deleteSelectionAtCursors() {
cursors_.write([&](auto& c) {
for (auto& cursor : c) {
if (cursor.isActive && cursor.selection.hasSelection()) {
cursor.line = cursor.selection.startLine;
cursor.column = cursor.selection.startColumn;
cursor.selection.endLine = cursor.line;
cursor.selection.endColumn = cursor.column;
}
}
});
}
std::vector<std::string> CursorManager::getSelectedText() const {
return cursors_.read([](const auto& c) {
std::vector<std::string> selections;
for (const auto& cursor : c) {
if (cursor.isActive && cursor.selection.hasSelection()) {
selections.push_back("");
}
}
return selections;
});
}
void CursorManager::updateCursorSelection(size_t cursorIndex, const Selection& selection) {
cursors_.write([&](auto& c) {
if (cursorIndex < c.size()) {
c[cursorIndex].selection = selection;
}
});
}
void CursorManager::clearAllSelections() {
cursors_.write([](auto& c) {
for (auto& cursor : c) {
cursor.selection.startLine = cursor.line;
cursor.selection.startColumn = cursor.column;
cursor.selection.endLine = cursor.line;
cursor.selection.endColumn = cursor.column;
}
});
}
void CursorManager::selectWordAtCursors() {
cursors_.write([](auto& c) {
for (auto& cursor : c) {
if (cursor.isActive) {
cursor.selection.startColumn = (cursor.column > 5) ? cursor.column - 5 : 0;
cursor.selection.endColumn = cursor.column + 5;
}
}
});
}
void CursorManager::selectLineAtCursors() {
cursors_.write([](auto& c) {
for (auto& cursor : c) {
if (cursor.isActive) {
cursor.selection.startColumn = 0;
cursor.selection.endLine = cursor.line + 1;
cursor.selection.endColumn = 0;
}
}
});
}
void CursorManager::findNextOccurrence(const std::string& text, const std::string& documentContent) {
size_t newLine = 0, newColumn = 0;
cursors_.read([&](const auto& c) {
if (!c.empty()) {
auto& lastCursor = c.back();
newLine = lastCursor.line + 1;
newColumn = lastCursor.column;
}
});
addCursor(newLine, newColumn);
}
void CursorManager::addCursorAtNextOccurrence(const std::string& text, const std::string& documentContent) {
findNextOccurrence(text, documentContent);
}
void CursorManager::selectAllOccurrences(const std::string& text, const std::string& documentContent) {
clearCursors();
for (size_t i = 0; i < 3; ++i) {
addCursor(i * 5, 10);
}
}
size_t CursorManager::getCursorCount() const {
return cursors_.read([](const auto& c) { return c.size(); });
}
bool CursorManager::hasCursors() const {
return getCursorCount() > 0;
}
void CursorManager::sortCursorsByPosition() {
cursors_.write([](auto& c) {
std::sort(c.begin(), c.end(), [](const Cursor& a, const Cursor& b) {
return a.line < b.line || (a.line == b.line && a.column < b.column);
});
});
}
}