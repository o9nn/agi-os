#include <cassert>
#include <iostream>
#include <vector>
#include "bolt/editor/cursor_manager.hpp"
#include "bolt/editor/integrated_editor.hpp"
using namespace bolt;
void test_multi_cursor_basic() {
std::cout << "[MultiCursor] BasicOperations ... ";
CursorManager& manager = CursorManager::getInstance();
manager.clearCursors();
manager.addCursor(0, 0);
manager.addCursor(1, 5);
manager.addCursor(2, 10);
auto cursors = manager.getCursors();
assert(cursors.size() == 3);
assert(manager.getCursorCount() == 3);
assert(manager.hasCursors());
assert(cursors[0].line == 0 && cursors[0].column == 0);
assert(cursors[1].line == 1 && cursors[1].column == 5);
assert(cursors[2].line == 2 && cursors[2].column == 10);
std::cout << "PASS" << std::endl;
}
void test_multi_cursor_selection() {
std::cout << "[MultiCursor] SelectionOperations ... ";
CursorManager& manager = CursorManager::getInstance();
manager.clearCursors();
Selection selection{1, 5, 1, 10};
manager.addCursorWithSelection(1, 5, selection);
auto cursors = manager.getCursors();
assert(cursors.size() == 1);
assert(cursors[0].selection.hasSelection());
assert(cursors[0].selection.startLine == 1);
assert(cursors[0].selection.startColumn == 5);
assert(cursors[0].selection.endLine == 1);
assert(cursors[0].selection.endColumn == 10);
manager.clearAllSelections();
cursors = manager.getCursors();
assert(!cursors[0].selection.hasSelection());
std::cout << "PASS" << std::endl;
}
void test_multi_cursor_movement() {
std::cout << "[MultiCursor] MovementOperations ... ";
CursorManager& manager = CursorManager::getInstance();
manager.clearCursors();
manager.addCursor(5, 10);
manager.addCursor(10, 20);
manager.moveCursors(2, 3);
auto cursors = manager.getCursors();
assert(cursors[0].line == 7 && cursors[0].column == 13);
assert(cursors[1].line == 12 && cursors[1].column == 23);
manager.moveCursors(-10, -20);
cursors = manager.getCursors();
assert(cursors[0].line == 0 && cursors[0].column == 0);
assert(cursors[1].line == 2 && cursors[1].column == 3);
std::cout << "PASS" << std::endl;
}
void test_multi_cursor_primary() {
std::cout << "[MultiCursor] PrimaryCursorOperations ... ";
CursorManager& manager = CursorManager::getInstance();
manager.clearCursors();
manager.addCursor(1, 1);
manager.addCursor(2, 2);
manager.addCursor(3, 3);
manager.setPrimaryCursor(1);
auto primary = manager.getPrimaryCursor();
assert(primary.line == 2 && primary.column == 2);
manager.setPrimaryCursor(10);
primary = manager.getPrimaryCursor();
assert(primary.line == 0 && primary.column == 0);
std::cout << "PASS" << std::endl;
}
void test_multi_cursor_text_operations() {
std::cout << "[MultiCursor] TextOperations ... ";
CursorManager& manager = CursorManager::getInstance();
manager.clearCursors();
manager.addCursor(0, 5);
manager.addCursor(1, 10);
manager.insertTextAtCursors("Hello");
auto cursors = manager.getCursors();
bool foundFirst = false, foundSecond = false;
for (const auto& cursor : cursors) {
if (cursor.line == 0 && cursor.column == 10) foundFirst = true;
if (cursor.line == 1 && cursor.column == 15) foundSecond = true;
}
assert(foundFirst && foundSecond);
manager.deleteAtCursors();
cursors = manager.getCursors();
foundFirst = foundSecond = false;
for (const auto& cursor : cursors) {
if (cursor.line == 0 && cursor.column == 9) foundFirst = true;
if (cursor.line == 1 && cursor.column == 14) foundSecond = true;
}
assert(foundFirst && foundSecond);
std::cout << "PASS" << std::endl;
}
void test_multi_cursor_removal() {
std::cout << "[MultiCursor] RemovalOperations ... ";
CursorManager& manager = CursorManager::getInstance();
manager.clearCursors();
manager.addCursor(1, 1);
manager.addCursor(2, 2);
manager.addCursor(3, 3);
assert(manager.getCursorCount() == 3);
manager.removeCursor(2, 2);
assert(manager.getCursorCount() == 2);
auto cursors = manager.getCursors();
assert((cursors[0].line == 1 && cursors[0].column == 1) ||
(cursors[1].line == 1 && cursors[1].column == 1));
assert((cursors[0].line == 3 && cursors[0].column == 3) ||
(cursors[1].line == 3 && cursors[1].column == 3));
manager.clearCursors();
assert(manager.getCursorCount() == 0);
assert(!manager.hasCursors());
std::cout << "PASS" << std::endl;
}
void test_integrated_editor_multi_cursor() {
std::cout << "[IntegratedEditor] MultiCursorIntegration ... ";
IntegratedEditor& editor = IntegratedEditor::getInstance();
editor.addCursorAtPosition(0, 0);
editor.addCursorAtPosition(1, 5);
assert(editor.getCursorCount() == 2);
auto cursors = editor.getAllCursors();
assert(cursors.size() == 2);
editor.clearExtraCursors();
assert(editor.getCursorCount() == 1);
std::cout << "PASS" << std::endl;
}
void test_multi_cursor_find_operations() {
std::cout << "[MultiCursor] FindOperations ... ";
CursorManager& manager = CursorManager::getInstance();
manager.clearCursors();
manager.addCursor(0, 0);
manager.findNextOccurrence("test", "some document content");
assert(manager.getCursorCount() >= 1);
manager.selectAllOccurrences("test", "document content");
assert(manager.getCursorCount() > 1);
std::cout << "PASS" << std::endl;
}
int main() {
std::cout << "Multi-Cursor Editing Tests" << std::endl;
std::cout << "=========================" << std::endl;
try {
test_multi_cursor_basic();
test_multi_cursor_selection();
test_multi_cursor_movement();
test_multi_cursor_primary();
test_multi_cursor_text_operations();
test_multi_cursor_removal();
test_integrated_editor_multi_cursor();
test_multi_cursor_find_operations();
std::cout << std::endl << "All multi-cursor tests passed!" << std::endl;
return 0;
} catch (const std::exception& e) {
std::cerr << "Test failed with exception: " << e.what() << std::endl;
return 1;
}
}