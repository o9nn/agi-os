#include "bolt/test_framework.hpp"
#include "bolt/editor/code_folding.hpp"
#include "bolt/editor/code_folding_detector.hpp"
#include "bolt/editor/code_folding_manager.hpp"
#include "bolt/editor/integrated_editor.hpp"
#include <iostream>
namespace bolt {
class CodeFoldingTests {
public:
static void runAllTests() {
std::cout << "Running Code Folding Tests...\n";
testFoldRangeCreation();
testCodeFoldingDetection();
testFoldingManager();
testIntegratedEditor();
std::cout << "Code Folding Tests completed.\n";
}
private:
static void testFoldRangeCreation() {
TEST_SUITE("CodeFolding");
TEST_CASE("FoldRangeCreation") {
FoldRange range{10, 20, false, "test"};
ASSERT_EQ(range.startLine, 10);
ASSERT_EQ(range.endLine, 20);
ASSERT_EQ(range.isFolded, false);
ASSERT_EQ(range.placeholder, "test");
}
TEST_CASE("CodeFoldingSingleton") {
auto& folding1 = CodeFolding::getInstance();
auto& folding2 = CodeFolding::getInstance();
ASSERT_EQ(&folding1, &folding2);
}
TEST_CASE("AddFoldRange") {
auto& folding = CodeFolding::getInstance();
folding.addFoldRange("/test/file.cpp", 5, 15);
auto ranges = folding.getFoldingRanges("/test/file.cpp");
ASSERT_EQ(ranges.size(), 1);
ASSERT_EQ(ranges[0].startLine, 5);
ASSERT_EQ(ranges[0].endLine, 15);
ASSERT_EQ(ranges[0].isFolded, false);
}
TEST_CASE("ToggleFold") {
auto& folding = CodeFolding::getInstance();
folding.addFoldRange("/test/toggle.cpp", 8, 12);
auto ranges = folding.getFoldingRanges("/test/toggle.cpp");
ASSERT_EQ(ranges[0].isFolded, false);
folding.toggleFold("/test/toggle.cpp", 10);
ranges = folding.getFoldingRanges("/test/toggle.cpp");
ASSERT_EQ(ranges[0].isFolded, true);
folding.toggleFold("/test/toggle.cpp", 10);
ranges = folding.getFoldingRanges("/test/toggle.cpp");
ASSERT_EQ(ranges[0].isFolded, false);
}
}
static void testCodeFoldingDetection() {
TEST_SUITE("CodeFoldingDetector");
TEST_CASE("SimpleBraceDetection") {
std::string code = "int main() {\n"
"    return 0;\n"
"}\n";
auto ranges = CodeFoldingDetector::detectFoldableRanges(code);
ASSERT_EQ(ranges.size(), 1);
ASSERT_EQ(ranges[0].startLine, 0);
ASSERT_EQ(ranges[0].endLine, 2);
ASSERT_EQ(ranges[0].isFolded, false);
ASSERT_EQ(ranges[0].placeholder, "{ ... }");
}
TEST_CASE("NestedBraces") {
std::string code = "class Test {\n"
"public:\n"
"    void func() {\n"
"        if (true) {\n"
"            return;\n"
"        }\n"
"    }\n"
"}\n";
auto ranges = CodeFoldingDetector::detectFoldableRanges(code);
ASSERT_GE(ranges.size(), 3);
}
TEST_CASE("CommentFolding") {
std::string code = "\n"
"int x = 5;\n";
auto ranges = CodeFoldingDetector::detectFoldableRanges(code);
ASSERT_GE(ranges.size(), 1);
bool foundComment = false;
for (const auto& range : ranges) {
if (range.placeholder == "") {
foundComment = true;
ASSERT_EQ(range.startLine, 0);
ASSERT_EQ(range.endLine, 2);
break;
}
}
ASSERT_TRUE(foundComment);
}
TEST_CASE("FoldableRegionDetection") {
ASSERT_TRUE(CodeFoldingDetector::isFoldableRegion("class MyClass {"));
ASSERT_TRUE(CodeFoldingDetector::isFoldableRegion("  function test() {"));
ASSERT_TRUE(CodeFoldingDetector::isFoldableRegion("if (condition) {"));
ASSERT_FALSE(CodeFoldingDetector::isFoldableRegion("int x = 5;"));
}
}
static void testFoldingManager() {
TEST_SUITE("CodeFoldingManager");
TEST_CASE("ManagerSingleton") {
auto& manager1 = CodeFoldingManager::getInstance();
auto& manager2 = CodeFoldingManager::getInstance();
ASSERT_EQ(&manager1, &manager2);
}
TEST_CASE("EnableDisableFolding") {
auto& manager = CodeFoldingManager::getInstance();
manager.setFoldingEnabled(true);
ASSERT_TRUE(manager.isFoldingEnabled());
manager.setFoldingEnabled(false);
ASSERT_FALSE(manager.isFoldingEnabled());
}
TEST_CASE("UpdateFoldingRanges") {
auto& manager = CodeFoldingManager::getInstance();
manager.setFoldingEnabled(true);
std::string code = "void test() {\n"
"    int x = 1;\n"
"}\n";
manager.updateFoldingRanges("/test/manager.cpp", code);
auto ranges = manager.getFoldingRanges("/test/manager.cpp");
ASSERT_GE(ranges.size(), 1);
}
TEST_CASE("ManagerToggleFold") {
auto& manager = CodeFoldingManager::getInstance();
manager.setFoldingEnabled(true);
std::string code = "int func() {\n"
"    return 42;\n"
"}\n";
manager.updateFoldingRanges("/test/toggle_mgr.cpp", code);
auto ranges = manager.getFoldingRanges("/test/toggle_mgr.cpp");
ASSERT_GE(ranges.size(), 1);
bool initialState = ranges[0].isFolded;
manager.toggleFold("/test/toggle_mgr.cpp", ranges[0].startLine);
ranges = manager.getFoldingRanges("/test/toggle_mgr.cpp");
ASSERT_NE(ranges[0].isFolded, initialState);
}
}
static void testIntegratedEditor() {
TEST_SUITE("IntegratedEditor");
TEST_CASE("IntegratedEditorSingleton") {
auto& editor1 = IntegratedEditor::getInstance();
auto& editor2 = IntegratedEditor::getInstance();
ASSERT_EQ(&editor1, &editor2);
}
TEST_CASE("OpenDocumentWithFolding") {
auto& editor = IntegratedEditor::getInstance();
std::string code = "class TestClass {\n"
"public:\n"
"    void method() {\n"
"
"    }\n"
"}\n";
editor.openDocument("/test/integrated.cpp", code);
auto ranges = editor.getFoldingRanges("/test/integrated.cpp");
ASSERT_GE(ranges.size(), 1);
}
TEST_CASE("IntegratedFoldingOperations") {
auto& editor = IntegratedEditor::getInstance();
editor.setFoldingEnabled(true);
std::string code = "namespace test {\n"
"    int value = 100;\n"
"}\n";
editor.openDocument("/test/operations.cpp", code);
auto ranges = editor.getFoldingRanges("/test/operations.cpp");
if (!ranges.empty()) {
editor.toggleFold("/test/operations.cpp", ranges[0].startLine);
editor.expandAllFolds("/test/operations.cpp");
editor.collapseAllFolds("/test/operations.cpp");
}
ASSERT_TRUE(editor.isFoldingEnabled());
}
TEST_CASE("UpdateContentRefolding") {
auto& editor = IntegratedEditor::getInstance();
std::string originalCode = "int x = 1;\n";
std::string newCode = "struct Data {\n"
"    int value;\n"
"};\n";
editor.openDocument("/test/update.cpp", originalCode);
auto originalRanges = editor.getFoldingRanges("/test/update.cpp");
editor.updateDocumentContent("/test/update.cpp", newCode);
auto newRanges = editor.getFoldingRanges("/test/update.cpp");
ASSERT_GE(newRanges.size(), originalRanges.size());
}
}
};
}