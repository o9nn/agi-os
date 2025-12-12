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
        
        // Test basic fold range creation
        TEST_CASE("FoldRangeCreation") {
            FoldRange range{10, 20, false, "test"};
            ASSERT_EQ(range.startLine, 10);
            ASSERT_EQ(range.endLine, 20);
            ASSERT_EQ(range.isFolded, false);
            ASSERT_EQ(range.placeholder, "test");
        }
        
        // Test CodeFolding singleton
        TEST_CASE("CodeFoldingSingleton") {
            auto& folding1 = CodeFolding::getInstance();
            auto& folding2 = CodeFolding::getInstance();
            ASSERT_EQ(&folding1, &folding2);
        }
        
        // Test adding fold ranges
        TEST_CASE("AddFoldRange") {
            auto& folding = CodeFolding::getInstance();
            folding.addFoldRange("/test/file.cpp", 5, 15);
            
            auto ranges = folding.getFoldingRanges("/test/file.cpp");
            ASSERT_EQ(ranges.size(), 1);
            ASSERT_EQ(ranges[0].startLine, 5);
            ASSERT_EQ(ranges[0].endLine, 15);
            ASSERT_EQ(ranges[0].isFolded, false);
        }
        
        // Test toggling folds
        TEST_CASE("ToggleFold") {
            auto& folding = CodeFolding::getInstance();
            folding.addFoldRange("/test/toggle.cpp", 8, 12);
            
            // Initially not folded
            auto ranges = folding.getFoldingRanges("/test/toggle.cpp");
            ASSERT_EQ(ranges[0].isFolded, false);
            
            // Toggle fold
            folding.toggleFold("/test/toggle.cpp", 10);
            ranges = folding.getFoldingRanges("/test/toggle.cpp");
            ASSERT_EQ(ranges[0].isFolded, true);
            
            // Toggle again
            folding.toggleFold("/test/toggle.cpp", 10);
            ranges = folding.getFoldingRanges("/test/toggle.cpp");
            ASSERT_EQ(ranges[0].isFolded, false);
        }
    }
    
    static void testCodeFoldingDetection() {
        TEST_SUITE("CodeFoldingDetector");
        
        // Test simple brace detection
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
        
        // Test nested braces
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
            ASSERT_GE(ranges.size(), 3); // Class, function, if statement
        }
        
        // Test comment folding
        TEST_CASE("CommentFolding") {
            std::string code = "/* This is a\n"
                              "   multi-line\n"
                              "   comment */\n"
                              "int x = 5;\n";
            
            auto ranges = CodeFoldingDetector::detectFoldableRanges(code);
            ASSERT_GE(ranges.size(), 1);
            
            // Find comment range
            bool foundComment = false;
            for (const auto& range : ranges) {
                if (range.placeholder == "/* ... */") {
                    foundComment = true;
                    ASSERT_EQ(range.startLine, 0);
                    ASSERT_EQ(range.endLine, 2);
                    break;
                }
            }
            ASSERT_TRUE(foundComment);
        }
        
        // Test foldable region detection
        TEST_CASE("FoldableRegionDetection") {
            ASSERT_TRUE(CodeFoldingDetector::isFoldableRegion("class MyClass {"));
            ASSERT_TRUE(CodeFoldingDetector::isFoldableRegion("  function test() {"));
            ASSERT_TRUE(CodeFoldingDetector::isFoldableRegion("if (condition) {"));
            ASSERT_FALSE(CodeFoldingDetector::isFoldableRegion("int x = 5;"));
        }
    }
    
    static void testFoldingManager() {
        TEST_SUITE("CodeFoldingManager");
        
        // Test manager singleton
        TEST_CASE("ManagerSingleton") {
            auto& manager1 = CodeFoldingManager::getInstance();
            auto& manager2 = CodeFoldingManager::getInstance();
            ASSERT_EQ(&manager1, &manager2);
        }
        
        // Test enabling/disabling folding
        TEST_CASE("EnableDisableFolding") {
            auto& manager = CodeFoldingManager::getInstance();
            
            manager.setFoldingEnabled(true);
            ASSERT_TRUE(manager.isFoldingEnabled());
            
            manager.setFoldingEnabled(false);
            ASSERT_FALSE(manager.isFoldingEnabled());
        }
        
        // Test updating folding ranges
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
        
        // Test toggle fold through manager
        TEST_CASE("ManagerToggleFold") {
            auto& manager = CodeFoldingManager::getInstance();
            manager.setFoldingEnabled(true);
            
            std::string code = "int func() {\n"
                              "    return 42;\n"
                              "}\n";
            
            manager.updateFoldingRanges("/test/toggle_mgr.cpp", code);
            
            // Check initial state
            auto ranges = manager.getFoldingRanges("/test/toggle_mgr.cpp");
            ASSERT_GE(ranges.size(), 1);
            bool initialState = ranges[0].isFolded;
            
            // Toggle
            manager.toggleFold("/test/toggle_mgr.cpp", ranges[0].startLine);
            
            // Check state changed
            ranges = manager.getFoldingRanges("/test/toggle_mgr.cpp");
            ASSERT_NE(ranges[0].isFolded, initialState);
        }
    }
    
    static void testIntegratedEditor() {
        TEST_SUITE("IntegratedEditor");
        
        // Test integrated editor singleton
        TEST_CASE("IntegratedEditorSingleton") {
            auto& editor1 = IntegratedEditor::getInstance();
            auto& editor2 = IntegratedEditor::getInstance();
            ASSERT_EQ(&editor1, &editor2);
        }
        
        // Test opening document with folding
        TEST_CASE("OpenDocumentWithFolding") {
            auto& editor = IntegratedEditor::getInstance();
            
            std::string code = "class TestClass {\n"
                              "public:\n"
                              "    void method() {\n"
                              "        // code here\n"
                              "    }\n"
                              "}\n";
            
            editor.openDocument("/test/integrated.cpp", code);
            
            auto ranges = editor.getFoldingRanges("/test/integrated.cpp");
            ASSERT_GE(ranges.size(), 1);
        }
        
        // Test folding operations through integrated editor
        TEST_CASE("IntegratedFoldingOperations") {
            auto& editor = IntegratedEditor::getInstance();
            editor.setFoldingEnabled(true);
            
            std::string code = "namespace test {\n"
                              "    int value = 100;\n"
                              "}\n";
            
            editor.openDocument("/test/operations.cpp", code);
            
            auto ranges = editor.getFoldingRanges("/test/operations.cpp");
            if (!ranges.empty()) {
                // Test toggle
                editor.toggleFold("/test/operations.cpp", ranges[0].startLine);
                
                // Test expand all
                editor.expandAllFolds("/test/operations.cpp");
                
                // Test collapse all
                editor.collapseAllFolds("/test/operations.cpp");
            }
            
            ASSERT_TRUE(editor.isFoldingEnabled());
        }
        
        // Test content update with re-folding
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
            
            // New code should have different folding ranges
            ASSERT_GE(newRanges.size(), originalRanges.size());
        }
    }
};

} // namespace bolt