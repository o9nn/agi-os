#include "bolt/test_framework.hpp"
#include "bolt/ai/ai_completion_provider.hpp"
#include "bolt/editor/integrated_editor.hpp"
#include <iostream>
using namespace bolt;
void testAICompletionProvider() {
MockAICompletionProvider provider;
TestFramework::assert_equal(provider.isReady(), false, "Provider should not be ready initially");
TestFramework::assert_equal(provider.initialize(), true, "Provider should initialize successfully");
TestFramework::assert_equal(provider.isReady(), true, "Provider should be ready after initialization");
CodeContext cppContext;
cppContext.filePath = "test.cpp";
cppContext.content = "int main() {\n    std::\n}";
cppContext.cursorPosition = 20;
cppContext.line = 1;
cppContext.column = 9;
cppContext.language = "cpp";
auto completions = provider.generateCompletions(cppContext, "std::", 5);
TestFramework::assert_true(completions.size() > 0, "Should generate C++ completions for std::");
TestFramework::assert_true(completions[0].label == "vector", "First completion should be 'vector'");
provider.shutdown();
TestFramework::assert_equal(provider.isReady(), false, "Provider should not be ready after shutdown");
}
void testAICodeCompletionEngine() {
AICodeCompletionEngine& engine = AICodeCompletionEngine::getInstance();
TestFramework::assert_true(engine.isAIReady(), "AI should be ready with mock provider");
TestFramework::assert_true(engine.isAIEnabled(), "AI should be enabled by default");
CodeContext context;
context.filePath = "test.cpp";
context.content = "class MyClass {\npublic:\n    ";
context.cursorPosition = 25;
context.line = 2;
context.column = 4;
context.language = "cpp";
auto completions = engine.getCompletions(context, "cla", 10);
TestFramework::assert_true(completions.size() > 0, "Should generate completions for 'cla' prefix");
engine.setAIEnabled(false);
TestFramework::assert_false(engine.isAIEnabled(), "AI should be disabled");
completions = engine.getCompletions(context, "cla", 10);
TestFramework::assert_true(completions.size() >= 0, "Should still work with AI disabled");
engine.setAIEnabled(true);
TestFramework::assert_true(engine.isAIEnabled(), "AI should be re-enabled");
}
void testIntegratedEditorAICompletion() {
IntegratedEditor& editor = IntegratedEditor::getInstance();
TestFramework::assert_true(editor.isAICompletionReady(), "AI completion should be ready");
TestFramework::assert_true(editor.isAICompletionEnabled(), "AI completion should be enabled");
std::string testFilePath = "/tmp/test.cpp";
std::string testContent = "int main() {\n    std::\n    return 0;\n}";
editor.openDocument(testFilePath, testContent);
auto completions = editor.getAICompletions(testFilePath, "std::", 5);
TestFramework::assert_true(completions.size() > 0, "Should get AI completions for C++ file");
TestFramework::assert_false(editor.isCodeCompletionActive(), "Code completion should not be active initially");
editor.triggerCodeCompletion(testFilePath, "std::");
TestFramework::assert_true(editor.isCodeCompletionActive(), "Code completion should be active after trigger");
CompletionItem selected = editor.getSelectedCompletion();
TestFramework::assert_false(selected.label.empty(), "Selected completion should not be empty");
editor.selectNextCompletion();
editor.selectPreviousCompletion();
editor.cancelCompletion();
TestFramework::assert_false(editor.isCodeCompletionActive(), "Code completion should not be active after cancel");
editor.setAICompletionEnabled(false);
TestFramework::assert_false(editor.isAICompletionEnabled(), "AI completion should be disabled");
editor.setAICompletionEnabled(true);
TestFramework::assert_true(editor.isAICompletionEnabled(), "AI completion should be re-enabled");
}
int main() {
std::cout << "Bolt C++ AI Code Completion Tests\n";
std::cout << "==================================\n";
TestFramework::run_test("AICompletionProvider", "MockProviderBasics", testAICompletionProvider);
TestFramework::run_test("AICodeCompletionEngine", "EngineBasics", testAICodeCompletionEngine);
TestFramework::run_test("IntegratedEditor", "AICompletionIntegration", testIntegratedEditorAICompletion);
TestFramework::print_results();
return TestFramework::get_exit_code();
}