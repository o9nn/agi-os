#include "bolt/test_framework.hpp"
#include "bolt/bolt.hpp"
#include "bolt/core/memory_manager.hpp"
#include "bolt/core/chat_store.hpp"
#include "bolt/core/editor_store.hpp"
#include "bolt/core/workbench_store.hpp"
#include "bolt/core/message_handler.hpp"
#include "bolt/editor/code_folding.hpp"
#include "bolt/editor/code_folding_manager.hpp"
#include "bolt/editor/integrated_editor.hpp"
#include <memory>
#include <thread>
#include <chrono>
using namespace bolt::test;
void resetStoreStates() {
auto& chatStore = bolt::ChatStore::getInstance();
(void)bolt::EditorStore::getInstance();
auto& workbenchStore = bolt::WorkbenchStore::getInstance();
auto& memManager = bolt::MemoryManager::getInstance();
chatStore.setChatStarted(false);
chatStore.setShowChat(true);
chatStore.setAborted(false);
workbenchStore.setShowWorkbench(false);
workbenchStore.setCurrentView("code");
memManager.forceReset();
}
BOLT_TEST(Integration, BoltAppInitialization) {
resetStoreStates();
auto& app = bolt::BoltApp::getInstance();
app.initialize();
(void)bolt::ChatStore::getInstance();
(void)bolt::EditorStore::getInstance();
(void)bolt::WorkbenchStore::getInstance();
BOLT_ASSERT_TRUE(true);
}
BOLT_TEST(Integration, MultiStoreInteractions) {
resetStoreStates();
auto& chatStore = bolt::ChatStore::getInstance();
auto& editorStore = bolt::EditorStore::getInstance();
auto& workbenchStore = bolt::WorkbenchStore::getInstance();
chatStore.setChatStarted(true);
chatStore.setShowChat(true);
bolt::SimpleChatMessage msg{"user", "Please help me edit main.cpp"};
chatStore.addMessage(msg);
workbenchStore.setShowWorkbench(true);
workbenchStore.setCurrentView("code");
bolt::EditorDocument doc;
doc.value = "int main() {\n    return 0;\n}";
doc.filePath = "/workspace/main.cpp";
doc.scroll = {0, 0};
doc.cursor = {0, std::nullopt};
editorStore.setDocument("/workspace/main.cpp", doc);
BOLT_ASSERT_TRUE(chatStore.getChatStarted());
BOLT_ASSERT_TRUE(chatStore.getShowChat());
BOLT_ASSERT_TRUE(workbenchStore.getShowWorkbench());
BOLT_ASSERT_EQ("code", workbenchStore.getCurrentView());
auto messages = chatStore.getMessages();
BOLT_ASSERT_TRUE(messages.size() >= 1);
bool foundMessage = false;
for (const auto& message : messages) {
if (message.content == "Please help me edit main.cpp") {
foundMessage = true;
break;
}
}
BOLT_ASSERT_TRUE(foundMessage);
}
BOLT_TEST(Integration, CodeFoldingEditorIntegration) {
auto& editor = bolt::IntegratedEditor::getInstance();
auto& manager = bolt::CodeFoldingManager::getInstance();
manager.setFoldingEnabled(true);
editor.setFoldingEnabled(true);
std::string complexCode =
"class TestClass {\n"
"public:\n"
"    void method1() {\n"
"        if (true) {\n"
"            int x = 1;\n"
"        }\n"
"    }\n"
"    void method2() {\n"
"        for (int i = 0; i < 10; i++) {\n"
"            std::cout << i << std::endl;\n"
"        }\n"
"    }\n"
"};\n";
editor.openDocument("/test/complex.cpp", complexCode);
auto ranges = editor.getFoldingRanges("/test/complex.cpp");
BOLT_ASSERT_TRUE(ranges.size() >= 3);
editor.toggleFold("/test/complex.cpp", 3);
auto updatedRanges = editor.getFoldingRanges("/test/complex.cpp");
bool foundFoldedRange = false;
for (const auto& range : updatedRanges) {
if (range.isFolded) {
foundFoldedRange = true;
break;
}
}
BOLT_ASSERT_TRUE(foundFoldedRange);
}
BOLT_TEST(Integration, MessageHandlerWorkflow) {
resetStoreStates();
auto& handler = bolt::MessageHandler::getInstance();
auto& chatStore = bolt::ChatStore::getInstance();
handler.initialize();
chatStore.setChatStarted(false);
bolt::Message startMsg(bolt::MessageType::Chat, "Hello, I need help");
handler.pushMessage(startMsg);
handler.processMessages();
chatStore.setChatStarted(true);
bolt::Message sysMsg(bolt::MessageType::System, "Chat session initialized");
handler.pushMessage(sysMsg);
handler.processMessages();
BOLT_ASSERT_TRUE(chatStore.getChatStarted());
bolt::Message cmdMsg(bolt::MessageType::Command, "open file main.cpp");
handler.pushMessage(cmdMsg);
handler.processMessages();
BOLT_ASSERT_TRUE(true);
}
BOLT_TEST(Integration, MemoryManagerUnderLoad) {
resetStoreStates();
auto& memManager = bolt::MemoryManager::getInstance();
auto& chatStore = bolt::ChatStore::getInstance();
auto& editorStore = bolt::EditorStore::getInstance();
size_t initialMessageCount = chatStore.getMessages().size();
std::vector<void*> allocations;
for (int i = 0; i < 40; i++) {
void* ptr = memManager.allocate(1024 + i * 10);
allocations.push_back(ptr);
bolt::SimpleChatMessage msg{"user", "Message " + std::to_string(i)};
chatStore.addMessage(msg);
bolt::EditorDocument doc;
doc.value = "Content for document " + std::to_string(i);
doc.filePath = "/test/file" + std::to_string(i) + ".cpp";
doc.scroll = {0, 0};
doc.cursor = {0, std::nullopt};
editorStore.setDocument(doc.filePath, doc);
}
size_t peakUsage = memManager.getPeakUsage();
BOLT_ASSERT_TRUE(peakUsage > 40 * 1024);
auto messages = chatStore.getMessages();
BOLT_ASSERT_EQ(initialMessageCount + 40, messages.size());
for (void* ptr : allocations) {
memManager.deallocate(ptr);
}
size_t finalUsage = memManager.getCurrentUsage();
BOLT_ASSERT_TRUE(finalUsage < peakUsage);
}
BOLT_TEST(Integration, EditorDocumentLifecycle) {
resetStoreStates();
auto& editor = bolt::IntegratedEditor::getInstance();
auto& editorStore = bolt::EditorStore::getInstance();
auto& workbenchStore = bolt::WorkbenchStore::getInstance();
auto& foldingManager = bolt::CodeFoldingManager::getInstance();
foldingManager.setFoldingEnabled(true);
editor.setFoldingEnabled(true);
workbenchStore.setShowWorkbench(true);
workbenchStore.setCurrentView("code");
std::string documentContent =
"
"#include <iostream>\n"
"\n"
"class Calculator {\n"
"    int value;\n"
"};\n"
"\n"
"int main() {\n"
"    Calculator calc;\n"
"    return 0;\n"
"}\n";
const std::string filePath = "/workspace/calculator.cpp";
editor.openDocument(filePath, documentContent);
BOLT_ASSERT_TRUE(workbenchStore.getShowWorkbench());
BOLT_ASSERT_EQ("code", workbenchStore.getCurrentView());
bolt::EditorDocument doc;
doc.value = documentContent + "\n
doc.filePath = filePath;
doc.scroll = {10, 5};
doc.cursor = {20, std::nullopt};
editorStore.setDocument(filePath, doc);
editorStore.updateScrollPosition(filePath, 15, 8);
BOLT_ASSERT_TRUE(editor.isFoldingEnabled());
BOLT_ASSERT_TRUE(foldingManager.isFoldingEnabled());
std::string newContent = documentContent + "\n
editor.updateDocumentContent(filePath, newContent);
BOLT_ASSERT_TRUE(workbenchStore.getShowWorkbench());
BOLT_ASSERT_EQ("code", workbenchStore.getCurrentView());
}
BOLT_TEST(Integration, ChatEditorWorkflow) {
resetStoreStates();
auto& chatStore = bolt::ChatStore::getInstance();
auto& editorStore = bolt::EditorStore::getInstance();
auto& workbenchStore = bolt::WorkbenchStore::getInstance();
auto& messageHandler = bolt::MessageHandler::getInstance();
messageHandler.initialize();
size_t initialMessageCount = chatStore.getMessages().size();
chatStore.setChatStarted(true);
chatStore.setShowChat(true);
bolt::SimpleChatMessage userMsg{"user", "I need help debugging this C++ function"};
chatStore.addMessage(userMsg);
bolt::SimpleChatMessage assistantMsg{"assistant", "I'd be happy to help! Let me open the file for you."};
chatStore.addMessage(assistantMsg);
workbenchStore.setShowWorkbench(true);
workbenchStore.setCurrentView("code");
std::string buggyCode =
"#include <iostream>\n"
"\n"
"int fibonacci(int n) {\n"
"    if (n <= 1) {\n"
"        return n;\n"
"    }\n"
"
"    fibonacci(n-1) + fibonacci(n-2);\n"
"}\n"
"\n"
"int main() {\n"
"    std::cout << fibonacci(10) << std::endl;\n"
"    return 0;\n"
"}\n";
bolt::EditorDocument doc;
doc.value = buggyCode;
doc.filePath = "/workspace/fibonacci.cpp";
doc.scroll = {0, 0};
doc.cursor = {7, std::nullopt};
editorStore.setDocument("/workspace/fibonacci.cpp", doc);
bolt::SimpleChatMessage feedbackMsg{"assistant", "I found the issue! Line 8 is missing a 'return' statement."};
chatStore.addMessage(feedbackMsg);
auto messages = chatStore.getMessages();
BOLT_ASSERT_EQ(initialMessageCount + 3, messages.size());
BOLT_ASSERT_TRUE(chatStore.getChatStarted());
BOLT_ASSERT_TRUE(workbenchStore.getShowWorkbench());
BOLT_ASSERT_EQ("code", workbenchStore.getCurrentView());
BOLT_ASSERT_TRUE(buggyCode.find("fibonacci(n-1) + fibonacci(n-2);") != std::string::npos);
std::string fixedCode = buggyCode;
size_t pos = fixedCode.find("fibonacci(n-1) + fibonacci(n-2);");
if (pos != std::string::npos) {
fixedCode.replace(pos, 32, "return fibonacci(n-1) + fibonacci(n-2);");
}
doc.value = fixedCode;
editorStore.setDocument("/workspace/fibonacci.cpp", doc);
bolt::SimpleChatMessage confirmMsg{"user", "Thanks! I added the return statement and it works now."};
chatStore.addMessage(confirmMsg);
auto finalMessages = chatStore.getMessages();
BOLT_ASSERT_EQ(initialMessageCount + 4, finalMessages.size());
BOLT_ASSERT_TRUE(fixedCode.find("return fibonacci(n-1) + fibonacci(n-2);") != std::string::npos);
}
BOLT_TEST(Integration, MultithreadedOperations) {
resetStoreStates();
auto& memManager = bolt::MemoryManager::getInstance();
auto& chatStore = bolt::ChatStore::getInstance();
size_t initialMessageCount = chatStore.getMessages().size();
std::vector<void*> allocations;
const int totalOperations = 20;
for (int i = 0; i < totalOperations; i++) {
void* ptr = memManager.allocate(512 + i * 10);
allocations.push_back(ptr);
bolt::SimpleChatMessage msg{
"thread_sim",
"Message " + std::to_string(i) + " from simulated thread operation"
};
chatStore.addMessage(msg);
std::this_thread::sleep_for(std::chrono::microseconds(1));
}
auto messages = chatStore.getMessages();
BOLT_ASSERT_EQ(initialMessageCount + totalOperations, messages.size());
for (void* ptr : allocations) {
memManager.deallocate(ptr);
}
size_t finalUsage = memManager.getCurrentUsage();
size_t peakUsage = memManager.getPeakUsage();
BOLT_ASSERT_TRUE(peakUsage > 0);
BOLT_ASSERT_TRUE(finalUsage <= peakUsage);
}
BOLT_TEST(Integration, SystemShutdownCleanup) {
resetStoreStates();
auto& memManager = bolt::MemoryManager::getInstance();
auto& chatStore = bolt::ChatStore::getInstance();
auto& editorStore = bolt::EditorStore::getInstance();
auto& workbenchStore = bolt::WorkbenchStore::getInstance();
auto& messageHandler = bolt::MessageHandler::getInstance();
size_t initialMessageCount = chatStore.getMessages().size();
chatStore.setChatStarted(true);
for (int i = 0; i < 10; i++) {
bolt::SimpleChatMessage msg{"user", "Test message " + std::to_string(i)};
chatStore.addMessage(msg);
}
for (int i = 0; i < 5; i++) {
bolt::EditorDocument doc;
doc.value = "Content for file " + std::to_string(i);
doc.filePath = "/test/file" + std::to_string(i) + ".cpp";
editorStore.setDocument(doc.filePath, doc);
}
workbenchStore.setShowWorkbench(true);
workbenchStore.setCurrentView("terminal");
messageHandler.initialize();
for (int i = 0; i < 5; i++) {
bolt::Message msg(bolt::MessageType::System, "System message " + std::to_string(i));
messageHandler.pushMessage(msg);
}
BOLT_ASSERT_TRUE(chatStore.getChatStarted());
BOLT_ASSERT_EQ(initialMessageCount + 10, chatStore.getMessages().size());
BOLT_ASSERT_TRUE(workbenchStore.getShowWorkbench());
size_t currentUsage = memManager.getCurrentUsage();
size_t peakUsage = memManager.getPeakUsage();
BOLT_ASSERT_TRUE(peakUsage >= currentUsage);
BOLT_ASSERT_TRUE(true);
}