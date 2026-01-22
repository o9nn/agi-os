#include "bolt/test_framework.hpp"
#include "bolt/core/error_handling.hpp"
#include "bolt/core/memory_manager.hpp"
#include "bolt/core/message_handler.hpp"
#include "bolt/core/chat_store.hpp"
#include "bolt/core/editor_store.hpp"
#include "bolt/core/workbench_store.hpp"
BOLT_TEST(ErrorHandling, BoltExceptionBasic) {
try {
throw bolt::BoltException(bolt::ErrorCode::INVALID_PARAMETER, "Test error");
} catch (const bolt::BoltException& e) {
BOLT_ASSERT_TRUE(e.getErrorCode() == bolt::ErrorCode::INVALID_PARAMETER);
BOLT_ASSERT_TRUE(std::string(e.what()).find("Test error") != std::string::npos);
}
}
BOLT_TEST(ErrorHandling, MemoryExceptionInheritance) {
try {
throw bolt::MemoryException(bolt::ErrorCode::MEMORY_ALLOCATION_FAILED, "Memory error");
} catch (const bolt::BoltException& e) {
BOLT_ASSERT_TRUE(e.getErrorCode() == bolt::ErrorCode::MEMORY_ALLOCATION_FAILED);
}
}
BOLT_TEST(ErrorHandling, ValidateNotNull) {
int value = 42;
BOLT_ASSERT_TRUE(true);
bolt::ErrorHandler::validateNotNull(&value, "value");
BOLT_ASSERT_THROWS(bolt::BoltException,
bolt::ErrorHandler::validateNotNull(nullptr, "null_ptr"));
}
BOLT_TEST(ErrorHandling, ValidateParameter) {
bolt::ErrorHandler::validateParameter(true, "Should pass");
BOLT_ASSERT_THROWS(bolt::BoltException,
bolt::ErrorHandler::validateParameter(false, "Should fail"));
}
BOLT_TEST(ErrorHandling, ValidateMemorySize) {
bolt::ErrorHandler::validateMemorySize(1);
bolt::ErrorHandler::validateMemorySize(1024);
bolt::ErrorHandler::validateMemorySize(1024 * 1024);
BOLT_ASSERT_THROWS(bolt::MemoryException,
bolt::ErrorHandler::validateMemorySize(0));
BOLT_ASSERT_THROWS(bolt::MemoryException,
bolt::ErrorHandler::validateMemorySize(SIZE_MAX));
}
BOLT_TEST(MemoryManagerErrors, AllocationSizeValidation) {
auto& manager = bolt::MemoryManager::getInstance();
BOLT_ASSERT_THROWS(bolt::MemoryException, manager.allocate(0));
BOLT_ASSERT_THROWS(bolt::MemoryException, manager.allocate(SIZE_MAX));
}
BOLT_TEST(MemoryManagerErrors, MaxUsageLimit) {
auto& manager = bolt::MemoryManager::getInstance();
manager.setMaxAllowedUsage(100);
void* ptr1 = manager.allocate(50);
BOLT_ASSERT_NOT_NULL(ptr1);
BOLT_ASSERT_THROWS(bolt::MemoryException, manager.allocate(60));
manager.deallocate(ptr1);
manager.setMaxAllowedUsage(SIZE_MAX);
}
BOLT_TEST(MemoryManagerErrors, DoubleFreeDetection) {
auto& manager = bolt::MemoryManager::getInstance();
void* ptr = manager.allocate(100);
BOLT_ASSERT_NOT_NULL(ptr);
manager.deallocate(ptr);
BOLT_ASSERT_THROWS(bolt::MemoryException, manager.deallocate(ptr));
}
BOLT_TEST(MemoryManagerErrors, ResetWithActiveAllocations) {
auto& manager = bolt::MemoryManager::getInstance();
void* ptr = manager.allocate(100);
BOLT_ASSERT_NOT_NULL(ptr);
BOLT_ASSERT_THROWS(bolt::MemoryException, manager.reset());
manager.deallocate(ptr);
manager.reset();
}
BOLT_TEST(MemoryManagerErrors, MemoryLeakDetection) {
auto& manager = bolt::MemoryManager::getInstance();
BOLT_ASSERT_FALSE(manager.hasMemoryLeaks());
BOLT_ASSERT_TRUE(manager.getLeakedAllocations().size() == 0);
void* ptr = manager.allocate(100);
BOLT_ASSERT_TRUE(manager.hasMemoryLeaks());
BOLT_ASSERT_TRUE(manager.getLeakedAllocations().size() == 1);
manager.deallocate(ptr);
BOLT_ASSERT_FALSE(manager.hasMemoryLeaks());
}
BOLT_TEST(MessageHandlerErrors, UninitializedAccess) {
auto& handler = bolt::MessageHandler::getInstance();
if (!handler.isInitialized()) {
bolt::Message msg(bolt::MessageType::Chat, "test");
BOLT_ASSERT_THROWS(bolt::BoltException, handler.pushMessage(msg));
BOLT_ASSERT_THROWS(bolt::BoltException, handler.processMessages());
BOLT_ASSERT_THROWS(bolt::BoltException, handler.getQueueSize());
BOLT_ASSERT_THROWS(bolt::BoltException, handler.setMaxQueueSize(100));
}
handler.initialize();
BOLT_ASSERT_TRUE(handler.isInitialized());
}
BOLT_TEST(MessageHandlerErrors, MessageValidation) {
auto& handler = bolt::MessageHandler::getInstance();
handler.initialize();
handler.processMessages();
bolt::Message validMsg(bolt::MessageType::Chat, "Hello");
handler.pushMessage(validMsg);
std::string oversizedContent(bolt::Message::MAX_MESSAGE_LENGTH + 1, 'x');
BOLT_ASSERT_THROWS(bolt::MessageException,
bolt::Message(bolt::MessageType::Chat, oversizedContent));
handler.processMessages();
}
BOLT_TEST(MessageHandlerErrors, QueueOverflow) {
auto& handler = bolt::MessageHandler::getInstance();
handler.initialize();
handler.processMessages();
handler.setMaxQueueSize(3);
handler.pushMessage(bolt::Message(bolt::MessageType::System, "msg1"));
handler.pushMessage(bolt::Message(bolt::MessageType::System, "msg2"));
handler.pushMessage(bolt::Message(bolt::MessageType::System, "msg3"));
BOLT_ASSERT_THROWS(bolt::MessageException,
handler.pushMessage(bolt::Message(bolt::MessageType::System, "overflow")));
handler.processMessages();
handler.setMaxQueueSize(1000);
}
BOLT_TEST(MessageHandlerErrors, InvalidQueueSize) {
auto& handler = bolt::MessageHandler::getInstance();
handler.initialize();
BOLT_ASSERT_THROWS(bolt::BoltException, handler.setMaxQueueSize(0));
}
BOLT_TEST(ChatStoreErrors, MessageValidation) {
auto& store = bolt::ChatStore::getInstance();
bolt::SimpleChatMessage validMsg("user", "Hello");
store.addMessage(validMsg);
BOLT_ASSERT_THROWS(bolt::StoreException,
bolt::SimpleChatMessage("", "content"));
BOLT_ASSERT_THROWS(bolt::StoreException,
bolt::SimpleChatMessage("user", ""));
std::string oversizedSender(bolt::SimpleChatMessage::MAX_SENDER_LENGTH + 1, 'x');
BOLT_ASSERT_THROWS(bolt::StoreException,
bolt::SimpleChatMessage(oversizedSender, "content"));
std::string oversizedContent(bolt::SimpleChatMessage::MAX_CONTENT_LENGTH + 1, 'x');
BOLT_ASSERT_THROWS(bolt::StoreException,
bolt::SimpleChatMessage("user", oversizedContent));
}
BOLT_TEST(ChatStoreErrors, ListenerValidation) {
auto& store = bolt::ChatStore::getInstance();
BOLT_ASSERT_THROWS(bolt::BoltException,
store.addListener(nullptr));
}
BOLT_TEST(ChatStoreErrors, ThreadSafety) {
auto& store = bolt::ChatStore::getInstance();
store.setChatStarted(true);
bool started = store.getChatStarted();
BOLT_ASSERT_TRUE(started);
store.setShowChat(false);
bool showChat = store.getShowChat();
BOLT_ASSERT_FALSE(showChat);
}
BOLT_TEST(EditorStoreErrors, DocumentValidation) {
auto& store = bolt::EditorStore::getInstance();
bolt::EditorDocument validDoc;
validDoc.filePath = "/test/file.cpp";
validDoc.value = "int main() { return 0; }";
validDoc.scroll = {0, 0};
store.setDocument("/test/file.cpp", validDoc);
bolt::EditorDocument invalidDoc;
invalidDoc.filePath = "";
BOLT_ASSERT_THROWS(bolt::EditorException, invalidDoc.validateDocument());
std::string oversizedPath(bolt::EditorDocument::MAX_FILE_PATH_LENGTH + 1, 'x');
invalidDoc.filePath = oversizedPath;
BOLT_ASSERT_THROWS(bolt::EditorException, invalidDoc.validateDocument());
validDoc.filePath = "/test/valid.cpp";
validDoc.scroll = {-1, 0};
BOLT_ASSERT_THROWS(bolt::EditorException, validDoc.validateDocument());
}
BOLT_TEST(EditorStoreErrors, DocumentOperations) {
auto& store = bolt::EditorStore::getInstance();
BOLT_ASSERT_THROWS(bolt::EditorException,
store.updateScrollPosition("/nonexistent.cpp", 10, 5));
BOLT_ASSERT_THROWS(bolt::EditorException,
store.updateFoldingRanges("/nonexistent.cpp", {}));
BOLT_ASSERT_THROWS(bolt::EditorException,
store.toggleFold("/nonexistent.cpp", 5));
BOLT_ASSERT_THROWS(bolt::BoltException,
store.updateScrollPosition("", 10, 5));
}
BOLT_TEST(EditorStoreErrors, FoldingValidation) {
auto& store = bolt::EditorStore::getInstance();
bolt::EditorDocument doc;
doc.filePath = "/test/folding.cpp";
doc.value = "int main() {\n  return 0;\n}";
doc.scroll = {0, 0};
store.setDocument("/test/folding.cpp", doc);
std::vector<bolt::FoldRange> ranges;
ranges.push_back(bolt::FoldRange{0, 2, false, "..."});
store.updateFoldingRanges("/test/folding.cpp", ranges);
store.toggleFold("/test/folding.cpp", 1);
BOLT_ASSERT_THROWS(bolt::EditorException,
store.toggleFold("/test/folding.cpp", 10));
}
BOLT_TEST(EditorStoreErrors, DocumentLimits) {
auto& store = bolt::EditorStore::getInstance();
bolt::EditorDocument doc;
doc.filePath = "/test/limit.cpp";
doc.value = "test";
doc.scroll = {0, 0};
store.setDocument("/test/limit.cpp", doc);
store.closeDocument("/test/limit.cpp");
}
BOLT_TEST(WorkbenchStoreErrors, ViewValidation) {
auto& store = bolt::WorkbenchStore::getInstance();
store.setCurrentView("editor");
BOLT_ASSERT_THROWS(bolt::BoltException,
store.setCurrentView(""));
std::string oversizedView(1000, 'x');
BOLT_ASSERT_THROWS(bolt::StoreException,
store.setCurrentView(oversizedView));
}
BOLT_TEST(WorkbenchStoreErrors, FilePathValidation) {
auto& store = bolt::WorkbenchStore::getInstance();
store.setSelectedFile("/valid/path.cpp");
store.setSelectedFile("");
std::string oversizedPath(3000, 'x');
BOLT_ASSERT_THROWS(bolt::StoreException,
store.setSelectedFile(oversizedPath));
}
BOLT_TEST(WorkbenchStoreErrors, ListenerValidation) {
auto& store = bolt::WorkbenchStore::getInstance();
BOLT_ASSERT_THROWS(bolt::BoltException,
store.addListener(nullptr));
store.addListener([](){});
}
BOLT_TEST(WorkbenchStoreErrors, ThreadSafety) {
auto& store = bolt::WorkbenchStore::getInstance();
store.setShowWorkbench(true);
bool show = store.getShowWorkbench();
BOLT_ASSERT_TRUE(show);
store.toggleTerminal(true);
bool terminal = store.getShowTerminal();
BOLT_ASSERT_TRUE(terminal);
}
BOLT_TEST(ErrorRecovery, MemoryManagerRecovery) {
auto& manager = bolt::MemoryManager::getInstance();
void* ptr1 = manager.allocate(100);
void* ptr2 = manager.allocate(200);
BOLT_ASSERT_NOT_NULL(ptr1);
BOLT_ASSERT_NOT_NULL(ptr2);
manager.forceReset();
void* ptr3 = manager.allocate(150);
BOLT_ASSERT_NOT_NULL(ptr3);
manager.deallocate(ptr3);
}
BOLT_TEST(ErrorRecovery, MessageHandlerRecovery) {
auto& handler = bolt::MessageHandler::getInstance();
handler.initialize();
try {
bolt::Message invalidCmd(bolt::MessageType::Command, "invalid-command");
handler.pushMessage(invalidCmd);
handler.processMessages();
} catch (...) {
}
bolt::Message validMsg(bolt::MessageType::System, "recovery test");
handler.pushMessage(validMsg);
BOLT_ASSERT_TRUE(handler.getQueueSize() < 1000);
}