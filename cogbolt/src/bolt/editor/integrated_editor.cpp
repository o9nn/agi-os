#include "bolt/editor/integrated_editor.hpp"
#include "bolt/editor/cursor_manager.hpp"
#include "bolt/editor/keyboard_shortcuts.hpp"
#include "bolt/editor/split_view_manager.hpp"
#include "bolt/ai/ai_completion_provider.hpp"
#include <fstream>
namespace bolt {
IntegratedEditor::IntegratedEditor()
: editorStore_(EditorStore::getInstance())
, foldingManager_(CodeFoldingManager::getInstance())
, foldingUI_(std::make_unique<CodeFoldingUI>())
, fileTreeManager_(FileTreeManager::getInstance())
, fileTreeUI_(std::make_unique<FileTreeUI>())
, cursorManager_(CursorManager::getInstance())
, keyboardShortcuts_(KeyboardShortcuts::getInstance())
, splitViewManager_(SplitViewManager::getInstance())
, tabBar_(TabBar::getInstance())
, aiCompletionEngine_(AICodeCompletionEngine::getInstance())
, codeCompletion_(std::make_unique<CodeCompletion>())
, debugger_(std::make_shared<DebuggerInterface>())
, debuggerUI_(std::make_unique<DebuggerUI>()) {
foldingManager_.setOnFoldingChanged([this]() {
auto currentFile = editorStore_.getSelectedFile();
if (!currentFile.empty()) {
synchronizeFoldingState(currentFile);
}
});
fileTreeManager_.setOnFileSelected([this](const std::string& path) {
openFileFromTree(path);
});
initializeKeyboardShortcuts();
splitViewManager_.setOnPaneOperation([this](SplitViewManager::PaneOperation op, const std::string& paneId) {
if (op == SplitViewManager::PaneOperation::Focus) {
EditorPane* pane = splitViewManager_.getPane(paneId);
if (pane && pane->hasDocument()) {
editorStore_.setSelectedFile(pane->getDocumentPath());
}
}
});
debugger_->set_editor(this);
debuggerUI_->set_debugger(debugger_);
debuggerUI_->initialize();
}
void IntegratedEditor::openDocument(const std::string& filePath, const std::string& content) {
EditorDocument doc;
doc.value = content;
doc.filePath = filePath;
doc.scroll = {0, 0};
doc.cursor = {0, std::nullopt};
detectAndUpdateFolding(filePath, content);
doc.foldingRanges = getFoldingRanges(filePath);
editorStore_.setDocument(filePath, doc);
editorStore_.setSelectedFile(filePath);
if (splitViewManager_.isEnabled()) {
EditorPane* activePane = splitViewManager_.getActivePane();
if (activePane) {
activePane->openDocument(filePath);
}
}
}
void IntegratedEditor::updateDocumentContent(const std::string& filePath, const std::string& content) {
auto* doc = editorStore_.getCurrentDocument();
if (doc && doc->filePath == filePath) {
doc->value = content;
}
detectAndUpdateFolding(filePath, content);
synchronizeFoldingState(filePath);
}
void IntegratedEditor::closeDocument(const std::string& filePath) {
foldingManager_.updateFoldingRanges(filePath, "");
}
void IntegratedEditor::toggleFold(const std::string& filePath, size_t line) {
foldingManager_.toggleFold(filePath, line);
editorStore_.toggleFold(filePath, line);
}
void IntegratedEditor::expandAllFolds(const std::string& filePath) {
auto ranges = foldingManager_.getFoldingRanges(filePath);
for (auto& range : ranges) {
if (range.isFolded) {
toggleFold(filePath, range.startLine);
}
}
}
void IntegratedEditor::collapseAllFolds(const std::string& filePath) {
auto ranges = foldingManager_.getFoldingRanges(filePath);
for (auto& range : ranges) {
if (!range.isFolded) {
toggleFold(filePath, range.startLine);
}
}
}
void IntegratedEditor::refreshFolding(const std::string& filePath) {
auto* doc = editorStore_.getCurrentDocument();
if (doc && doc->filePath == filePath) {
detectAndUpdateFolding(filePath, doc->value);
synchronizeFoldingState(filePath);
}
}
void IntegratedEditor::renderEditor(const std::string& filePath) {
if (foldingUI_->isVisible()) {
foldingUI_->renderFoldingMarkers(filePath);
}
if (fileTreeUI_->isVisible()) {
renderFileTree();
}
}
void IntegratedEditor::handleClick(const std::string& filePath, size_t line, size_t character) {
auto ranges = getFoldingRanges(filePath);
for (const auto& range : ranges) {
if (line == range.startLine) {
toggleFold(filePath, line);
return;
}
}
}
std::vector<FoldRange> IntegratedEditor::getFoldingRanges(const std::string& filePath) const {
return foldingManager_.getFoldingRanges(filePath);
}
bool IntegratedEditor::isFoldingEnabled() const {
return foldingManager_.isFoldingEnabled();
}
void IntegratedEditor::setFoldingEnabled(bool enabled) {
foldingManager_.setFoldingEnabled(enabled);
foldingUI_->setVisibility(enabled);
}
void IntegratedEditor::detectAndUpdateFolding(const std::string& filePath, const std::string& content) {
foldingManager_.updateFoldingRanges(filePath, content);
}
void IntegratedEditor::synchronizeFoldingState(const std::string& filePath) {
auto ranges = foldingManager_.getFoldingRanges(filePath);
editorStore_.updateFoldingRanges(filePath, ranges);
}
void IntegratedEditor::setFileTreeRootDirectory(const std::string& path) {
fileTreeManager_.setRootDirectory(path);
}
void IntegratedEditor::refreshFileTree() {
fileTreeManager_.refreshTree();
}
void IntegratedEditor::toggleFileTreeVisibility() {
bool currentVisibility = fileTreeUI_->isVisible();
fileTreeUI_->setVisible(!currentVisibility);
}
bool IntegratedEditor::isFileTreeVisible() const {
return fileTreeUI_->isVisible();
}
void IntegratedEditor::renderFileTree() {
if (fileTreeUI_->isVisible()) {
fileTreeUI_->render();
}
}
void IntegratedEditor::openFileFromTree(const std::string& filePath) {
try {
std::ifstream file(filePath);
if (!file.is_open()) {
return;
}
std::string content((std::istreambuf_iterator<char>(file)),
std::istreambuf_iterator<char>());
file.close();
openDocument(filePath, content);
} catch (const std::exception&) {
}
}
void IntegratedEditor::showFileInTree(const std::string& filePath) {
fileTreeManager_.selectFile(filePath);
}
void IntegratedEditor::addCursorAtPosition(size_t line, size_t column) {
cursorManager_.addCursor(line, column);
}
void IntegratedEditor::addCursorAtNextOccurrence(const std::string& text) {
auto* doc = editorStore_.getCurrentDocument();
if (doc) {
cursorManager_.addCursorAtNextOccurrence(text, doc->value);
}
}
void IntegratedEditor::selectAllOccurrences(const std::string& text) {
auto* doc = editorStore_.getCurrentDocument();
if (doc) {
cursorManager_.selectAllOccurrences(text, doc->value);
}
}
void IntegratedEditor::clearExtraCursors() {
auto primaryCursor = cursorManager_.getPrimaryCursor();
cursorManager_.clearCursors();
cursorManager_.addCursor(primaryCursor.line, primaryCursor.column);
}
void IntegratedEditor::insertTextAtCursors(const std::string& text) {
cursorManager_.insertTextAtCursors(text);
}
void IntegratedEditor::deleteAtCursors() {
cursorManager_.deleteAtCursors();
}
size_t IntegratedEditor::getCursorCount() const {
return cursorManager_.getCursorCount();
}
std::vector<Cursor> IntegratedEditor::getAllCursors() const {
return cursorManager_.getCursors();
}
bool IntegratedEditor::handleKeyboardShortcut(const std::string& key, const std::string& command) {
return keyboardShortcuts_.executeShortcut(key, ShortcutContext::Editor);
}
void IntegratedEditor::initializeKeyboardShortcuts() {
keyboardShortcuts_.setActiveContext(ShortcutContext::Editor);
keyboardShortcuts_.registerShortcut("Ctrl+D", "addCursorAtNextOccurrence", [this]() {
auto cursors = getAllCursors();
if (!cursors.empty()) {
auto& lastCursor = cursors.back();
addCursorAtPosition(lastCursor.line + 1, lastCursor.column);
}
}, ShortcutContext::Editor, "Add cursor at next occurrence of selected text");
keyboardShortcuts_.registerShortcut("Ctrl+Shift+L", "selectAllOccurrences", [this]() {
selectAllOccurrences("");
}, ShortcutContext::Editor, "Select all occurrences of selected text");
keyboardShortcuts_.registerShortcut("Ctrl+Alt+Up", "addCursorAbove", [this]() {
auto cursors = getAllCursors();
if (!cursors.empty()) {
auto& primary = cursors[0];
if (primary.line > 0) {
addCursorAtPosition(primary.line - 1, primary.column);
}
}
}, ShortcutContext::Editor, "Add cursor above current line");
keyboardShortcuts_.registerShortcut("Ctrl+Alt+Down", "addCursorBelow", [this]() {
auto cursors = getAllCursors();
if (!cursors.empty()) {
auto& primary = cursors[0];
addCursorAtPosition(primary.line + 1, primary.column);
}
}, ShortcutContext::Editor, "Add cursor below current line");
keyboardShortcuts_.registerShortcut("Escape", "clearExtraCursors", [this]() {
clearExtraCursors();
}, ShortcutContext::Editor, "Clear all extra cursors");
keyboardShortcuts_.initDefaultShortcuts();
keyboardShortcuts_.registerShortcut("Ctrl+\\", "createHorizontalSplit", [this]() {
createHorizontalSplit();
}, ShortcutContext::Global, "Create horizontal split");
keyboardShortcuts_.registerShortcut("Ctrl+Shift+\\", "createVerticalSplit", [this]() {
createVerticalSplit();
}, ShortcutContext::Global, "Create vertical split");
keyboardShortcuts_.registerShortcut("Ctrl+W", "closePane", [this]() {
std::string activeId = splitViewManager_.getActivePaneId();
closePane(activeId);
}, ShortcutContext::Global, "Close current pane");
keyboardShortcuts_.registerShortcut("Ctrl+Tab", "navigateToNextPane", [this]() {
navigateToNextPane();
}, ShortcutContext::Global, "Navigate to next pane");
keyboardShortcuts_.registerShortcut("Ctrl+Shift+Tab", "navigateToPreviousPane", [this]() {
navigateToPreviousPane();
}, ShortcutContext::Global, "Navigate to previous pane");
keyboardShortcuts_.registerShortcut("Ctrl+Space", "triggerCodeCompletion", [this]() {
auto currentFile = editorStore_.getSelectedFile();
if (!currentFile.empty()) {
auto* doc = editorStore_.getCurrentDocument();
if (doc) {
size_t line = 0, column = 0;
for (size_t i = 0; i < doc->cursor.position && i < doc->value.length(); ++i) {
if (doc->value[i] == '\n') {
line++;
column = 0;
} else {
column++;
}
}
triggerCodeCompletion(currentFile, line, column);
}
}
}, ShortcutContext::Editor, "Trigger AI code completion");
keyboardShortcuts_.registerShortcut("Enter", "acceptCompletion", [this]() {
if (isCodeCompletionActive()) {
acceptCompletion();
}
}, ShortcutContext::Editor, "Accept code completion");
keyboardShortcuts_.registerShortcut("Escape", "cancelCompletion", [this]() {
if (isCodeCompletionActive()) {
cancelCompletion();
}
}, ShortcutContext::Editor, "Cancel code completion");
keyboardShortcuts_.registerShortcut("Down", "selectNextCompletion", [this]() {
if (isCodeCompletionActive()) {
selectNextCompletion();
}
}, ShortcutContext::Editor, "Select next completion");
keyboardShortcuts_.registerShortcut("Up", "selectPreviousCompletion", [this]() {
if (isCodeCompletionActive()) {
selectPreviousCompletion();
}
}, ShortcutContext::Editor, "Select previous completion");
keyboardShortcuts_.registerShortcut("Ctrl+PageDown", "nextTab", [this]() {
switchToNextTab();
}, ShortcutContext::Global, "Switch to next tab");
keyboardShortcuts_.registerShortcut("Ctrl+PageUp", "previousTab", [this]() {
switchToPreviousTab();
}, ShortcutContext::Global, "Switch to previous tab");
keyboardShortcuts_.registerShortcut("Ctrl+W", "closeCurrentTab", [this]() {
auto activeTab = getActiveTab();
if (activeTab.has_value()) {
closeTabById(activeTab->id);
}
}, ShortcutContext::Global, "Close current tab");
}
std::string IntegratedEditor::createHorizontalSplit(const std::string& filePath) {
std::string newPaneId = splitViewManager_.createHorizontalSplit();
if (!newPaneId.empty() && !filePath.empty()) {
openDocumentInPane(newPaneId, filePath);
}
return newPaneId;
}
std::string IntegratedEditor::createVerticalSplit(const std::string& filePath) {
std::string newPaneId = splitViewManager_.createVerticalSplit();
if (!newPaneId.empty() && !filePath.empty()) {
openDocumentInPane(newPaneId, filePath);
}
return newPaneId;
}
bool IntegratedEditor::closePane(const std::string& paneId) {
return splitViewManager_.closePane(paneId);
}
bool IntegratedEditor::focusPane(const std::string& paneId) {
return splitViewManager_.focusPane(paneId);
}
void IntegratedEditor::navigateToNextPane() {
splitViewManager_.navigateToNextPane();
}
void IntegratedEditor::navigateToPreviousPane() {
splitViewManager_.navigateToPreviousPane();
}
bool IntegratedEditor::isSplitViewEnabled() const {
return splitViewManager_.isEnabled();
}
void IntegratedEditor::setSplitViewEnabled(bool enabled) {
splitViewManager_.setEnabled(enabled);
}
bool IntegratedEditor::hasSplits() const {
return splitViewManager_.hasSplits();
}
void IntegratedEditor::collapseAllSplits() {
splitViewManager_.collapseAllSplits();
}
std::vector<std::string> IntegratedEditor::getAllPaneIds() const {
return splitViewManager_.getAllPaneIds();
}
std::string IntegratedEditor::getActivePaneId() const {
return splitViewManager_.getActivePaneId();
}
EditorPane* IntegratedEditor::getActivePane() {
return splitViewManager_.getActivePane();
}
void IntegratedEditor::openDocumentInPane(const std::string& paneId, const std::string& filePath) {
splitViewManager_.openDocumentInPane(paneId, filePath);
}
void IntegratedEditor::openDocumentInNewPane(const std::string& filePath, SplitViewManager::SplitDirection direction) {
splitViewManager_.openDocumentInNewPane(filePath, direction);
}
std::vector<std::string> IntegratedEditor::getOpenDocuments() const {
return splitViewManager_.getOpenDocuments();
}
std::string IntegratedEditor::findPaneWithDocument(const std::string& filePath) const {
return splitViewManager_.findPaneWithDocument(filePath);
}
void IntegratedEditor::triggerCodeCompletion(const std::string& filePath, size_t line, size_t column) {
auto* doc = editorStore_.getCurrentDocument();
if (!doc || doc->filePath != filePath) {
return;
}
size_t cursorPosition = 0;
std::string content = doc->value;
size_t currentLine = 0;
for (size_t i = 0; i < content.length() && currentLine < line; ++i) {
if (content[i] == '\n') {
currentLine++;
if (currentLine == line) {
cursorPosition = i + 1 + column;
break;
}
}
}
std::string prefix;
if (cursorPosition > 0) {
size_t start = cursorPosition;
while (start > 0 && (std::isalnum(content[start - 1]) || content[start - 1] == '_' || content[start - 1] == ':')) {
start--;
}
prefix = content.substr(start, cursorPosition - start);
}
triggerCodeCompletion(filePath, prefix);
}
void IntegratedEditor::triggerCodeCompletion(const std::string& filePath, const std::string& prefix) {
auto* doc = editorStore_.getCurrentDocument();
if (!doc || doc->filePath != filePath) {
return;
}
auto completions = getAICompletions(filePath, prefix);
if (!completions.empty()) {
codeCompletion_->setSuggestions(completions);
codeCompletion_->activate();
}
}
std::vector<CompletionItem> IntegratedEditor::getAICompletions(const std::string& filePath, const std::string& prefix, size_t limit) {
auto* doc = editorStore_.getCurrentDocument();
if (!doc || doc->filePath != filePath) {
return {};
}
std::string language = "text";
if (filePath.length() >= 4) {
std::string ext = filePath.substr(filePath.length() - 4);
if (ext == ".cpp" || ext == ".hpp") {
language = "cpp";
}
}
if (filePath.length() >= 2 && filePath.substr(filePath.length() - 2) == ".h") {
language = "cpp";
}
if (filePath.length() >= 3 && filePath.substr(filePath.length() - 3) == ".py") {
language = "python";
}
if (filePath.length() >= 3) {
std::string ext = filePath.substr(filePath.length() - 3);
if (ext == ".js" || ext == ".ts") {
language = "javascript";
}
}
size_t line = 0, column = 0;
for (size_t i = 0; i < doc->cursor.position && i < doc->value.length(); ++i) {
if (doc->value[i] == '\n') {
line++;
column = 0;
} else {
column++;
}
}
CodeContext context;
context.filePath = filePath;
context.content = doc->value;
context.cursorPosition = doc->cursor.position;
context.line = line;
context.column = column;
context.language = language;
return aiCompletionEngine_.getCompletions(context, prefix, limit);
}
bool IntegratedEditor::isCodeCompletionActive() const {
return codeCompletion_->isActive();
}
void IntegratedEditor::acceptCompletion() {
if (codeCompletion_->isActive()) {
auto selectedCompletion = codeCompletion_->getSelectedSuggestion();
auto* doc = editorStore_.getCurrentDocument();
if (!doc || selectedCompletion.label.empty()) {
codeCompletion_->deactivate();
return;
}
size_t cursorPos = doc->cursor.position;
std::string content = doc->value;
if (cursorPos > content.length()) {
cursorPos = content.length();
}
size_t prefixStart = cursorPos;
while (prefixStart > 0 && prefixStart <= content.length() &&
(std::isalnum(content[prefixStart - 1]) ||
content[prefixStart - 1] == '_' ||
content[prefixStart - 1] == ':')) {
prefixStart--;
}
std::string insertText;
if (selectedCompletion.kind == "snippet") {
insertText = selectedCompletion.detail;
} else {
insertText = selectedCompletion.label;
}
std::string newContent = content.substr(0, prefixStart) +
insertText +
content.substr(cursorPos);
doc->value = newContent;
size_t newCursorPos = prefixStart + insertText.length();
doc->cursor.position = newCursorPos;
editorStore_.setDocument(doc->filePath, *doc);
codeCompletion_->deactivate();
}
}
void IntegratedEditor::cancelCompletion() {
codeCompletion_->deactivate();
}
void IntegratedEditor::selectNextCompletion() {
if (codeCompletion_->isActive()) {
codeCompletion_->selectNext();
}
}
void IntegratedEditor::selectPreviousCompletion() {
if (codeCompletion_->isActive()) {
codeCompletion_->selectPrevious();
}
}
CompletionItem IntegratedEditor::getSelectedCompletion() const {
return codeCompletion_->getSelectedSuggestion();
}
void IntegratedEditor::setAICompletionEnabled(bool enabled) {
aiCompletionEngine_.setAIEnabled(enabled);
}
bool IntegratedEditor::isAICompletionEnabled() const {
return aiCompletionEngine_.isAIEnabled();
}
bool IntegratedEditor::isAICompletionReady() const {
return aiCompletionEngine_.isAIReady();
}
bool IntegratedEditor::startDebugSession(const std::string& filePath) {
try {
std::ifstream file(filePath);
if (!file.is_open()) {
return false;
}
std::string content((std::istreambuf_iterator<char>(file)),
std::istreambuf_iterator<char>());
file.close();
return debugger_->start_debug_session_from_source(content);
} catch (const std::exception&) {
return false;
}
}
bool IntegratedEditor::startDebugSessionFromSource(const std::string& limboSource) {
return debugger_->start_debug_session_from_source(limboSource);
}
void IntegratedEditor::stopDebugSession() {
debugger_->stop_debug_session();
}
bool IntegratedEditor::isDebugging() const {
return debugger_->is_debugging();
}
DebugState IntegratedEditor::getDebugState() const {
return debugger_->get_debug_state();
}
void IntegratedEditor::debugContinue() {
debugger_->continue_execution();
}
void IntegratedEditor::debugStepOver() {
debugger_->step_over();
}
void IntegratedEditor::debugStepInto() {
debugger_->step_into();
}
void IntegratedEditor::debugStepOut() {
debugger_->step_out();
}
void IntegratedEditor::debugPause() {
debugger_->pause_execution();
}
bool IntegratedEditor::toggleBreakpointAtLine(const std::string& filePath, size_t line) {
return debugger_->set_breakpoint_at_line(filePath, line);
}
bool IntegratedEditor::setBreakpointAtLine(const std::string& filePath, size_t line) {
return debugger_->set_breakpoint_at_line(filePath, line);
}
bool IntegratedEditor::removeBreakpointAtLine(const std::string& filePath, size_t line) {
return debugger_->remove_breakpoint_at_line(filePath, line);
}
void IntegratedEditor::clearAllBreakpoints() {
debugger_->clear_all_breakpoints();
}
std::vector<BreakpointInfo> IntegratedEditor::getAllBreakpoints() const {
return debugger_->get_all_breakpoints();
}
void IntegratedEditor::showDebugger() {
debuggerUI_->set_visible(true);
}
void IntegratedEditor::hideDebugger() {
debuggerUI_->set_visible(false);
}
bool IntegratedEditor::isDebuggerVisible() const {
return debuggerUI_->is_visible();
}
void IntegratedEditor::toggleDebuggerVisibility() {
debuggerUI_->set_visible(!debuggerUI_->is_visible());
}
size_t IntegratedEditor::getCurrentDebugPC() const {
return debugger_->get_current_pc();
}
std::string IntegratedEditor::getCurrentDebugInstruction() const {
return debugger_->get_current_instruction();
}
std::vector<size_t> IntegratedEditor::getDebugCallStack() const {
return debugger_->get_call_stack();
}
std::vector<std::string> IntegratedEditor::getDebugStackContents() const {
return debugger_->get_stack_contents();
}
std::map<std::string, std::string> IntegratedEditor::getDebugGlobalVariables() const {
return debugger_->get_global_variables();
}
size_t IntegratedEditor::openDocumentInTab(const std::string& filePath, const std::string& content) {
size_t tabId = tabBar_.addTab(filePath);
openDocument(filePath, content);
return tabId;
}
bool IntegratedEditor::closeTabById(size_t tabId) {
auto tab = tabBar_.getTab(tabId);
if (!tab.has_value()) {
return false;
}
closeDocument(tab->filePath);
return tabBar_.closeTab(tabId);
}
bool IntegratedEditor::closeTabByPath(const std::string& filePath) {
closeDocument(filePath);
return tabBar_.closeTabByPath(filePath);
}
void IntegratedEditor::closeAllTabs() {
auto tabs = tabBar_.getAllTabs();
for (const auto& tab : tabs) {
if (!tab.isPinned) {
closeDocument(tab.filePath);
}
}
tabBar_.closeAllTabs();
}
void IntegratedEditor::closeOtherTabs(size_t exceptTabId) {
auto tabs = tabBar_.getAllTabs();
for (const auto& tab : tabs) {
if (tab.id != exceptTabId && !tab.isPinned) {
closeDocument(tab.filePath);
}
}
tabBar_.closeOtherTabs(exceptTabId);
}
bool IntegratedEditor::switchToTab(size_t tabId) {
auto tab = tabBar_.getTab(tabId);
if (!tab.has_value()) {
return false;
}
editorStore_.setSelectedFile(tab->filePath);
return tabBar_.activateTab(tabId);
}
bool IntegratedEditor::switchToTabByPath(const std::string& filePath) {
editorStore_.setSelectedFile(filePath);
return tabBar_.activateTabByPath(filePath);
}
void IntegratedEditor::switchToNextTab() {
tabBar_.activateNextTab();
auto activeTab = tabBar_.getActiveTab();
if (activeTab.has_value()) {
editorStore_.setSelectedFile(activeTab->filePath);
}
}
void IntegratedEditor::switchToPreviousTab() {
tabBar_.activatePreviousTab();
auto activeTab = tabBar_.getActiveTab();
if (activeTab.has_value()) {
editorStore_.setSelectedFile(activeTab->filePath);
}
}
std::optional<EditorTab> IntegratedEditor::getActiveTab() const {
return tabBar_.getActiveTab();
}
std::vector<EditorTab> IntegratedEditor::getAllTabs() const {
return tabBar_.getAllTabs();
}
size_t IntegratedEditor::getTabCount() const {
return tabBar_.getTabCount();
}
bool IntegratedEditor::hasOpenTabs() const {
return tabBar_.hasTabs();
}
void IntegratedEditor::setTabDirty(size_t tabId, bool isDirty) {
tabBar_.setTabDirty(tabId, isDirty);
}
void IntegratedEditor::setTabPinned(size_t tabId, bool isPinned) {
tabBar_.setTabPinned(tabId, isPinned);
}
}