#ifndef INTEGRATED_EDITOR_HPP
#define INTEGRATED_EDITOR_HPP

#include "bolt/core/editor_store.hpp"
#include "bolt/editor/code_folding_manager.hpp"
#include "bolt/editor/code_folding_ui.hpp"
#include "bolt/editor/code_folding_detector.hpp"
#include "bolt/editor/file_tree.hpp"
#include "bolt/editor/cursor_manager.hpp"
#include "bolt/editor/keyboard_shortcuts.hpp"
#include "bolt/editor/split_view_manager.hpp"
#include "bolt/editor/code_completion.hpp"
#include "bolt/editor/tab_bar.hpp"
#include "bolt/editor/debugger_interface.hpp"
#include "bolt/editor/debugger_ui.hpp"
#include "bolt/ai/ai_completion_provider.hpp"
#include <string>
#include <memory>

namespace bolt {

class IntegratedEditor {
private:
    EditorStore& editorStore_;
    CodeFoldingManager& foldingManager_;
    std::unique_ptr<CodeFoldingUI> foldingUI_;
    FileTreeManager& fileTreeManager_;
    std::unique_ptr<FileTreeUI> fileTreeUI_;
    CursorManager& cursorManager_;
    KeyboardShortcuts& keyboardShortcuts_;
    SplitViewManager& splitViewManager_;
    TabBar& tabBar_;
    AICodeCompletionEngine& aiCompletionEngine_;
    std::unique_ptr<CodeCompletion> codeCompletion_;
    
    // Debugger integration
    std::shared_ptr<DebuggerInterface> debugger_;
    std::unique_ptr<DebuggerUI> debuggerUI_;

public:
    static IntegratedEditor& getInstance() {
        static IntegratedEditor instance;
        return instance;
    }

    // Document management with folding integration
    void openDocument(const std::string& filePath, const std::string& content);
    void updateDocumentContent(const std::string& filePath, const std::string& content);
    void closeDocument(const std::string& filePath);
    
    // Folding operations
    void toggleFold(const std::string& filePath, size_t line);
    void expandAllFolds(const std::string& filePath);
    void collapseAllFolds(const std::string& filePath);
    void refreshFolding(const std::string& filePath);
    
    // UI operations
    void renderEditor(const std::string& filePath);
    void handleClick(const std::string& filePath, size_t line, size_t character);
    
    // File tree operations
    void setFileTreeRootDirectory(const std::string& path);
    void refreshFileTree();
    void toggleFileTreeVisibility();
    bool isFileTreeVisible() const;
    void renderFileTree();
    
    // File tree integration with editor
    void openFileFromTree(const std::string& filePath);
    void showFileInTree(const std::string& filePath);
    
    // Folding state queries
    std::vector<FoldRange> getFoldingRanges(const std::string& filePath) const;
    bool isFoldingEnabled() const;
    void setFoldingEnabled(bool enabled);
    
    // Multi-cursor operations
    void addCursorAtPosition(size_t line, size_t column);
    void addCursorAtNextOccurrence(const std::string& text);
    void selectAllOccurrences(const std::string& text);
    void clearExtraCursors();
    void insertTextAtCursors(const std::string& text);
    void deleteAtCursors();
    size_t getCursorCount() const;
    std::vector<Cursor> getAllCursors() const;
    
    // Keyboard shortcut handling
    bool handleKeyboardShortcut(const std::string& key, const std::string& command);
    void initializeKeyboardShortcuts();
    
    // Split view operations
    std::string createHorizontalSplit(const std::string& filePath = "");
    std::string createVerticalSplit(const std::string& filePath = "");
    bool closePane(const std::string& paneId);
    bool focusPane(const std::string& paneId);
    void navigateToNextPane();
    void navigateToPreviousPane();
    
    // Split view management
    bool isSplitViewEnabled() const;
    void setSplitViewEnabled(bool enabled);
    bool hasSplits() const;
    void collapseAllSplits();
    std::vector<std::string> getAllPaneIds() const;
    std::string getActivePaneId() const;
    EditorPane* getActivePane();
    
    // Document operations in split view
    void openDocumentInPane(const std::string& paneId, const std::string& filePath);
    void openDocumentInNewPane(const std::string& filePath, SplitViewManager::SplitDirection direction = SplitViewManager::SplitDirection::Horizontal);
    std::vector<std::string> getOpenDocuments() const;
    std::string findPaneWithDocument(const std::string& filePath) const;
    
    // AI Code Completion operations
    void triggerCodeCompletion(const std::string& filePath, size_t line, size_t column);
    void triggerCodeCompletion(const std::string& filePath, const std::string& prefix);
    std::vector<CompletionItem> getAICompletions(const std::string& filePath, const std::string& prefix, size_t limit = 10);
    bool isCodeCompletionActive() const;
    void acceptCompletion();
    void cancelCompletion();
    void selectNextCompletion();
    void selectPreviousCompletion();
    CompletionItem getSelectedCompletion() const;
    
    // AI completion settings
    void setAICompletionEnabled(bool enabled);
    bool isAICompletionEnabled() const;
    bool isAICompletionReady() const;
    
    // Debugger operations
    bool startDebugSession(const std::string& filePath);
    bool startDebugSessionFromSource(const std::string& limboSource);
    void stopDebugSession();
    bool isDebugging() const;
    DebugState getDebugState() const;
    
    // Debug execution control
    void debugContinue();
    void debugStepOver();
    void debugStepInto();
    void debugStepOut();
    void debugPause();
    
    // Debug breakpoint management
    bool toggleBreakpointAtLine(const std::string& filePath, size_t line);
    bool setBreakpointAtLine(const std::string& filePath, size_t line);
    bool removeBreakpointAtLine(const std::string& filePath, size_t line);
    void clearAllBreakpoints();
    std::vector<BreakpointInfo> getAllBreakpoints() const;
    
    // Debug UI
    void showDebugger();
    void hideDebugger();
    bool isDebuggerVisible() const;
    void toggleDebuggerVisibility();
    
    // Debug information
    size_t getCurrentDebugPC() const;
    std::string getCurrentDebugInstruction() const;
    std::vector<size_t> getDebugCallStack() const;
    std::vector<std::string> getDebugStackContents() const;
    std::map<std::string, std::string> getDebugGlobalVariables() const;
    
    // Tab management operations
    size_t openDocumentInTab(const std::string& filePath, const std::string& content);
    bool closeTabById(size_t tabId);
    bool closeTabByPath(const std::string& filePath);
    void closeAllTabs();
    void closeOtherTabs(size_t exceptTabId);
    bool switchToTab(size_t tabId);
    bool switchToTabByPath(const std::string& filePath);
    void switchToNextTab();
    void switchToPreviousTab();
    std::optional<EditorTab> getActiveTab() const;
    std::vector<EditorTab> getAllTabs() const;
    size_t getTabCount() const;
    bool hasOpenTabs() const;
    void setTabDirty(size_t tabId, bool isDirty);
    void setTabPinned(size_t tabId, bool isPinned);

private:
    IntegratedEditor();
    void detectAndUpdateFolding(const std::string& filePath, const std::string& content);
    void synchronizeFoldingState(const std::string& filePath);
};

} // namespace bolt

#endif