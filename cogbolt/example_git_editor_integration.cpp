#include <iostream>
#include <memory>
#include <string>
#include "bolt/git/git.hpp"
#include "bolt/core/editor_store.hpp"
class GitAwareEditor {
public:
GitAwareEditor() {
editorStore_ = std::make_shared<bolt::EditorStore>();
initializeGit();
}
void openFile(const std::string& filePath) {
try {
editorStore_->openDocument(filePath);
if (bolt::git::isGitActive()) {
auto status = bolt::git::quick::status(filePath);
if (status) {
displayGitStatus(filePath, *status);
}
}
std::cout << "✅ Opened file: " << filePath << "\n";
} catch (const std::exception& e) {
std::cout << "❌ Failed to open file: " << e.what() << "\n";
}
}
void saveFile(const std::string& filePath, const std::string& content) {
try {
std::cout << "💾 Saving file: " << filePath << "\n";
if (bolt::git::isGitActive()) {
auto& git = bolt::git::getGitIntegration();
git.notifyFileChanged(filePath);
auto status = bolt::git::quick::status(filePath);
if (status) {
displayGitStatus(filePath, *status);
}
}
std::cout << "✅ File saved successfully\n";
} catch (const std::exception& e) {
std::cout << "❌ Failed to save file: " << e.what() << "\n";
}
}
void showProjectStatus() {
if (!bolt::git::isGitActive()) {
std::cout << "📁 Project is not a Git repository\n";
return;
}
auto& git = bolt::git::getGitIntegration();
auto repoInfo = git.getRepositoryInfo();
std::cout << "\n📊 Project Git Status:\n";
std::cout << "==================\n";
std::cout << "Branch: " << repoInfo.currentBranch << "\n";
std::cout << "Status: " << (repoInfo.isClean ? "🟢 Clean" : "🟡 Has changes") << "\n";
std::cout << "Modified: " << repoInfo.uncommittedFiles << " files\n";
std::cout << "Staged: " << repoInfo.stagedFiles << " files\n";
std::cout << "Untracked: " << repoInfo.untrackedFiles << " files\n\n";
}
void quickCommit(const std::string& message) {
if (!bolt::git::isGitActive()) {
std::cout << "❌ Git not available\n";
return;
}
std::cout << "🚀 Quick commit workflow:\n";
if (bolt::git::quick::stageAll()) {
std::cout << "✅ Staged all changes\n";
if (bolt::git::quick::commit(message)) {
std::cout << "✅ Committed: " << message << "\n";
showProjectStatus();
} else {
std::cout << "❌ Commit failed\n";
}
} else {
std::cout << "❌ Failed to stage changes\n";
}
}
private:
std::shared_ptr<bolt::EditorStore> editorStore_;
void initializeGit() {
std::string currentDir = std::filesystem::current_path().string();
if (bolt::git::initializeGit(currentDir)) {
std::cout << "✅ Git integration initialized\n";
auto& git = bolt::git::getGitIntegration();
git.setOperationCallback([](bool success, const std::string& message) {
std::cout << "🔄 Git: " << (success ? "✅" : "❌") << " " << message << "\n";
});
showProjectStatus();
} else {
std::cout << "ℹ️  Git integration not available (not a Git repository)\n";
}
}
void displayGitStatus(const std::string& filePath, bolt::git::GitFileStatus status) {
std::string statusIcon;
std::string statusText;
switch (status) {
case bolt::git::GitFileStatus::UNTRACKED:
statusIcon = "❓";
statusText = "Untracked";
break;
case bolt::git::GitFileStatus::MODIFIED:
statusIcon = "📝";
statusText = "Modified";
break;
case bolt::git::GitFileStatus::STAGED:
statusIcon = "📦";
statusText = "Staged";
break;
case bolt::git::GitFileStatus::DELETED:
statusIcon = "🗑️";
statusText = "Deleted";
break;
case bolt::git::GitFileStatus::CLEAN:
statusIcon = "🟢";
statusText = "Clean";
break;
default:
statusIcon = "❔";
statusText = "Unknown";
}
std::cout << "📄 " << filePath << " " << statusIcon << " " << statusText << "\n";
}
};
int main() {
std::cout << "=== Git-Aware Editor Example ===\n\n";
GitAwareEditor editor;
std::cout << "\n🔹 Simulating file operations:\n";
editor.openFile("README.md");
editor.saveFile("README.md", "Updated content");
std::cout << "\n🔹 Quick commit example:\n";
editor.quickCommit("Update documentation via Git-aware editor");
std::cout << "\n✨ Example completed! This shows how Git integration can be seamlessly embedded into editor workflows.\n";
return 0;
}