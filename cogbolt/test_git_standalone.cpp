#include <iostream>
#include <cassert>
#include <string>
#include <filesystem>
#include "bolt/git/git_repository.hpp"
int main() {
std::cout << "Testing Git Integration (Standalone)...\n";
try {
std::string currentDir = std::filesystem::current_path().string();
bolt::git::GitRepository repo(currentDir);
std::cout << "Current directory: " << currentDir << "\n";
bool isGitRepo = repo.isGitRepository();
std::cout << "Is Git repository: " << (isGitRepo ? "Yes" : "No") << "\n";
if (isGitRepo) {
std::string root = repo.getRepositoryRoot();
std::cout << "Repository root: " << root << "\n";
auto branch = repo.getCurrentBranch();
if (branch) {
std::cout << "Current branch: " << *branch << "\n";
} else {
std::cout << "Could not determine current branch\n";
}
auto fileStatus = repo.getFileStatus();
std::cout << "Files with Git status: " << fileStatus.size() << "\n";
bool hasChanges = repo.hasUncommittedChanges();
std::cout << "Has uncommitted changes: " << (hasChanges ? "Yes" : "No") << "\n";
auto branches = repo.getBranches();
std::cout << "Total branches found: " << branches.size() << "\n";
std::cout << "\n✅ Git integration basic functionality works!\n";
} else {
std::cout << "\n⚠️  Not testing Git operations (not a Git repository)\n";
}
return 0;
} catch (const std::exception& e) {
std::cerr << "❌ Test failed: " << e.what() << std::endl;
return 1;
}
}