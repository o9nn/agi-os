#include "bolt/editor/integrated_editor.hpp"
#include <iostream>
#include <filesystem>
#include <fstream>
using namespace bolt;
void demonstrateFileTreeIntegration() {
std::cout << "Bolt C++ File Tree Integration Demo\n";
std::cout << "===================================\n\n";
auto& editor = IntegratedEditor::getInstance();
std::string testDir = "/tmp/bolt_integration_demo";
std::filesystem::create_directories(testDir);
std::filesystem::create_directories(testDir + "/src");
std::filesystem::create_directories(testDir + "/include");
std::ofstream(testDir + "/main.cpp")
<< "#include <iostream>\n\nint main() {\n    std::cout << \"Hello World!\" << std::endl;\n    return 0;\n}\n";
std::ofstream(testDir + "/README.md")
<< "# Test Project\n\nThis is a test project for the file tree demo.\n";
std::ofstream(testDir + "/src/utils.cpp")
<< "#include \"utils.h\"\n\nvoid utils_function() {\n
std::ofstream(testDir + "/include/utils.h")
<< "#ifndef UTILS_H\n#define UTILS_H\n\nvoid utils_function();\n\n#endif\n";
std::cout << "Created test project structure at: " << testDir << "\n\n";
std::cout << "Setting file tree root directory...\n";
editor.setFileTreeRootDirectory(testDir);
std::cout << "Rendering file tree:\n";
editor.renderFileTree();
std::cout << "\n";
std::string mainCppPath = testDir + "/main.cpp";
std::cout << "Opening " << mainCppPath << " through file tree integration...\n";
editor.openFileFromTree(mainCppPath);
std::cout << "File has been opened in the editor!\n\n";
std::cout << "Selecting the file in the tree:\n";
editor.showFileInTree(mainCppPath);
editor.renderFileTree();
std::cout << "\n";
std::cout << "File tree is visible: " << (editor.isFileTreeVisible() ? "Yes" : "No") << "\n";
std::cout << "Toggling file tree visibility...\n";
editor.toggleFileTreeVisibility();
std::cout << "File tree is visible: " << (editor.isFileTreeVisible() ? "Yes" : "No") << "\n";
editor.toggleFileTreeVisibility();
std::cout << "Toggled back. File tree is visible: " << (editor.isFileTreeVisible() ? "Yes" : "No") << "\n\n";
std::cout << "Testing folding integration...\n";
editor.setFoldingEnabled(true);
std::string utilsCppPath = testDir + "/src/utils.cpp";
editor.openFileFromTree(utilsCppPath);
auto foldingRanges = editor.getFoldingRanges(utilsCppPath);
std::cout << "Folding ranges detected for " << utilsCppPath << ": " << foldingRanges.size() << " ranges\n";
std::cout << "\nRefreshing file tree...\n";
editor.refreshFileTree();
editor.renderFileTree();
std::filesystem::remove_all(testDir);
std::cout << "\nDemo completed! Test directory cleaned up.\n";
std::cout << "The file tree is fully integrated with the editor!\n";
}
int main() {
try {
demonstrateFileTreeIntegration();
return 0;
} catch (const std::exception& e) {
std::cerr << "Error: " << e.what() << std::endl;
return 1;
}
}