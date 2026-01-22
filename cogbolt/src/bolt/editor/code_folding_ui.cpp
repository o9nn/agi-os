#include "bolt/editor/code_folding_ui.hpp"
namespace bolt {
CodeFoldingUI::CodeFoldingUI() : visible_(true), foldingManager_(std::make_shared<CodeFoldingManager>()) {}
void CodeFoldingUI::toggleVisibility() {
visible_ = !visible_;
}
void CodeFoldingUI::setVisibility(bool visible) {
visible_ = visible;
}
bool CodeFoldingUI::isVisible() const {
return visible_;
}
void CodeFoldingUI::renderFoldingIndicator(size_t lineNumber, bool isFolded) {
if (!visible_) return;
std::string indicator = isFolded ? "[+]" : "[-]";
}
void CodeFoldingUI::handleFoldingClick(size_t lineNumber) {
if (!visible_) return;
}
CodeFoldingUI::FoldingGutter CodeFoldingUI::createGutter() {
return {
true,
14,
"▶",
"▼"
};
}
void CodeFoldingUI::renderFoldingMarkers(const std::string& filePath) {
auto ranges = CodeFoldingManager::getInstance().getFoldingRanges(filePath);
for (const auto& range : ranges) {
if (range.isFolded) {
renderMarker(range.startLine, range.endLine, true);
} else {
renderMarker(range.startLine, range.endLine, false);
}
}
}
void CodeFoldingUI::renderMarker(size_t startLine, size_t endLine, bool isCollapsed) {
auto gutter = createGutter();
if (!gutter.isVisible) return;
int xPos = 0;
int yPos = static_cast<int>(startLine * lineHeight);
const std::string& icon = isCollapsed ? gutter.iconCollapsed : gutter.iconExpanded;
drawIcon(xPos, yPos, icon);
}
void CodeFoldingUI::drawIcon(int x, int y, const std::string& icon) {
}
}