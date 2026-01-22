#include "bolt/editor/code_folding.hpp"
#include <memory>
#include <string>
#include <vector>
namespace bolt {
class CodeFoldingRenderer {
private:
std::shared_ptr<CodeFoldingManager> foldingManager_;
std::shared_ptr<CodeFoldingUI> foldingUI_;
bool showFoldingMarkers_{true};
public:
CodeFoldingRenderer() {
foldingManager_ = std::make_shared<CodeFoldingManager>();
foldingUI_ = std::make_shared<CodeFoldingUI>();
}
void render(const std::string& filePath, size_t viewportStartLine, size_t viewportEndLine) {
if (!showFoldingMarkers_) return;
auto ranges = foldingManager_->getFoldingRanges(filePath);
for (const auto& range : ranges) {
if (range.startLine >= viewportStartLine && range.startLine <= viewportEndLine) {
renderFoldingMarker(range);
}
}
}
void handleClick(const std::string& filePath, size_t line) {
foldingManager_->handleClick(filePath, line);
invalidateView();
}
void setShowFoldingMarkers(bool show) {
showFoldingMarkers_ = show;
invalidateView();
}
private:
void renderFoldingMarker(const FoldRange& range) {
auto gutter = foldingUI_->createGutter();
int xPos = 0;
int yPos = range.startLine * lineHeight;
const std::string& icon = range.isFolded ? gutter.iconCollapsed : gutter.iconExpanded;
drawIcon(xPos, yPos, icon);
}
void drawIcon(int x, int y, const std::string& icon) {
}
void invalidateView() {
}
static constexpr int lineHeight = 20;
};
}