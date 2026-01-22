#include "bolt/editor/editor_pane.hpp"
#include "bolt/core/editor_store.hpp"
#include <fstream>
#include <stdexcept>
#include <sstream>
namespace bolt {
EditorPane::EditorPane(const std::string& paneId)
: paneId_(paneId)
, position_{0, 0, 800, 600}
, editorStore_(EditorStore::getInstance()) {
state_.documentPath = "";
state_.cursorLine = 0;
state_.cursorColumn = 0;
state_.scrollLine = 0;
state_.scrollColumn = 0;
state_.hasFocus = false;
state_.isVisible = true;
initializeMinimap();
}
void EditorPane::setPosition(const PanePosition& position) {
position_ = position;
if (state_.isVisible) {
refresh();
}
}
void EditorPane::openDocument(const std::string& filePath) {
if (filePath.empty()) {
return;
}
if (state_.documentPath == filePath) {
setFocus(true);
return;
}
if (hasDocument()) {
closeDocument();
}
state_.documentPath = filePath;
state_.cursorLine = 0;
state_.cursorColumn = 0;
state_.scrollLine = 0;
state_.scrollColumn = 0;
try {
std::ifstream file(filePath);
if (file.is_open()) {
std::string content((std::istreambuf_iterator<char>(file)),
std::istreambuf_iterator<char>());
file.close();
EditorDocument doc;
doc.value = content;
doc.filePath = filePath;
doc.scroll = {static_cast<int>(state_.scrollLine), static_cast<int>(state_.scrollColumn)};
doc.cursor = {static_cast<size_t>(state_.cursorLine), std::nullopt};
doc.foldingRanges = {};
editorStore_.setDocument(filePath, doc);
syncWithEditorStore();
}
} catch (const std::exception&) {
state_.documentPath = filePath;
updateDocumentContent("");
}
updateMinimap();
refresh();
}
void EditorPane::closeDocument() {
if (!hasDocument()) {
return;
}
syncWithEditorStore();
state_.documentPath = "";
state_.cursorLine = 0;
state_.cursorColumn = 0;
state_.scrollLine = 0;
state_.scrollColumn = 0;
refresh();
}
void EditorPane::setFocus(bool focus) {
state_.hasFocus = focus;
if (focus && hasDocument()) {
editorStore_.setSelectedFile(state_.documentPath);
syncWithEditorStore();
}
refresh();
}
void EditorPane::setVisible(bool visible) {
state_.isVisible = visible;
if (visible) {
refresh();
}
}
void EditorPane::setCursorPosition(size_t line, size_t column) {
state_.cursorLine = line;
state_.cursorColumn = column;
if (hasDocument() && state_.hasFocus) {
syncWithEditorStore();
}
updateMinimapViewport();
}
void EditorPane::getCursorPosition(size_t& line, size_t& column) const {
line = state_.cursorLine;
column = state_.cursorColumn;
}
void EditorPane::setScrollPosition(size_t line, size_t column) {
state_.scrollLine = line;
state_.scrollColumn = column;
if (hasDocument() && state_.hasFocus) {
editorStore_.updateScrollPosition(state_.documentPath,
static_cast<int>(line),
static_cast<int>(column));
}
updateMinimapViewport();
}
void EditorPane::getScrollPosition(size_t& line, size_t& column) const {
line = state_.scrollLine;
column = state_.scrollColumn;
}
std::string EditorPane::getDocumentContent() const {
if (!hasDocument()) {
return "";
}
auto* doc = editorStore_.getCurrentDocument();
if (doc && doc->filePath == state_.documentPath) {
return doc->value;
}
try {
std::ifstream file(state_.documentPath);
if (file.is_open()) {
std::string content((std::istreambuf_iterator<char>(file)),
std::istreambuf_iterator<char>());
file.close();
return content;
}
} catch (const std::exception&) {
}
return "";
}
void EditorPane::updateDocumentContent(const std::string& content) {
if (!hasDocument()) {
return;
}
updateEditorStoreDocument();
auto* doc = editorStore_.getCurrentDocument();
if (doc && doc->filePath == state_.documentPath) {
doc->value = content;
} else {
EditorDocument newDoc;
newDoc.value = content;
newDoc.filePath = state_.documentPath;
newDoc.scroll = {static_cast<int>(state_.scrollLine), static_cast<int>(state_.scrollColumn)};
newDoc.cursor = {static_cast<size_t>(state_.cursorLine), std::nullopt};
newDoc.foldingRanges = {};
editorStore_.setDocument(state_.documentPath, newDoc);
}
refresh();
}
void EditorPane::render() {
if (!state_.isVisible) {
return;
}
if (hasDocument() && state_.hasFocus) {
syncWithEditorStore();
}
}
void EditorPane::refresh() {
render();
}
void EditorPane::restoreState(const PaneState& state) {
state_ = state;
if (hasDocument()) {
syncWithEditorStore();
refresh();
}
}
void EditorPane::syncWithEditorStore() {
if (!hasDocument()) {
return;
}
if (state_.hasFocus) {
editorStore_.setSelectedFile(state_.documentPath);
auto* doc = editorStore_.getCurrentDocument();
if (doc && doc->filePath == state_.documentPath) {
doc->cursor = {static_cast<size_t>(state_.cursorLine), std::nullopt};
doc->scroll = {static_cast<int>(state_.scrollLine), static_cast<int>(state_.scrollColumn)};
}
}
}
void EditorPane::updateEditorStoreDocument() {
if (!hasDocument()) {
return;
}
auto* doc = editorStore_.getCurrentDocument();
if (!doc || doc->filePath != state_.documentPath) {
EditorDocument newDoc;
newDoc.filePath = state_.documentPath;
newDoc.value = getDocumentContent();
newDoc.scroll = {static_cast<int>(state_.scrollLine), static_cast<int>(state_.scrollColumn)};
newDoc.cursor = {static_cast<size_t>(state_.cursorLine), std::nullopt};
newDoc.foldingRanges = {};
editorStore_.setDocument(state_.documentPath, newDoc);
}
}
void EditorPane::setMinimapEnabled(bool enabled) {
minimapEnabled_ = enabled;
if (minimapUI_) {
minimapUI_->setVisible(enabled);
}
}
void EditorPane::updateMinimap() {
if (!minimapEnabled_ || !minimap_ || !hasDocument()) {
return;
}
auto lines = getDocumentLines();
minimap_->setDocument(state_.documentPath, lines);
updateMinimapViewport();
}
std::vector<std::string> EditorPane::renderMinimap() const {
if (!minimapEnabled_ || !minimapUI_ || !minimapUI_->isVisible()) {
return {};
}
return minimapUI_->render();
}
void EditorPane::initializeMinimap() {
Minimap::MinimapConfig config;
config.width = std::min(size_t{60}, static_cast<size_t>(position_.width / 8));
config.height = std::min(size_t{30}, static_cast<size_t>(position_.height / 4));
minimap_ = std::make_unique<Minimap>(config);
MinimapUI::UIConfig uiConfig;
uiConfig.showBorder = true;
uiConfig.showTitle = true;
uiConfig.showScrollIndicator = true;
uiConfig.title = "Minimap";
minimapUI_ = std::make_unique<MinimapUI>(*minimap_, uiConfig);
minimapUI_->setNavigationCallback([this](size_t line) {
setCursorPosition(line, 0);
setScrollPosition(line > 10 ? line - 10 : 0, 0);
});
MinimapUI::Position minimapPos;
minimapPos.x = position_.x + position_.width - minimapUI_->getRequiredWidth() - 10;
minimapPos.y = position_.y + 10;
minimapPos.width = minimapUI_->getRequiredWidth();
minimapPos.height = minimapUI_->getRequiredHeight();
minimapUI_->setPosition(minimapPos);
}
void EditorPane::updateMinimapViewport() {
if (!minimap_ || !hasDocument()) {
return;
}
auto lines = getDocumentLines();
size_t visibleLines = position_.height / 20;
Minimap::ViewportInfo viewport;
viewport.startLine = state_.scrollLine;
viewport.endLine = std::min(state_.scrollLine + visibleLines, lines.size());
viewport.currentLine = state_.cursorLine;
viewport.totalLines = lines.size();
minimap_->updateViewport(viewport);
}
std::vector<std::string> EditorPane::getDocumentLines() const {
if (!hasDocument()) {
return {};
}
std::string content = getDocumentContent();
std::vector<std::string> lines;
std::istringstream stream(content);
std::string line;
while (std::getline(stream, line)) {
lines.push_back(line);
}
return lines;
}
}