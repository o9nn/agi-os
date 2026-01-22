#include "bolt/editor/split_view_manager.hpp"
#include <algorithm>
#include <sstream>
#include <cmath>
namespace bolt {
SplitViewManager::SplitViewManager() {
std::string initialPaneId = createPane();
activePaneId_.write([&](std::string& activeId) {
activeId = initialPaneId;
});
}
std::string SplitViewManager::createHorizontalSplit(const std::string& sourcePaneId) {
std::string sourceId = sourcePaneId.empty() ? getActivePaneId() : sourcePaneId;
std::string newPaneId;
EditorPane::PanePosition newPanePosition;
newPaneId = withPanes([&](auto& panes) -> std::string {
auto it = panes.find(sourceId);
if (it == panes.end()) {
return "";
}
EditorPane* sourcePane = it->second.get();
EditorPane::PanePosition sourcePos = sourcePane->getPosition();
EditorPane::PanePosition leftPos = sourcePos;
leftPos.width = sourcePos.width / 2;
EditorPane::PanePosition rightPos = sourcePos;
rightPos.x = sourcePos.x + leftPos.width;
rightPos.width = sourcePos.width - leftPos.width;
if (!validatePaneSize(leftPos.width, leftPos.height) ||
!validatePaneSize(rightPos.width, rightPos.height)) {
return "";
}
sourcePane->setPosition(leftPos);
std::string newPaneIdLocal = generatePaneId();
auto newPane = std::make_unique<EditorPane>(newPaneIdLocal);
newPane->setPosition(rightPos);
if (sourcePane->hasDocument()) {
newPane->openDocument(sourcePane->getDocumentPath());
}
panes[newPaneIdLocal] = std::move(newPane);
newPanePosition = rightPos;
return newPaneIdLocal;
});
if (!newPaneId.empty()) {
addPaneToLayout(newPaneId, newPanePosition);
focusPane(newPaneId);
notifyPaneOperation(PaneOperation::Create, newPaneId);
}
return newPaneId;
}
std::string SplitViewManager::createVerticalSplit(const std::string& sourcePaneId) {
std::string sourceId = sourcePaneId.empty() ? getActivePaneId() : sourcePaneId;
std::string newPaneId;
EditorPane::PanePosition newPanePosition;
newPaneId = withPanes([&](auto& panes) -> std::string {
auto it = panes.find(sourceId);
if (it == panes.end()) {
return "";
}
EditorPane* sourcePane = it->second.get();
EditorPane::PanePosition sourcePos = sourcePane->getPosition();
EditorPane::PanePosition topPos = sourcePos;
topPos.height = sourcePos.height / 2;
EditorPane::PanePosition bottomPos = sourcePos;
bottomPos.y = sourcePos.y + topPos.height;
bottomPos.height = sourcePos.height - topPos.height;
if (!validatePaneSize(topPos.width, topPos.height) ||
!validatePaneSize(bottomPos.width, bottomPos.height)) {
return "";
}
sourcePane->setPosition(topPos);
std::string newPaneIdLocal = generatePaneId();
auto newPane = std::make_unique<EditorPane>(newPaneIdLocal);
newPane->setPosition(bottomPos);
if (sourcePane->hasDocument()) {
newPane->openDocument(sourcePane->getDocumentPath());
}
panes[newPaneIdLocal] = std::move(newPane);
newPanePosition = bottomPos;
return newPaneIdLocal;
});
if (!newPaneId.empty()) {
addPaneToLayout(newPaneId, newPanePosition);
focusPane(newPaneId);
notifyPaneOperation(PaneOperation::Create, newPaneId);
}
return newPaneId;
}
bool SplitViewManager::closePane(const std::string& paneId) {
bool paneExists = withPanes([&](const auto& panes) -> bool {
return panes.find(paneId) != panes.end();
});
if (!paneExists) {
return false;
}
std::string activeId = getActivePaneId();
std::string newActivePaneId;
bool success = withPanes([&](auto& panes) -> bool {
if (panes.size() <= 1) {
return false;
}
auto it = panes.find(paneId);
if (it == panes.end()) {
return false;
}
if (activeId == paneId) {
for (const auto& pair : panes) {
if (pair.first != paneId) {
newActivePaneId = pair.first;
break;
}
}
}
panes.erase(it);
return true;
});
if (success) {
if (!newActivePaneId.empty()) {
focusPane(newActivePaneId);
}
removePaneFromLayout(paneId);
redistributeLayout();
notifyPaneOperation(PaneOperation::Close, paneId);
}
return success;
}
bool SplitViewManager::focusPane(const std::string& paneId) {
std::string oldActiveId = getActivePaneId();
bool success = withPanes([&](auto& panes) -> bool {
auto it = panes.find(paneId);
if (it == panes.end()) {
return false;
}
if (!oldActiveId.empty() && oldActiveId != paneId) {
auto oldIt = panes.find(oldActiveId);
if (oldIt != panes.end()) {
oldIt->second->setFocus(false);
}
}
it->second->setFocus(true);
return true;
});
if (success) {
activePaneId_.write([&](std::string& activeId) {
activeId = paneId;
});
notifyPaneOperation(PaneOperation::Focus, paneId);
}
return success;
}
std::string SplitViewManager::createPane() {
std::string paneId = generatePaneId();
auto updatePanes = [&](std::map<std::string, std::unique_ptr<EditorPane>>& panes) {
auto newPane = std::make_unique<EditorPane>(paneId);
EditorPane::PanePosition pos{0, 0, 800, 600};
if (!panes.empty()) {
pos = calculateSplitPosition(pos, SplitDirection::Horizontal);
}
newPane->setPosition(pos);
panes[paneId] = std::move(newPane);
addPaneToLayout(paneId, pos);
};
panes_.write(updatePanes);
notifyPaneOperation(PaneOperation::Create, paneId);
return paneId;
}
EditorPane* SplitViewManager::getPane(const std::string& paneId) {
return withPanes([&](auto& panes) -> EditorPane* {
auto it = panes.find(paneId);
return (it != panes.end()) ? it->second.get() : nullptr;
});
}
const EditorPane* SplitViewManager::getPane(const std::string& paneId) const {
return withPanes([&](const auto& panes) -> const EditorPane* {
auto it = panes.find(paneId);
return (it != panes.end()) ? it->second.get() : nullptr;
});
}
EditorPane* SplitViewManager::getActivePane() {
std::string activeId = getActivePaneId();
return activeId.empty() ? nullptr : getPane(activeId);
}
const EditorPane* SplitViewManager::getActivePane() const {
std::string activeId = getActivePaneId();
return activeId.empty() ? nullptr : getPane(activeId);
}
std::string SplitViewManager::getActivePaneId() const {
return activePaneId_.read([](const std::string& activeId) {
return activeId;
});
}
void SplitViewManager::setContainerSize(int width, int height) {
withPanes([&](auto& panes) {
if (panes.size() == 1) {
auto& pane = panes.begin()->second;
EditorPane::PanePosition pos{0, 0, width, height};
pane->setPosition(pos);
} else {
redistributeLayout();
}
});
}
void SplitViewManager::resizePane(const std::string& paneId, int width, int height) {
withPanes([&](auto& panes) {
auto it = panes.find(paneId);
if (it != panes.end()) {
EditorPane::PanePosition pos = it->second->getPosition();
pos.width = width;
pos.height = height;
enforceSizeConstraints(pos);
it->second->setPosition(pos);
notifyPaneOperation(PaneOperation::Resize, paneId);
}
});
}
void SplitViewManager::redistributeLayout() {
withPanes([&](auto& panes) {
if (panes.empty()) {
return;
}
size_t paneCount = panes.size();
int cols = static_cast<int>(std::ceil(std::sqrt(paneCount)));
int rows = static_cast<int>(std::ceil(static_cast<double>(paneCount) / cols));
int containerWidth = 800;
int containerHeight = 600;
if (!panes.empty()) {
const auto& firstPane = panes.begin()->second;
const auto& pos = firstPane->getPosition();
containerWidth = pos.width * cols;
containerHeight = pos.height * rows;
}
int paneWidth = containerWidth / cols;
int paneHeight = containerHeight / rows;
int index = 0;
for (auto& pair : panes) {
int col = index % cols;
int row = index / cols;
EditorPane::PanePosition pos{
col * paneWidth,
row * paneHeight,
paneWidth,
paneHeight
};
enforceSizeConstraints(pos);
pair.second->setPosition(pos);
index++;
}
});
}
std::vector<std::string> SplitViewManager::getAllPaneIds() const {
return withPanes([](const auto& panes) -> std::vector<std::string> {
std::vector<std::string> ids;
ids.reserve(panes.size());
for (const auto& pair : panes) {
ids.push_back(pair.first);
}
return ids;
});
}
size_t SplitViewManager::getPaneCount() const {
return withPanes([](const auto& panes) -> size_t {
return panes.size();
});
}
void SplitViewManager::openDocumentInPane(const std::string& paneId, const std::string& filePath) {
bool success = withPanes([&](auto& panes) -> bool {
auto it = panes.find(paneId);
if (it != panes.end()) {
it->second->openDocument(filePath);
return true;
}
return false;
});
if (success) {
focusPane(paneId);
}
}
void SplitViewManager::openDocumentInNewPane(const std::string& filePath, SplitDirection direction) {
std::string newPaneId;
if (direction == SplitDirection::Horizontal) {
newPaneId = createHorizontalSplit();
} else {
newPaneId = createVerticalSplit();
}
if (!newPaneId.empty()) {
openDocumentInPane(newPaneId, filePath);
}
}
std::vector<std::string> SplitViewManager::getOpenDocuments() const {
return withPanes([](const auto& panes) -> std::vector<std::string> {
std::vector<std::string> documents;
for (const auto& pair : panes) {
if (pair.second->hasDocument()) {
documents.push_back(pair.second->getDocumentPath());
}
}
return documents;
});
}
std::string SplitViewManager::findPaneWithDocument(const std::string& filePath) const {
return withPanes([&](const auto& panes) -> std::string {
for (const auto& pair : panes) {
if (pair.second->hasDocument() && pair.second->getDocumentPath() == filePath) {
return pair.first;
}
}
return "";
});
}
bool SplitViewManager::isEnabled() const {
return isEnabled_.read([](bool enabled) { return enabled; });
}
void SplitViewManager::setEnabled(bool enabled) {
isEnabled_.write([enabled](bool& currentEnabled) {
currentEnabled = enabled;
});
if (!enabled) {
collapseAllSplits();
}
}
bool SplitViewManager::hasSplits() const {
return getPaneCount() > 1;
}
void SplitViewManager::collapseAllSplits() {
auto updatePanes = [&](std::map<std::string, std::unique_ptr<EditorPane>>& panes) {
if (panes.size() <= 1) {
return;
}
std::string activeId = getActivePaneId();
if (activeId.empty() && !panes.empty()) {
activeId = panes.begin()->first;
}
auto it = panes.begin();
while (it != panes.end()) {
if (it->first != activeId) {
it = panes.erase(it);
} else {
++it;
}
}
if (!panes.empty()) {
EditorPane::PanePosition fullSize{0, 0, 800, 600};
panes.begin()->second->setPosition(fullSize);
}
};
panes_.write(updatePanes);
layouts_.write([](auto& layouts) {
layouts.clear();
});
}
std::string SplitViewManager::getNextPane(const std::string& currentPaneId, bool forward) const {
return withPanes([&](const auto& panes) -> std::string {
if (panes.size() <= 1) {
return currentPaneId;
}
std::vector<std::string> paneIds;
for (const auto& pair : panes) {
paneIds.push_back(pair.first);
}
auto it = std::find(paneIds.begin(), paneIds.end(), currentPaneId);
if (it == paneIds.end()) {
return paneIds.empty() ? "" : paneIds[0];
}
if (forward) {
++it;
if (it == paneIds.end()) {
it = paneIds.begin();
}
} else {
if (it == paneIds.begin()) {
it = paneIds.end() - 1;
} else {
--it;
}
}
return *it;
});
}
bool SplitViewManager::navigateToNextPane() {
std::string currentId = getActivePaneId();
std::string nextId = getNextPane(currentId, true);
return !nextId.empty() && nextId != currentId && focusPane(nextId);
}
bool SplitViewManager::navigateToPreviousPane() {
std::string currentId = getActivePaneId();
std::string prevId = getNextPane(currentId, false);
return !prevId.empty() && prevId != currentId && focusPane(prevId);
}
void SplitViewManager::setOnPaneOperation(PaneOperationCallback callback) {
onPaneOperation_ = callback;
}
std::string SplitViewManager::serializeLayout() const {
std::ostringstream oss;
withPanes([&](const auto& panes) {
for (const auto& pair : panes) {
const auto& pos = pair.second->getPosition();
oss << pair.first << ":" << pos.x << "," << pos.y << "," << pos.width << "," << pos.height;
if (pair.second->hasDocument()) {
oss << ":" << pair.second->getDocumentPath();
}
oss << ";";
}
});
return oss.str();
}
bool SplitViewManager::restoreLayout(const std::string& layoutData) {
clearAllPanes();
std::istringstream iss(layoutData);
std::string paneData;
while (std::getline(iss, paneData, ';')) {
if (paneData.empty()) continue;
size_t colonPos = paneData.find(':');
if (colonPos == std::string::npos) continue;
std::string paneId = paneData.substr(0, colonPos);
createPane();
}
return true;
}
void SplitViewManager::clearAllPanes() {
auto clearPanes = [](std::map<std::string, std::unique_ptr<EditorPane>>& panes) {
panes.clear();
};
panes_.write(clearPanes);
layouts_.write([](auto& layouts) {
layouts.clear();
});
activePaneId_.write([](std::string& activeId) {
activeId = "";
});
}
void SplitViewManager::calculatePanePositions() {
redistributeLayout();
}
void SplitViewManager::updateLayout(const std::string& modifiedPaneId) {
calculatePanePositions();
}
EditorPane::PanePosition SplitViewManager::calculateSplitPosition(
const EditorPane::PanePosition& sourcePosition,
SplitDirection direction,
float splitRatio) {
EditorPane::PanePosition newPosition = sourcePosition;
if (direction == SplitDirection::Horizontal) {
int splitWidth = static_cast<int>(sourcePosition.width * splitRatio);
newPosition.x = sourcePosition.x + splitWidth;
newPosition.width = sourcePosition.width - splitWidth;
} else {
int splitHeight = static_cast<int>(sourcePosition.height * splitRatio);
newPosition.y = sourcePosition.y + splitHeight;
newPosition.height = sourcePosition.height - splitHeight;
}
enforceSizeConstraints(newPosition);
return newPosition;
}
std::string SplitViewManager::generatePaneId() {
return nextPaneId_.write([](int& nextId) -> std::string {
return "pane_" + std::to_string(nextId++);
});
}
void SplitViewManager::notifyPaneOperation(PaneOperation op, const std::string& paneId) {
if (onPaneOperation_) {
onPaneOperation_(op, paneId);
}
}
bool SplitViewManager::isValidPaneId(const std::string& paneId) const {
return withPanes([&](const auto& panes) -> bool {
return panes.find(paneId) != panes.end();
});
}
void SplitViewManager::removePaneFromLayout(const std::string& paneId) {
layouts_.write([&](auto& layouts) {
layouts.erase(
std::remove_if(layouts.begin(), layouts.end(),
[&paneId](const SplitLayout& layout) {
return layout.paneId == paneId;
}),
layouts.end()
);
});
}
void SplitViewManager::addPaneToLayout(const std::string& paneId, const EditorPane::PanePosition& position) {
layouts_.write([&](auto& layouts) {
SplitLayout layout;
layout.paneId = paneId;
layout.position = position;
layouts.push_back(layout);
});
}
bool SplitViewManager::validatePaneSize(int width, int height) const {
return width >= MIN_PANE_WIDTH && height >= MIN_PANE_HEIGHT;
}
void SplitViewManager::enforceSizeConstraints(EditorPane::PanePosition& position) const {
if (position.width < MIN_PANE_WIDTH) {
position.width = MIN_PANE_WIDTH;
}
if (position.height < MIN_PANE_HEIGHT) {
position.height = MIN_PANE_HEIGHT;
}
}
template<typename Func>
auto SplitViewManager::withPanes(Func&& func) const -> decltype(func(std::declval<const std::map<std::string, std::unique_ptr<EditorPane>>&>())) {
return panes_.read(std::forward<Func>(func));
}
template<typename Func>
auto SplitViewManager::withPanes(Func&& func) -> decltype(func(std::declval<std::map<std::string, std::unique_ptr<EditorPane>>&>())) {
return panes_.write(std::forward<Func>(func));
}
}