#include "bolt/test_framework.hpp"
#include "bolt/editor/tab_bar.hpp"
#include <iostream>
using namespace bolt;
BOLT_TEST(TabBar, CreateTab) {
TabBar& tabBar = TabBar::getInstance();
tabBar.closeAllTabs();
size_t tabId = tabBar.addTab("/path/to/file1.cpp");
BOLT_ASSERT_TRUE(tabId > 0);
BOLT_ASSERT_EQ(1u, tabBar.getTabCount());
auto tab = tabBar.getTab(tabId);
BOLT_ASSERT_TRUE(tab.has_value());
BOLT_ASSERT_EQ("/path/to/file1.cpp", tab->filePath);
BOLT_ASSERT_EQ("file1.cpp", tab->displayName);
BOLT_ASSERT_FALSE(tab->isDirty);
BOLT_ASSERT_FALSE(tab->isPinned);
}
BOLT_TEST(TabBar, DuplicateTabPrevention) {
TabBar& tabBar = TabBar::getInstance();
tabBar.closeAllTabs();
size_t tabId1 = tabBar.addTab("/path/to/file.cpp");
size_t tabId2 = tabBar.addTab("/path/to/file.cpp");
BOLT_ASSERT_EQ(tabId1, tabId2);
BOLT_ASSERT_EQ(1u, tabBar.getTabCount());
}
BOLT_TEST(TabBar, MultipleTabs) {
TabBar& tabBar = TabBar::getInstance();
tabBar.closeAllTabs();
size_t tab1 = tabBar.addTab("/file1.cpp");
size_t tab2 = tabBar.addTab("/file2.cpp");
size_t tab3 = tabBar.addTab("/file3.cpp");
BOLT_ASSERT_EQ(3u, tabBar.getTabCount());
BOLT_ASSERT_TRUE(tab1 != tab2);
BOLT_ASSERT_TRUE(tab2 != tab3);
}
BOLT_TEST(TabBar, TabActivation) {
TabBar& tabBar = TabBar::getInstance();
tabBar.closeAllTabs();
size_t tab1 = tabBar.addTab("/file1.cpp");
size_t tab2 = tabBar.addTab("/file2.cpp");
auto activeTab = tabBar.getActiveTab();
BOLT_ASSERT_TRUE(activeTab.has_value());
BOLT_ASSERT_EQ(tab2, activeTab->id);
bool activated = tabBar.activateTab(tab1);
BOLT_ASSERT_TRUE(activated);
activeTab = tabBar.getActiveTab();
BOLT_ASSERT_TRUE(activeTab.has_value());
BOLT_ASSERT_EQ(tab1, activeTab->id);
}
BOLT_TEST(TabBar, CloseTab) {
TabBar& tabBar = TabBar::getInstance();
tabBar.closeAllTabs();
(void)tabBar.addTab("/file1.cpp");
size_t tab2 = tabBar.addTab("/file2.cpp");
(void)tabBar.addTab("/file3.cpp");
BOLT_ASSERT_EQ(3u, tabBar.getTabCount());
bool closed = tabBar.closeTab(tab2);
BOLT_ASSERT_TRUE(closed);
BOLT_ASSERT_EQ(2u, tabBar.getTabCount());
auto tab = tabBar.getTab(tab2);
BOLT_ASSERT_FALSE(tab.has_value());
}
BOLT_TEST(TabBar, CloseByPath) {
TabBar& tabBar = TabBar::getInstance();
tabBar.closeAllTabs();
tabBar.addTab("/file1.cpp");
tabBar.addTab("/file2.cpp");
bool closed = tabBar.closeTabByPath("/file1.cpp");
BOLT_ASSERT_TRUE(closed);
BOLT_ASSERT_EQ(1u, tabBar.getTabCount());
}
BOLT_TEST(TabBar, TabNavigation) {
TabBar& tabBar = TabBar::getInstance();
tabBar.closeAllTabs();
size_t tab1 = tabBar.addTab("/file1.cpp");
size_t tab2 = tabBar.addTab("/file2.cpp");
size_t tab3 = tabBar.addTab("/file3.cpp");
tabBar.activateTab(tab3);
tabBar.activateNextTab();
auto active = tabBar.getActiveTab();
BOLT_ASSERT_TRUE(active.has_value());
BOLT_ASSERT_EQ(tab1, active->id);
tabBar.activatePreviousTab();
active = tabBar.getActiveTab();
BOLT_ASSERT_TRUE(active.has_value());
BOLT_ASSERT_EQ(tab3, active->id);
tabBar.activatePreviousTab();
active = tabBar.getActiveTab();
BOLT_ASSERT_TRUE(active.has_value());
BOLT_ASSERT_EQ(tab2, active->id);
}
BOLT_TEST(TabBar, DirtyFlag) {
TabBar& tabBar = TabBar::getInstance();
tabBar.closeAllTabs();
size_t tabId = tabBar.addTab("/file.cpp");
auto tab = tabBar.getTab(tabId);
BOLT_ASSERT_TRUE(tab.has_value());
BOLT_ASSERT_FALSE(tab->isDirty);
tabBar.setTabDirty(tabId, true);
tab = tabBar.getTab(tabId);
BOLT_ASSERT_TRUE(tab->isDirty);
tabBar.setTabDirty(tabId, false);
tab = tabBar.getTab(tabId);
BOLT_ASSERT_FALSE(tab->isDirty);
}
BOLT_TEST(TabBar, PinnedTabs) {
TabBar& tabBar = TabBar::getInstance();
tabBar.closeAllTabs();
size_t tab1 = tabBar.addTab("/file1.cpp");
(void)tabBar.addTab("/file2.cpp");
tabBar.setTabPinned(tab1, true);
bool closed = tabBar.closeTab(tab1);
BOLT_ASSERT_FALSE(closed);
BOLT_ASSERT_EQ(2u, tabBar.getTabCount());
tabBar.closeAllTabs();
BOLT_ASSERT_EQ(1u, tabBar.getTabCount());
auto tab = tabBar.getActiveTab();
BOLT_ASSERT_TRUE(tab.has_value());
BOLT_ASSERT_EQ(tab1, tab->id);
}
BOLT_TEST(TabBar, CloseOtherTabs) {
TabBar& tabBar = TabBar::getInstance();
tabBar.closeAllTabs();
size_t tab1 = tabBar.addTab("/file1.cpp");
(void)tabBar.addTab("/file2.cpp");
size_t tab3 = tabBar.addTab("/file3.cpp");
(void)tabBar.addTab("/file4.cpp");
tabBar.setTabPinned(tab1, true);
tabBar.closeOtherTabs(tab3);
BOLT_ASSERT_EQ(2u, tabBar.getTabCount());
auto t1 = tabBar.getTab(tab1);
auto t3 = tabBar.getTab(tab3);
BOLT_ASSERT_TRUE(t1.has_value());
BOLT_ASSERT_TRUE(t3.has_value());
}
BOLT_TEST(TabBar, GetByPath) {
TabBar& tabBar = TabBar::getInstance();
tabBar.closeAllTabs();
size_t tabId = tabBar.addTab("/my/path/file.cpp");
auto tab = tabBar.getTabByPath("/my/path/file.cpp");
BOLT_ASSERT_TRUE(tab.has_value());
BOLT_ASSERT_EQ(tabId, tab->id);
tab = tabBar.getTabByPath("/does/not/exist.cpp");
BOLT_ASSERT_FALSE(tab.has_value());
}
BOLT_TEST(TabBar, TabReordering) {
TabBar& tabBar = TabBar::getInstance();
tabBar.closeAllTabs();
size_t tab1 = tabBar.addTab("/file1.cpp");
size_t tab2 = tabBar.addTab("/file2.cpp");
size_t tab3 = tabBar.addTab("/file3.cpp");
auto tabs = tabBar.getAllTabs();
BOLT_ASSERT_EQ(3u, tabs.size());
BOLT_ASSERT_EQ(tab1, tabs[0].id);
BOLT_ASSERT_EQ(tab2, tabs[1].id);
BOLT_ASSERT_EQ(tab3, tabs[2].id);
tabBar.moveTab(tab3, 0);
tabs = tabBar.getAllTabs();
BOLT_ASSERT_EQ(tab3, tabs[0].id);
BOLT_ASSERT_EQ(tab1, tabs[1].id);
BOLT_ASSERT_EQ(tab2, tabs[2].id);
}
BOLT_TEST(TabBar, GetAllTabs) {
TabBar& tabBar = TabBar::getInstance();
tabBar.closeAllTabs();
tabBar.addTab("/file1.cpp");
tabBar.addTab("/file2.cpp");
tabBar.addTab("/file3.cpp");
auto tabs = tabBar.getAllTabs();
BOLT_ASSERT_EQ(3u, tabs.size());
BOLT_ASSERT_EQ("/file1.cpp", tabs[0].filePath);
BOLT_ASSERT_EQ("/file2.cpp", tabs[1].filePath);
BOLT_ASSERT_EQ("/file3.cpp", tabs[2].filePath);
}
BOLT_TEST(TabBar, CloseActiveTab) {
TabBar& tabBar = TabBar::getInstance();
tabBar.closeAllTabs();
size_t tab1 = tabBar.addTab("/file1.cpp");
size_t tab2 = tabBar.addTab("/file2.cpp");
(void)tabBar.addTab("/file3.cpp");
tabBar.activateTab(tab2);
tabBar.closeTab(tab2);
auto active = tabBar.getActiveTab();
BOLT_ASSERT_TRUE(active.has_value());
BOLT_ASSERT_EQ(tab1, active->id);
}
BOLT_TEST(TabBar, DisplayNameExtraction) {
TabBar& tabBar = TabBar::getInstance();
tabBar.closeAllTabs();
size_t tab1 = tabBar.addTab("/path/to/my/file.cpp");
size_t tab2 = tabBar.addTab("C:\\Windows\\path\\file.hpp");
size_t tab3 = tabBar.addTab("filename_only.txt");
auto t1 = tabBar.getTab(tab1);
auto t2 = tabBar.getTab(tab2);
auto t3 = tabBar.getTab(tab3);
BOLT_ASSERT_TRUE(t1.has_value());
BOLT_ASSERT_TRUE(t2.has_value());
BOLT_ASSERT_TRUE(t3.has_value());
BOLT_ASSERT_EQ("file.cpp", t1->displayName);
BOLT_ASSERT_EQ("file.hpp", t2->displayName);
BOLT_ASSERT_EQ("filename_only.txt", t3->displayName);
}