#include <iostream>
#include <cassert>
#include <algorithm>
#include "bolt/editor/theme_system.hpp"
namespace {
void runTest(const std::string& testName, void(*testFunc)()) {
try {
testFunc();
std::cout << "[PASS] " << testName << std::endl;
} catch (const std::exception& e) {
std::cout << "[FAIL] " << testName << " - " << e.what() << std::endl;
throw;
} catch (...) {
std::cout << "[FAIL] " << testName << " - Unknown error" << std::endl;
throw;
}
}
void testSingleton() {
auto& instance1 = bolt::ThemeSystem::getInstance();
auto& instance2 = bolt::ThemeSystem::getInstance();
assert(&instance1 == &instance2);
}
void testDefaultThemesLoading() {
auto& themeSystem = bolt::ThemeSystem::getInstance();
themeSystem.loadDefaultThemes();
themeSystem.setTheme("dark");
auto darkColors = themeSystem.getCurrentColors();
assert(darkColors.background == "#1E1E1E");
assert(darkColors.foreground == "#D4D4D4");
assert(darkColors.keyword == "#569CD6");
themeSystem.setTheme("light");
auto lightColors = themeSystem.getCurrentColors();
assert(lightColors.background == "#FFFFFF");
assert(lightColors.foreground == "#000000");
assert(lightColors.keyword == "#0000FF");
}
void testFontSettings() {
auto& themeSystem = bolt::ThemeSystem::getInstance();
themeSystem.loadDefaultThemes();
themeSystem.setTheme("dark");
auto fontSettings = themeSystem.getCurrentFontSettings();
assert(fontSettings.family == "Consolas, 'Courier New', monospace");
assert(fontSettings.size == 14);
assert(fontSettings.lineHeight == 1.5f);
}
void testCustomTheme() {
auto& themeSystem = bolt::ThemeSystem::getInstance();
bolt::Theme customTheme;
customTheme.colors.background = "#222222";
customTheme.colors.foreground = "#EEEEEE";
customTheme.colors.keyword = "#FF6B6B";
customTheme.fontSettings.family = "Monaco";
customTheme.fontSettings.size = 16;
customTheme.fontSettings.lineHeight = 1.6f;
themeSystem.addTheme("custom", customTheme);
themeSystem.setTheme("custom");
auto colors = themeSystem.getCurrentColors();
assert(colors.background == "#222222");
assert(colors.foreground == "#EEEEEE");
assert(colors.keyword == "#FF6B6B");
auto fontSettings = themeSystem.getCurrentFontSettings();
assert(fontSettings.family == "Monaco");
assert(fontSettings.size == 16);
assert(fontSettings.lineHeight == 1.6f);
}
void testInvalidTheme() {
auto& themeSystem = bolt::ThemeSystem::getInstance();
themeSystem.loadDefaultThemes();
themeSystem.setTheme("dark");
auto validColors = themeSystem.getCurrentColors();
themeSystem.setTheme("nonexistent");
auto currentColors = themeSystem.getCurrentColors();
assert(currentColors.background == validColors.background);
assert(currentColors.foreground == validColors.foreground);
}
void testThemeSwitching() {
auto& themeSystem = bolt::ThemeSystem::getInstance();
themeSystem.loadDefaultThemes();
themeSystem.setTheme("light");
auto lightColors = themeSystem.getCurrentColors();
assert(lightColors.background == "#FFFFFF");
themeSystem.setTheme("dark");
auto darkColors = themeSystem.getCurrentColors();
assert(darkColors.background == "#1E1E1E");
themeSystem.setTheme("light");
auto lightColors2 = themeSystem.getCurrentColors();
assert(lightColors2.background == "#FFFFFF");
}
void testThemeEnumeration() {
auto& themeSystem = bolt::ThemeSystem::getInstance();
themeSystem.loadDefaultThemes();
auto themes = themeSystem.getAvailableThemes();
assert(themes.size() >= 2);
auto darkFound = std::find(themes.begin(), themes.end(), "dark") != themes.end();
auto lightFound = std::find(themes.begin(), themes.end(), "light") != themes.end();
assert(darkFound);
assert(lightFound);
}
void testCurrentThemeName() {
auto& themeSystem = bolt::ThemeSystem::getInstance();
themeSystem.loadDefaultThemes();
themeSystem.setTheme("light");
assert(themeSystem.getCurrentThemeName() == "light");
themeSystem.setTheme("dark");
assert(themeSystem.getCurrentThemeName() == "dark");
}
void testThemeExistence() {
auto& themeSystem = bolt::ThemeSystem::getInstance();
themeSystem.loadDefaultThemes();
assert(themeSystem.hasTheme("dark"));
assert(themeSystem.hasTheme("light"));
assert(!themeSystem.hasTheme("nonexistent"));
}
void testColorSchemeCompleteness() {
auto& themeSystem = bolt::ThemeSystem::getInstance();
themeSystem.loadDefaultThemes();
themeSystem.setTheme("dark");
auto colors = themeSystem.getCurrentColors();
assert(!colors.background.empty());
assert(!colors.foreground.empty());
assert(!colors.selection.empty());
assert(!colors.lineNumbers.empty());
assert(!colors.comment.empty());
assert(!colors.keyword.empty());
assert(!colors.string.empty());
assert(!colors.number.empty());
assert(!colors.type.empty());
assert(!colors.function.empty());
}
}
int main() {
std::cout << "Running Theme System Tests..." << std::endl;
try {
runTest("Singleton Pattern", testSingleton);
runTest("Default Themes Loading", testDefaultThemesLoading);
runTest("Font Settings", testFontSettings);
runTest("Custom Theme Addition", testCustomTheme);
runTest("Invalid Theme Handling", testInvalidTheme);
runTest("Theme Switching", testThemeSwitching);
runTest("Color Scheme Completeness", testColorSchemeCompleteness);
runTest("Theme Enumeration", testThemeEnumeration);
runTest("Current Theme Name", testCurrentThemeName);
runTest("Theme Existence Check", testThemeExistence);
std::cout << "\n✅ All Theme System tests passed!" << std::endl;
return 0;
} catch (...) {
std::cout << "\n❌ Some Theme System tests failed!" << std::endl;
return 1;
}
}