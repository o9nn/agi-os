#include <iostream>
#include <string>
#include <vector>
#include "bolt/editor/theme_system.hpp"
namespace {
class ThemedEditor {
private:
std::string currentBackground_;
std::string currentForeground_;
std::string currentFont_;
int currentFontSize_;
public:
void applyTheme() {
auto& themeSystem = bolt::ThemeSystem::getInstance();
auto colors = themeSystem.getCurrentColors();
auto fonts = themeSystem.getCurrentFontSettings();
currentBackground_ = colors.background;
currentForeground_ = colors.foreground;
currentFont_ = fonts.family;
currentFontSize_ = fonts.size;
std::cout << "Editor theme applied:" << std::endl;
std::cout << "  Background: " << currentBackground_ << std::endl;
std::cout << "  Foreground: " << currentForeground_ << std::endl;
std::cout << "  Font: " << currentFont_ << " " << currentFontSize_ << "px" << std::endl;
}
void highlightCode(const std::string& code) {
auto& themeSystem = bolt::ThemeSystem::getInstance();
auto colors = themeSystem.getCurrentColors();
std::cout << "\nHighlighted code with current theme:" << std::endl;
std::cout << "----------------------------------------" << std::endl;
std::vector<std::string> lines = {"
for (const auto& line : lines) {
if (line.find("
std::cout << "[" << colors.comment << "] " << line << std::endl;
} else if (line.find("class") != std::string::npos || line.find("int") != std::string::npos || line.find("string") != std::string::npos) {
std::cout << "[" << colors.keyword << "] " << line << std::endl;
} else if (line.find("\"") != std::string::npos) {
std::cout << "[" << colors.string << "] " << line << std::endl;
} else if (line.find("42") != std::string::npos) {
std::cout << "[" << colors.number << "] " << line << std::endl;
} else {
std::cout << "[" << colors.foreground << "] " << line << std::endl;
}
}
}
};
class ThemedUI {
public:
void updateColors() {
auto& themeSystem = bolt::ThemeSystem::getInstance();
auto colors = themeSystem.getCurrentColors();
std::cout << "\nUI component colors updated:" << std::endl;
std::cout << "  Menu background: " << colors.background << std::endl;
std::cout << "  Menu text: " << colors.foreground << std::endl;
std::cout << "  Selection: " << colors.selection << std::endl;
std::cout << "  Line numbers: " << colors.lineNumbers << std::endl;
}
};
void demonstrateIntegration() {
std::cout << "🔗 Theme System Integration Demo" << std::endl;
std::cout << "=================================" << std::endl;
auto& themeSystem = bolt::ThemeSystem::getInstance();
themeSystem.loadDefaultThemes();
ThemedEditor editor;
ThemedUI ui;
std::vector<std::string> themes = {"dark", "light"};
for (const auto& themeName : themes) {
std::cout << "\n📋 Switching to '" << themeName << "' theme..." << std::endl;
std::cout << std::string(50, '-') << std::endl;
themeSystem.setTheme(themeName);
editor.applyTheme();
ui.updateColors();
editor.highlightCode("sample code");
}
std::cout << "\n🎨 Adding custom theme..." << std::endl;
std::cout << std::string(50, '-') << std::endl;
bolt::Theme customTheme;
customTheme.colors.background = "#2B2B2B";
customTheme.colors.foreground = "#A9B7C6";
customTheme.colors.selection = "#214283";
customTheme.colors.lineNumbers = "#606366";
customTheme.colors.comment = "#808080";
customTheme.colors.keyword = "#CC7832";
customTheme.colors.string = "#6A8759";
customTheme.colors.number = "#6897BB";
customTheme.colors.type = "#FFC66D";
customTheme.colors.function = "#FFC66D";
customTheme.fontSettings.family = "JetBrains Mono";
customTheme.fontSettings.size = 15;
customTheme.fontSettings.lineHeight = 1.4f;
themeSystem.addTheme("intellij", customTheme);
themeSystem.setTheme("intellij");
editor.applyTheme();
ui.updateColors();
editor.highlightCode("sample code");
}
void demonstrateThemeListener() {
std::cout << "\n🔔 Theme Change Listener Demo" << std::endl;
std::cout << "==============================" << std::endl;
auto& themeSystem = bolt::ThemeSystem::getInstance();
auto respondToThemeChange = [&]() {
std::cout << "📢 Theme changed to: " << themeSystem.getCurrentThemeName() << std::endl;
auto colors = themeSystem.getCurrentColors();
std::cout << "   New background: " << colors.background << std::endl;
std::cout << "   New foreground: " << colors.foreground << std::endl;
};
std::vector<std::string> themes = {"dark", "light", "intellij"};
for (const auto& theme : themes) {
themeSystem.setTheme(theme);
respondToThemeChange();
}
}
}
int main() {
std::cout << "🎨 Theme System Integration Examples" << std::endl;
std::cout << "====================================" << std::endl;
demonstrateIntegration();
demonstrateThemeListener();
std::cout << "\n✅ Integration Examples Complete!" << std::endl;
std::cout << "These examples show how editor components can integrate with the theme system:" << std::endl;
std::cout << "- Editor components apply themes to their visual elements" << std::endl;
std::cout << "- UI components update colors when themes change" << std::endl;
std::cout << "- Syntax highlighting uses theme colors for different token types" << std::endl;
std::cout << "- Components can listen for theme changes and update automatically" << std::endl;
return 0;
}