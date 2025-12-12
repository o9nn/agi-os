#ifndef BOLT_GUI_DARK_THEME_HPP
#define BOLT_GUI_DARK_THEME_HPP

namespace bolt {
namespace gui {

/**
 * Apply bolt.diy inspired dark theme to ImGui
 * This theme features:
 * - Dark slate/charcoal backgrounds (#1a1a1f, #1e1e23)
 * - Blue accent colors for interactive elements
 * - Rounded corners for modern appearance
 * - Proper contrast for readability
 */
void ApplyBoltDarkTheme();

/**
 * Apply bolt.diy inspired light theme to ImGui (optional)
 */
void ApplyBoltLightTheme();

} // namespace gui
} // namespace bolt

#endif // BOLT_GUI_DARK_THEME_HPP
