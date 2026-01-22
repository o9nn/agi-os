#include <vector>
#include <string>
#include <map>
#include <sstream>
#include <algorithm>
std::vector<std::string> get_stack_contents_impl(const DISVM* vm) {
if (!vm) {
return {};
}
std::vector<std::string> contents;
size_t stack_size = vm->stack_size();
contents.push_back("Stack size: " + std::to_string(stack_size));
if (stack_size > 0) {
contents.push_back("Stack contents (top to bottom):");
try {
for (size_t i = 0; i < std::min(stack_size, size_t(10)); ++i) {
std::stringstream ss;
ss << "  [" << i << "] ";
try {
if (vm->can_inspect_stack()) {
auto value = vm->peek_stack(i);
ss << value;
} else {
ss << "<value at depth " << i << ">";
}
} catch (...) {
ss << "<inaccessible>";
}
contents.push_back(ss.str());
}
if (stack_size > 10) {
contents.push_back("  ... (" + std::to_string(stack_size - 10) + " more items)");
}
} catch (const std::exception& e) {
contents.push_back("  Error: " + std::string(e.what()));
}
} else {
contents.push_back("Stack is empty");
}
return contents;
}
std::map<std::string, std::string> get_global_variables_impl(const DISVM* vm) {
std::map<std::string, std::string> variables;
if (!vm) {
return variables;
}
try {
if (vm->has_globals_table()) {
auto globals = vm->get_globals_table();
for (const auto& [name, value] : globals) {
std::stringstream ss;
ss << value;
variables[name] = ss.str();
}
} else {
variables["<globals>"] = "VM does not expose globals table";
}
} catch (const std::exception& e) {
variables["<error>"] = std::string("Error accessing globals: ") + e.what();
}
return variables;
}
void highlight_current_line_impl(IntegratedEditor* editor, const std::string& file_path, int line) {
if (!editor || file_path.empty()) {
return;
}
try {
editor->clear_debug_highlight();
editor->set_debug_highlight(file_path, line);
editor->scroll_to_line(line);
editor->focus();
} catch (const std::exception& e) {
std::cerr << "Error highlighting line: " << e.what() << std::endl;
}
}
void clear_current_line_highlight_impl(IntegratedEditor* editor) {
if (!editor) {
return;
}
try {
editor->clear_debug_highlight();
} catch (const std::exception& e) {
std::cerr << "Error clearing highlight: " << e.what() << std::endl;
}
}
void refresh_breakpoint_markers_impl(IntegratedEditor* editor,
const std::vector<Breakpoint>& breakpoints) {
if (!editor) {
return;
}
try {
editor->clear_all_breakpoint_markers();
for (const auto& bp : breakpoints) {
if (!bp.file_path.empty() && bp.line > 0) {
editor->add_breakpoint_marker(bp.file_path, bp.line, bp.enabled);
}
}
editor->refresh();
} catch (const std::exception& e) {
std::cerr << "Error refreshing breakpoint markers: " << e.what() << std::endl;
}
}
void update_breakpoint_mapping_impl(std::map<std::pair<std::string, int>, size_t>& mapping,
const DISProgram& program) {
mapping.clear();
if (program.debug_info.empty()) {
return;
}
try {
for (const auto& debug_line : program.debug_info) {
size_t first_colon = debug_line.find(':');
size_t second_colon = debug_line.find(':', first_colon + 1);
if (first_colon != std::string::npos && second_colon != std::string::npos) {
std::string file = debug_line.substr(0, first_colon);
std::string line_str = debug_line.substr(first_colon + 1,
second_colon - first_colon - 1);
std::string pc_str = debug_line.substr(second_colon + 1);
int line = std::stoi(line_str);
size_t pc = std::stoull(pc_str);
mapping[{file, line}] = pc;
}
}
} catch (const std::exception& e) {
std::cerr << "Error parsing debug info: " << e.what() << std::endl;
}
}
std::string evaluate_watch_expression_impl(const DISVM* vm, const std::string& expression) {
if (!vm) {
return "<no VM>";
}
try {
if (expression.empty()) {
return "<empty expression>";
}
if (expression[0] == '$') {
try {
size_t index = std::stoull(expression.substr(1));
if (vm->can_inspect_stack() && index < vm->stack_size()) {
auto value = vm->peek_stack(index);
std::stringstream ss;
ss << value;
return ss.str();
} else {
return "<stack index out of range>";
}
} catch (...) {
return "<invalid stack reference>";
}
}
if (vm->has_globals_table()) {
auto globals = vm->get_globals_table();
auto it = globals.find(expression);
if (it != globals.end()) {
std::stringstream ss;
ss << it->second;
return ss.str();
}
}
if (expression == "PC" || expression == "pc") {
return std::to_string(vm->get_pc());
}
if (expression == "SP" || expression == "sp") {
return std::to_string(vm->stack_size());
}
try {
double result = std::stod(expression);
return std::to_string(result);
} catch (...) {
return "<cannot evaluate: " + expression + ">";
}
} catch (const std::exception& e) {
return "<error: " + std::string(e.what()) + ">";
}
}
void setup_vm_handlers_impl(DISVM* vm, DebuggerInterface* debugger) {
if (!vm || !debugger) {
return;
}
try {
vm->set_breakpoint_handler([debugger](size_t pc) {
debugger->on_breakpoint_hit(pc);
});
vm->set_step_handler([debugger](size_t pc) {
debugger->on_step_complete(pc);
});
vm->set_error_handler([debugger](const std::string& error) {
debugger->on_vm_error(error);
});
vm->set_ai_handler([debugger](const std::string& operation, const std::string& data) {
debugger->log_debug_message("AI operation: " + operation + " - " + data);
});
vm->set_glyph_handler([debugger](int glyph_id, int x, int y) {
debugger->log_debug_message("Glyph rendered: " + std::to_string(glyph_id) +
" at (" + std::to_string(x) + ", " + std::to_string(y) + ")");
});
} catch (const std::exception& e) {
std::cerr << "Error setting up VM handlers: " << e.what() << std::endl;
}
}