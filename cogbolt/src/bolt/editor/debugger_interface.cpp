#include "bolt/editor/debugger_interface.hpp"
#include "bolt/editor/integrated_editor.hpp"
#include "bolt/drawkern/dis_vm.hpp"
#include <iostream>
#include <sstream>
#include <algorithm>

namespace bolt {

DebuggerInterface::DebuggerInterface() 
    : vm_(std::make_unique<drawkern::DISVM>())
    , state_(DebugState::STOPPED)
    , event_callback_(nullptr)
    , editor_(nullptr)
    , debug_output_enabled_(true) {
    
    setup_vm_handlers();
}

DebuggerInterface::~DebuggerInterface() {
    stop_debug_session();
}

bool DebuggerInterface::start_debug_session(const drawkern::DISProgram& program) {
    if (state_ != DebugState::STOPPED) {
        stop_debug_session();
    }
    
    if (!vm_->load_program(program)) {
        transition_state(DebugState::ERROR, "Failed to load program");
        return false;
    }
    
    // Restore breakpoints
    for (const auto& bp : breakpoints_) {
        if (bp.enabled) {
            vm_->set_breakpoint(bp.pc);
        }
    }
    
    transition_state(DebugState::PAUSED, "Debug session started");
    fire_event(DebugEvent::SESSION_STARTED, "Debugging session initialized");
    
    highlight_current_line();
    refresh_breakpoint_markers();
    
    log_debug_message("Debug session started with " + std::to_string(program.instructions.size()) + " instructions");
    
    return true;
}

bool DebuggerInterface::start_debug_session_from_source(const std::string& limbo_source) {
    if (state_ != DebugState::STOPPED) {
        stop_debug_session();
    }
    
    if (!vm_->load_limbo_source(limbo_source)) {
        transition_state(DebugState::ERROR, "Failed to compile Limbo source");
        return false;
    }
    
    // Restore breakpoints
    for (const auto& bp : breakpoints_) {
        if (bp.enabled) {
            vm_->set_breakpoint(bp.pc);
        }
    }
    
    transition_state(DebugState::PAUSED, "Debug session started from source");
    fire_event(DebugEvent::SESSION_STARTED, "Debugging session initialized from Limbo source");
    
    highlight_current_line();
    refresh_breakpoint_markers();
    
    log_debug_message("Debug session started from Limbo source");
    
    return true;
}

void DebuggerInterface::stop_debug_session() {
    if (state_ == DebugState::STOPPED) {
        return;
    }
    
    vm_->halt();
    clear_current_line_highlight();
    
    transition_state(DebugState::STOPPED, "Debug session stopped");
    fire_event(DebugEvent::SESSION_STOPPED, "Debugging session ended");
    
    log_debug_message("Debug session stopped");
}

bool DebuggerInterface::is_debugging() const {
    return state_ != DebugState::STOPPED;
}

DebugState DebuggerInterface::get_debug_state() const {
    return state_;
}

void DebuggerInterface::continue_execution() {
    if (state_ != DebugState::PAUSED) {
        return;
    }
    
    transition_state(DebugState::RUNNING, "Continuing execution");
    
    vm_->continue_execution();
    
    if (vm_->is_at_breakpoint()) {
        transition_state(DebugState::PAUSED, "Breakpoint hit");
        fire_event(DebugEvent::BREAKPOINT_HIT, "Execution paused at breakpoint");
        highlight_current_line();
    } else if (!vm_->is_running()) {
        transition_state(DebugState::FINISHED, "Execution completed");
        fire_event(DebugEvent::EXECUTION_FINISHED, "Program execution completed");
        clear_current_line_highlight();
    }
    
    update_watch_expressions();
}

void DebuggerInterface::step_over() {
    if (state_ != DebugState::PAUSED) {
        return;
    }
    
    size_t old_pc = vm_->get_pc();
    vm_->step_over();
    
    if (vm_->is_at_breakpoint()) {
        transition_state(DebugState::PAUSED, "Breakpoint hit during step");
        fire_event(DebugEvent::BREAKPOINT_HIT, "Execution paused at breakpoint");
    } else if (!vm_->is_running()) {
        transition_state(DebugState::FINISHED, "Execution completed");
        fire_event(DebugEvent::EXECUTION_FINISHED, "Program execution completed");
        clear_current_line_highlight();
    } else {
        transition_state(DebugState::PAUSED, "Step completed");
        fire_event(DebugEvent::STEP_COMPLETED, "Step over completed");
        highlight_current_line();
    }
    
    update_watch_expressions();
    log_debug_message("Step over: PC " + std::to_string(old_pc) + " -> " + std::to_string(vm_->get_pc()));
}

void DebuggerInterface::step_into() {
    if (state_ != DebugState::PAUSED) {
        return;
    }
    
    size_t old_pc = vm_->get_pc();
    vm_->step_into();
    
    if (vm_->is_at_breakpoint()) {
        transition_state(DebugState::PAUSED, "Breakpoint hit during step");
        fire_event(DebugEvent::BREAKPOINT_HIT, "Execution paused at breakpoint");
    } else if (!vm_->is_running()) {
        transition_state(DebugState::FINISHED, "Execution completed");
        fire_event(DebugEvent::EXECUTION_FINISHED, "Program execution completed");
        clear_current_line_highlight();
    } else {
        transition_state(DebugState::PAUSED, "Step completed");
        fire_event(DebugEvent::STEP_COMPLETED, "Step into completed");
        highlight_current_line();
    }
    
    update_watch_expressions();
    log_debug_message("Step into: PC " + std::to_string(old_pc) + " -> " + std::to_string(vm_->get_pc()));
}

void DebuggerInterface::step_out() {
    if (state_ != DebugState::PAUSED) {
        return;
    }
    
    size_t old_pc = vm_->get_pc();
    size_t old_depth = vm_->get_call_stack_depth();
    
    vm_->step_out();
    
    if (vm_->is_at_breakpoint()) {
        transition_state(DebugState::PAUSED, "Breakpoint hit during step out");
        fire_event(DebugEvent::BREAKPOINT_HIT, "Execution paused at breakpoint");
    } else if (!vm_->is_running()) {
        transition_state(DebugState::FINISHED, "Execution completed");
        fire_event(DebugEvent::EXECUTION_FINISHED, "Program execution completed");
        clear_current_line_highlight();
    } else {
        transition_state(DebugState::PAUSED, "Step out completed");
        fire_event(DebugEvent::STEP_COMPLETED, "Step out completed");
        highlight_current_line();
    }
    
    update_watch_expressions();
    log_debug_message("Step out: PC " + std::to_string(old_pc) + " -> " + std::to_string(vm_->get_pc()) + 
                     ", depth " + std::to_string(old_depth) + " -> " + std::to_string(vm_->get_call_stack_depth()));
}

void DebuggerInterface::pause_execution() {
    if (state_ == DebugState::RUNNING) {
        vm_->halt();
        transition_state(DebugState::PAUSED, "Execution paused");
        highlight_current_line();
        log_debug_message("Execution paused by user");
    }
}

bool DebuggerInterface::set_breakpoint(size_t pc) {
    if (!is_valid_pc(pc)) {
        return false;
    }
    
    // Check if breakpoint already exists
    auto it = std::find_if(breakpoints_.begin(), breakpoints_.end(),
                          [pc](const BreakpointInfo& bp) { return bp.pc == pc; });
    
    if (it != breakpoints_.end()) {
        // Re-enable existing breakpoint
        it->enabled = true;
        if (is_debugging()) {
            vm_->set_breakpoint(pc);
        }
        log_debug_message("Re-enabled breakpoint at PC " + std::to_string(pc));
        return true;
    }
    
    // Create new breakpoint
    auto [file_path, line] = find_line_for_pc(pc);
    breakpoints_.emplace_back(pc, file_path, line);
    
    if (is_debugging()) {
        vm_->set_breakpoint(pc);
    }
    
    refresh_breakpoint_markers();
    log_debug_message("Set breakpoint at PC " + std::to_string(pc) + 
                     (file_path.empty() ? "" : " (" + file_path + ":" + std::to_string(line) + ")"));
    
    return true;
}

bool DebuggerInterface::set_breakpoint_at_line(const std::string& file_path, size_t line) {
    size_t pc = find_pc_for_line(file_path, line);
    if (pc == SIZE_MAX) {
        return false;
    }
    
    return set_breakpoint(pc);
}

bool DebuggerInterface::remove_breakpoint(size_t pc) {
    auto it = std::find_if(breakpoints_.begin(), breakpoints_.end(),
                          [pc](const BreakpointInfo& bp) { return bp.pc == pc; });
    
    if (it == breakpoints_.end()) {
        return false;
    }
    
    if (is_debugging()) {
        vm_->remove_breakpoint(pc);
    }
    
    breakpoints_.erase(it);
    refresh_breakpoint_markers();
    
    log_debug_message("Removed breakpoint at PC " + std::to_string(pc));
    return true;
}

bool DebuggerInterface::remove_breakpoint_at_line(const std::string& file_path, size_t line) {
    size_t pc = find_pc_for_line(file_path, line);
    if (pc == SIZE_MAX) {
        return false;
    }
    
    return remove_breakpoint(pc);
}

void DebuggerInterface::clear_all_breakpoints() {
    if (is_debugging()) {
        vm_->clear_all_breakpoints();
    }
    
    breakpoints_.clear();
    refresh_breakpoint_markers();
    
    log_debug_message("Cleared all breakpoints");
}

bool DebuggerInterface::toggle_breakpoint(size_t pc) {
    if (has_breakpoint(pc)) {
        return remove_breakpoint(pc);
    } else {
        return set_breakpoint(pc);
    }
}

bool DebuggerInterface::enable_breakpoint(size_t pc, bool enabled) {
    auto it = std::find_if(breakpoints_.begin(), breakpoints_.end(),
                          [pc](const BreakpointInfo& bp) { return bp.pc == pc; });
    
    if (it == breakpoints_.end()) {
        return false;
    }
    
    it->enabled = enabled;
    
    if (is_debugging()) {
        if (enabled) {
            vm_->set_breakpoint(pc);
        } else {
            vm_->remove_breakpoint(pc);
        }
    }
    
    refresh_breakpoint_markers();
    log_debug_message((enabled ? "Enabled" : "Disabled") + std::string(" breakpoint at PC ") + std::to_string(pc));
    
    return true;
}

std::vector<BreakpointInfo> DebuggerInterface::get_all_breakpoints() const {
    return breakpoints_;
}

bool DebuggerInterface::has_breakpoint(size_t pc) const {
    return std::any_of(breakpoints_.begin(), breakpoints_.end(),
                      [pc](const BreakpointInfo& bp) { return bp.pc == pc; });
}

size_t DebuggerInterface::get_current_pc() const {
    return vm_->get_pc();
}

std::string DebuggerInterface::get_current_instruction() const {
    return vm_->get_instruction_info(vm_->get_pc());
}

std::vector<size_t> DebuggerInterface::get_call_stack() const {
    return vm_->get_call_stack();
}

size_t DebuggerInterface::get_call_stack_depth() const {
    return vm_->get_call_stack_depth();
}

std::vector<std::string> DebuggerInterface::get_stack_contents() const {
    // Note: This is a simplified implementation since std::stack doesn't provide iteration
    std::vector<std::string> contents;
    contents.push_back("Stack size: " + std::to_string(vm_->stack_size()));
    // TODO: Add actual stack content inspection when VM provides access
    return contents;
}

std::map<std::string, std::string> DebuggerInterface::get_global_variables() const {
    std::map<std::string, std::string> variables;
    // TODO: Implement once VM provides access to globals inspection
    return variables;
}

bool DebuggerInterface::add_watch_expression(const std::string& expression) {
    // Check if already exists
    auto it = std::find_if(watch_expressions_.begin(), watch_expressions_.end(),
                          [&expression](const WatchExpression& we) { 
                              return we.expression == expression; 
                          });
    
    if (it != watch_expressions_.end()) {
        return false; // Already exists
    }
    
    watch_expressions_.emplace_back(expression);
    update_watch_expressions();
    
    log_debug_message("Added watch expression: " + expression);
    return true;
}

bool DebuggerInterface::remove_watch_expression(const std::string& expression) {
    auto it = std::find_if(watch_expressions_.begin(), watch_expressions_.end(),
                          [&expression](const WatchExpression& we) { 
                              return we.expression == expression; 
                          });
    
    if (it == watch_expressions_.end()) {
        return false;
    }
    
    watch_expressions_.erase(it);
    log_debug_message("Removed watch expression: " + expression);
    return true;
}

void DebuggerInterface::clear_watch_expressions() {
    watch_expressions_.clear();
    log_debug_message("Cleared all watch expressions");
}

std::vector<WatchExpression> DebuggerInterface::get_watch_expressions() const {
    return watch_expressions_;
}

void DebuggerInterface::update_watch_expressions() {
    for (auto& watch : watch_expressions_) {
        watch.value = evaluate_watch_expression(watch.expression);
        watch.valid = !watch.value.empty();
        watch.type = watch.valid ? "string" : "invalid"; // Simplified type detection
    }
}

void DebuggerInterface::set_event_callback(DebugEventCallback callback) {
    event_callback_ = callback;
}

void DebuggerInterface::set_editor(IntegratedEditor* editor) {
    editor_ = editor;
}

void DebuggerInterface::highlight_current_line() {
    if (!editor_ || !is_debugging()) {
        return;
    }
    
    auto [file_path, line] = find_line_for_pc(vm_->get_pc());
    if (!file_path.empty()) {
        // TODO: Implement editor integration for highlighting current debug line
        log_debug_message("Highlighting line " + std::to_string(line) + " in " + file_path);
    }
}

void DebuggerInterface::clear_current_line_highlight() {
    if (!editor_) {
        return;
    }
    
    // TODO: Implement editor integration for clearing debug line highlight
    log_debug_message("Cleared current line highlight");
}

void DebuggerInterface::refresh_breakpoint_markers() {
    if (!editor_) {
        return;
    }
    
    // TODO: Implement editor integration for refreshing breakpoint markers
    log_debug_message("Refreshed breakpoint markers");
}

size_t DebuggerInterface::get_program_size() const {
    return vm_->get_program_size();
}

std::string DebuggerInterface::get_instruction_at(size_t pc) const {
    return vm_->get_instruction_info(pc);
}

bool DebuggerInterface::is_valid_pc(size_t pc) const {
    return pc < get_program_size();
}

void DebuggerInterface::enable_debug_output(bool enabled) {
    debug_output_enabled_ = enabled;
}

std::vector<std::string> DebuggerInterface::get_debug_log() const {
    return debug_log_;
}

void DebuggerInterface::clear_debug_log() {
    debug_log_.clear();
}

// Private helper methods
void DebuggerInterface::transition_state(DebugState new_state, const std::string& message) {
    if (state_ != new_state) {
        state_ = new_state;
        log_debug_message("State transition to " + std::to_string(static_cast<int>(new_state)) + 
                         (message.empty() ? "" : ": " + message));
    }
}

void DebuggerInterface::fire_event(DebugEvent event, const std::string& message) {
    if (event_callback_) {
        event_callback_(event, message);
    }
}

void DebuggerInterface::log_debug_message(const std::string& message) {
    if (debug_output_enabled_) {
        debug_log_.push_back(message);
        if (debug_output_enabled_) {
            std::cout << "[DEBUG] " << message << std::endl;
        }
    }
}

void DebuggerInterface::update_breakpoint_mapping() {
    // TODO: Implement file/line to PC mapping based on debug info
    // This would require source-level debugging support in the VM
}

size_t DebuggerInterface::find_pc_for_line(const std::string& file_path, size_t line) const {
    auto it = file_line_to_pc_.find(file_path + ":" + std::to_string(line));
    return it != file_line_to_pc_.end() ? it->second : SIZE_MAX;
}

std::pair<std::string, size_t> DebuggerInterface::find_line_for_pc(size_t pc) const {
    auto it = pc_to_file_line_.find(pc);
    return it != pc_to_file_line_.end() ? it->second : std::make_pair("", 0);
}

std::string DebuggerInterface::evaluate_watch_expression(const std::string& expression) {
    // TODO: Implement expression evaluation
    // For now, return placeholder
    return "TODO: evaluate " + expression;
}

void DebuggerInterface::setup_vm_handlers() {
    // TODO: Set up VM handlers for AI integration, glyph rendering, etc.
    // This would allow the debugger to intercept and display VM operations
}

} // namespace bolt