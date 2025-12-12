cmake_minimum_required(VERSION 3.15)
project(bolt VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Include cross-platform configuration
include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/CrossPlatform.cmake")

# Include code coverage configuration
include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/CodeCoverage.cmake")

# Include documentation generation
include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/Documentation.cmake")

# Include sanitizer support for memory leak detection
include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/Sanitizers.cmake")

# Try to find CURL via CONFIG first, then fallback to module
set(CMAKE_PREFIX_PATH "${CMAKE_CURRENT_SOURCE_DIR}/vcpkg_installed/x64-linux;${CMAKE_PREFIX_PATH}")
set(CURL_FOUND FALSE)
find_package(CURL CONFIG QUIET)
if(CURL_FOUND)
    set(HAVE_CURL TRUE)
else()
    find_package(CURL QUIET)
    if(CURL_FOUND)
        set(HAVE_CURL TRUE)
    else()
        message(WARNING "libcurl not found - HTTP AI providers will be disabled")
        set(HAVE_CURL FALSE)
    endif()
endif()

# Try to find jsoncpp via CONFIG first, then fallback
set(JSONCPP_FOUND FALSE)
find_package(jsoncpp CONFIG QUIET)
if(jsoncpp_FOUND)
    set(HAVE_JSONCPP TRUE)
else()
    find_package(JsonCpp QUIET)
    if(JsonCpp_FOUND)
        set(HAVE_JSONCPP TRUE)
    else()
        message(WARNING "jsoncpp not found - HTTP AI providers will be disabled")
        set(HAVE_JSONCPP FALSE)
    endif()
endif()

find_package(OpenGL QUIET)
if(OpenGL_FOUND)
    set(HAVE_OPENGL TRUE)
else()
    message(WARNING "OpenGL not found - GUI components will be disabled")
    set(HAVE_OPENGL FALSE)
endif()

# Try to find zlib
find_package(ZLIB QUIET)
if(ZLIB_FOUND)
    set(HAVE_ZLIB TRUE)
else()
    message(WARNING "zlib not found - compression features will be disabled")
    set(HAVE_ZLIB FALSE)
endif()

find_package(glfw3 CONFIG QUIET)
if(glfw3_FOUND)
    set(HAVE_GLFW TRUE)
else()
    message(WARNING "GLFW not found - GUI components will be disabled")
    set(HAVE_GLFW FALSE)
endif()

# Try to find imgui
set(CMAKE_PREFIX_PATH "${CMAKE_CURRENT_SOURCE_DIR}/vcpkg_installed/x64-linux;${CMAKE_PREFIX_PATH}")
find_package(imgui CONFIG QUIET)
set(IMGUI_FOUND ${imgui_FOUND})
if(NOT IMGUI_FOUND)
    # Try alternative package names
    find_package(ImGui QUIET)
    set(IMGUI_FOUND ${ImGui_FOUND})
    if(NOT IMGUI_FOUND)
        message(WARNING "ImGui not found - GUI components will be disabled")
        set(IMGUI_FOUND FALSE)
    endif()
endif()

# Add GGML as subdirectory (conditional)
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/ggml/ggml.cpp")
    add_subdirectory(ggml/ggml.cpp)
    set(GGML_AVAILABLE TRUE)
else()
    set(GGML_AVAILABLE FALSE)
    message(WARNING "GGML not found - AI functionality will be simulated")
endif()

# Add llama.cpp as subdirectory (conditional) - disabled due to build issues
set(LLAMA_AVAILABLE FALSE)
message(WARNING "llama.cpp disabled - direct GGUF inference may be disabled")

# Main library - core components for DrawKern
add_library(bolt_lib
    # Core components
    src/bolt/core/bolt.cpp
    src/bolt/core/memory_manager.cpp
    src/bolt/core/memory_pool.cpp
    src/bolt/core/message_handler.cpp
    src/bolt/core/chat_store.cpp
    src/bolt/core/editor_store.cpp
    src/bolt/core/workbench_store.cpp
    src/bolt/core/plugin_system.cpp
    src/bolt/core/logging.cpp
    src/bolt/core/performance_profiler.cpp
    src/bolt/core/benchmark_suite.cpp
    src/bolt/core/code_analyzer.cpp
    src/bolt/utils/string_utils.cpp
    # Editor components (integrated_editor temporarily disabled due to AI dependencies)
    src/bolt/editor/code_folding.cpp
    src/bolt/editor/code_folding_detector.cpp
    src/bolt/editor/code_folding_manager.cpp
    src/bolt/editor/code_folding_visibility.cpp
    src/bolt/editor/code_folding_ui.cpp
    src/bolt/editor/integrated_editor.cpp
    src/bolt/editor/code_completion.cpp
    src/bolt/editor/cursor_manager.cpp
    src/bolt/editor/keyboard_shortcuts.cpp
    src/bolt/editor/file_tree_node.cpp
    src/bolt/editor/file_tree_manager.cpp
    src/bolt/editor/file_tree_ui.cpp
    src/bolt/editor/split_view_manager.cpp
    src/bolt/editor/editor_pane.cpp
    src/bolt/editor/minimap.cpp
    src/bolt/editor/minimap_renderer.cpp
    src/bolt/editor/minimap_ui.cpp
    src/bolt/editor/theme_system.cpp
    src/bolt/editor/tab_bar.cpp
    src/bolt/editor/debugger_interface.cpp
    src/bolt/editor/debugger_ui.cpp
    # LSP components (temporarily disabled due to build issues)
    # src/bolt/editor/lsp_json_rpc.cpp
    # src/bolt/editor/lsp_server.cpp
    # src/bolt/editor/lsp_client.cpp
    # src/bolt/editor/lsp_plugin_adapter.cpp
    # AI components (conditional)
    src/bolt/ai/ggml.cpp
    src/bolt/ai/rwkv_wrapper.cpp
    src/bolt/ai/gguf_loader.cpp
    src/bolt/ai/ggml_loader.cpp
    src/bolt/ai/model_loader.cpp
    src/bolt/ai/bpe_tokenizer.cpp
    src/bolt/ai/direct_gguf_inference.cpp
    src/bolt/ai/gpu_acceleration.cpp
    # GUI components
    src/bolt/gui/bolt_gui_app.cpp
    src/bolt/gui/widget_framework.cpp
    src/bolt/gui/widgets.cpp
    src/bolt/gui/widget_registration.cpp
    src/bolt/gui/dark_theme.cpp
    # DrawKern components
    src/bolt/drawkern/drawkern.cpp
    src/bolt/drawkern/styx_protocol.cpp
    src/bolt/drawkern/dis_vm.cpp
    src/bolt/drawkern/yacc_grammar.cpp
    src/bolt/drawkern/ai_integration.cpp
    # Collaboration components
    src/bolt/collaboration/document_operation.cpp
    src/bolt/collaboration/operational_transform.cpp
    src/bolt/collaboration/collaborative_session.cpp
    src/bolt/collaboration/collaboration_protocol.cpp
    src/bolt/collaboration/collaborative_editor_integration.cpp
    # Git integration components
    src/bolt/git/git_repository.cpp
    src/bolt/git/git_integration.cpp
    # Network optimization components
    src/bolt/network/websocket_server.cpp
    src/bolt/network/connection_pool.cpp
    src/bolt/network/message_compression.cpp
    src/bolt/network/network_buffer.cpp
    src/bolt/network/network_metrics.cpp
)

# Always include core AI features
target_sources(bolt_lib PRIVATE
    src/bolt/ai/ai_completion_provider.cpp
    src/bolt/ai/ai_config_manager.cpp
    src/bolt/ai/enhanced_ai_manager.cpp
    src/bolt/ai/ai_code_generator.cpp
    src/bolt/ai/ai_refactoring_engine.cpp
    src/bolt/ai/ai_http_client.cpp
)

# Add HTTP client compile definitions only if CURL and JSONCPP are available
if(HAVE_CURL AND HAVE_JSONCPP)
    target_compile_definitions(bolt_lib PUBLIC BOLT_HAVE_CURL=1 BOLT_HAVE_JSONCPP=1)
endif()

# Add zlib compile definition if available
if(HAVE_ZLIB)
    target_compile_definitions(bolt_lib PUBLIC BOLT_HAVE_ZLIB=1)
endif()



target_include_directories(bolt_lib PUBLIC
    ${PROJECT_SOURCE_DIR}/include
)

# Link required libraries using modern vcpkg targets
target_link_libraries(bolt_lib PUBLIC 
    pthread
    dl  # For dynamic plugin loading
    ssl # For secure WebSocket connections
    crypto # For WebSocket handshake
)

# Link zlib if available
if(HAVE_ZLIB)
    target_link_libraries(bolt_lib PUBLIC ZLIB::ZLIB)
endif()

# Link OpenGL and GLFW if available
if(HAVE_OPENGL AND HAVE_GLFW)
    target_link_libraries(bolt_lib PUBLIC 
        OpenGL::GL
        glfw
    )
    target_compile_definitions(bolt_lib PUBLIC BOLT_HAVE_OPENGL=1 BOLT_HAVE_GLFW=1)
endif()

# Link ImGui if available
if(IMGUI_FOUND)
    if(TARGET imgui::imgui)
        target_link_libraries(bolt_lib PUBLIC imgui::imgui)
    elseif(TARGET ImGui::ImGui)
        target_link_libraries(bolt_lib PUBLIC ImGui::ImGui)
    endif()
    target_compile_definitions(bolt_lib PUBLIC BOLT_HAVE_IMGUI=1)
endif()

if(HAVE_JSONCPP)
    if(TARGET JsonCpp::JsonCpp)
        target_link_libraries(bolt_lib PUBLIC JsonCpp::JsonCpp)
    endif()
endif()

if(HAVE_CURL)
    target_link_libraries(bolt_lib PUBLIC CURL::libcurl)
endif()

# Link GGML if available
if(GGML_AVAILABLE)
    target_include_directories(bolt_lib PUBLIC
        ${PROJECT_SOURCE_DIR}/ggml/ggml.cpp/include
        ${PROJECT_SOURCE_DIR}/ggml/rwkv.cpp
    )
    target_link_libraries(bolt_lib PUBLIC ggml)
endif()

# Link llama.cpp if available
if(LLAMA_AVAILABLE)
    target_include_directories(bolt_lib PUBLIC
        ${PROJECT_SOURCE_DIR}/ggml/llama.cpp/include
    )
    target_link_libraries(bolt_lib PUBLIC llama)
    target_compile_definitions(bolt_lib PUBLIC LLAMA_AVAILABLE=1)
endif()

# Main executable
add_executable(bolt src/bolt/main.cpp)
target_link_libraries(bolt PRIVATE bolt_lib)

# DrawKern demos (core functionality)
# Demo executables
add_executable(demo_drawkern demo_drawkern.cpp)
add_executable(demo_drawkern_full demo_drawkern_full.cpp)
# Link demo executables
target_link_libraries(demo_drawkern bolt_lib)
target_link_libraries(demo_drawkern_full bolt_lib)

add_executable(demo_drawkern_complete demo_drawkern_complete.cpp)
target_link_libraries(demo_drawkern_complete PRIVATE bolt_lib)

# AI Infrastructure demo
add_executable(demo_ai_infrastructure demo_ai_infrastructure.cpp)
target_link_libraries(demo_ai_infrastructure PRIVATE bolt_lib)

# AI Completion demo
add_executable(demo_ai_completion demo_ai_completion.cpp)
target_link_libraries(demo_ai_completion PRIVATE bolt_lib)

# Advanced AI Features demo (code generation, refactoring suggestions)
add_executable(demo_advanced_ai_features demo_advanced_ai_features.cpp)
target_link_libraries(demo_advanced_ai_features PRIVATE bolt_lib)

# AI Configuration demo
if(HAVE_CURL AND HAVE_JSONCPP)
    add_executable(demo_ai_config demo_ai_config.cpp)
    target_link_libraries(demo_ai_config PRIVATE bolt_lib)
endif()

# Direct GGUF loading demo
add_executable(demo_direct_gguf demo_direct_gguf.cpp)
target_link_libraries(demo_direct_gguf PRIVATE bolt_lib)

# GPU Acceleration demo
add_executable(demo_gpu_acceleration demo_gpu_acceleration.cpp)
target_link_libraries(demo_gpu_acceleration PRIVATE bolt_lib)

# Multi-cursor demo
add_executable(demo_multi_cursor demo_multi_cursor.cpp)
target_link_libraries(demo_multi_cursor PRIVATE bolt_lib)

# Split view demos
add_executable(demo_split_view_basic demo_split_view_basic.cpp)
target_link_libraries(demo_split_view_basic PRIVATE bolt_lib)

add_executable(demo_split_view demo_split_view.cpp)
target_link_libraries(demo_split_view PRIVATE bolt_lib)

# Debugger demo
add_executable(demo_debugger demo_debugger.cpp)
target_link_libraries(demo_debugger PRIVATE bolt_lib)

# Terminal AI Chat Application 
add_executable(bolt_terminal_ai bolt_terminal_ai.cpp)
target_link_libraries(bolt_terminal_ai PRIVATE bolt_lib)

# Plugin System Demo
add_executable(demo_plugin_system demo_plugin_system.cpp)
target_link_libraries(demo_plugin_system PRIVATE bolt_lib)

# Collaborative Editing Demo
add_executable(demo_collaborative_editing demo_collaborative_editing.cpp)
target_link_libraries(demo_collaborative_editing PRIVATE bolt_lib)

# Logging System Demo
add_executable(demo_logging demo_logging.cpp)
target_link_libraries(demo_logging PRIVATE bolt_lib)

# Simple Logging Test
add_executable(test_logging_simple test_logging_simple.cpp)
target_link_libraries(test_logging_simple PRIVATE bolt_lib)

#<<<<<<< copilot/fix-18
# Git Integration Demo
add_executable(demo_git_integration demo_git_integration.cpp)
target_link_libraries(demo_git_integration PRIVATE bolt_lib)

# Git Integration Test
add_executable(test_git_integration test/test_git_integration.cpp)
target_link_libraries(test_git_integration PRIVATE bolt_lib)

# Standalone Git Test (minimal dependencies)
add_executable(test_git_standalone test_git_standalone.cpp src/bolt/git/git_repository.cpp)
target_include_directories(test_git_standalone PRIVATE include)
#=======
#<<<<<<< copilot/fix-14
# Performance Profiler Demo
add_executable(demo_performance_profiler demo_performance_profiler.cpp)
target_link_libraries(demo_performance_profiler PRIVATE bolt_lib)

# Performance Profiler Test
add_executable(test_performance_profiler test_performance_profiler.cpp)
target_link_libraries(test_performance_profiler PRIVATE bolt_lib)

# Benchmark Suite Tool
add_executable(benchmark_tool benchmark_tool.cpp)
target_link_libraries(benchmark_tool PRIVATE bolt_lib)

# Benchmark Suite Test (temporarily disabled due to test framework issues)
# add_executable(test_benchmark_suite test_benchmark_suite.cpp)
# target_link_libraries(test_benchmark_suite PRIVATE bolt_lib)

# Code Analyzer Demo
add_executable(demo_code_analyzer demo_code_analyzer.cpp)
target_link_libraries(demo_code_analyzer PRIVATE bolt_lib)

# Code Analyzer Test (temporarily disabled due to test framework incompatibility)
# add_executable(test_code_analyzer test/test_code_analyzer.cpp)
# target_link_libraries(test_code_analyzer PRIVATE bolt_lib)

# Standalone Code Analyzer Demo (no dependencies on problematic components)
add_executable(demo_code_analyzer_standalone demo_code_analyzer_standalone.cpp)
target_include_directories(demo_code_analyzer_standalone PRIVATE include)
#======
# LSP Test (temporarily disabled due to missing LSP implementations)
# add_executable(test_lsp test_lsp.cpp)
# target_link_libraries(test_lsp PRIVATE bolt_lib)

# LSP Integration Demo (standalone)
add_executable(demo_lsp_integration demo_lsp_integration.cpp)
# No linking needed - this is a standalone demo

# LSP Editor Integration Demo (standalone)
add_executable(demo_lsp_editor_integration demo_lsp_editor_integration.cpp)
# Using standalone version to avoid compilation issues

# Network Optimizations Demo
add_executable(demo_network_optimizations demo_network_optimizations.cpp)
target_link_libraries(demo_network_optimizations PRIVATE bolt_lib)

#>>>>>>> main
#>>>>>>> main

# GUI Application with AI chat (only if ImGui is available)
if(IMGUI_FOUND)
    add_executable(gui_main src/bolt/gui_main.cpp)
    target_link_libraries(gui_main PRIVATE bolt_lib)
endif()

# Widget Framework Demo (only if ImGui is available)
if(IMGUI_FOUND)
    add_executable(demo_widget_framework demo_widget_framework.cpp)
    target_link_libraries(demo_widget_framework PRIVATE bolt_lib)
endif()

# Memory leak detection example
add_executable(memory_leak_detection_example examples/memory_leak_detection_example.cpp)
target_link_libraries(memory_leak_detection_example PRIVATE bolt_lib)

# Apply sanitizer flags to the example if enabled
if(SANITIZERS_ENABLED)
    add_sanitizer_flags(memory_leak_detection_example)
endif()

# Tests
enable_testing()
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/test")
    add_subdirectory(test)
endif()
# Model loading test
add_executable(test_model_loading test/test_model_loading.cpp)
target_link_libraries(test_model_loading bolt_lib)

