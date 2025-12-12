# Basic test framework setup
cmake_minimum_required(VERSION 3.15)

# Find if GoogleTest is available
find_package(GTest QUIET)

if(GTest_FOUND)
    # Create test executable if GoogleTest is available
    add_executable(bolt_tests
        basic_test.cpp  # Use existing test file
    )
    
    target_link_libraries(bolt_tests 
        PRIVATE 
        bolt_lib 
        GTest::gtest_main
    )
    
    # Add widget framework tests if GoogleTest is available
    add_executable(bolt_widget_framework_tests
        test_widget_framework.cpp
    )
    
    target_link_libraries(bolt_widget_framework_tests 
        PRIVATE 
        bolt_lib 
        GTest::gtest_main
    )
    
    target_include_directories(bolt_widget_framework_tests PRIVATE
        ${PROJECT_SOURCE_DIR}/include
    )
    
    # Add tests to CTest
    include(GoogleTest)
    gtest_discover_tests(bolt_tests)
    gtest_discover_tests(bolt_widget_framework_tests)
else()
    # Create a minimal test without GoogleTest
    add_executable(bolt_basic_test
        basic_test.cpp
    )
    
    target_link_libraries(bolt_basic_test PRIVATE bolt_lib)
    
    # Add basic test to CTest
    add_test(NAME basic_bolt_test COMMAND bolt_basic_test)
endif()

# Create comprehensive test runner using our custom framework
add_executable(bolt_unit_tests
    test_runner.cpp
    test_bolt_core.cpp
    test_error_handling.cpp
    test_file_tree.cpp
    test_minimap.cpp
    test_tab_bar.cpp
    test_ai_ggml.cpp
    test_ai_models_complete.cpp
    test_debugger.cpp
    test_logging.cpp
    test_memory_leak_detector.cpp
)

target_link_libraries(bolt_unit_tests PRIVATE bolt_lib)

target_include_directories(bolt_unit_tests PRIVATE
    ${PROJECT_SOURCE_DIR}/include
)

# Create sanitizer integration test executable
add_executable(bolt_sanitizer_tests
    test_runner.cpp
    test_sanitizer_integration.cpp
)

target_link_libraries(bolt_sanitizer_tests PRIVATE bolt_lib)

target_include_directories(bolt_sanitizer_tests PRIVATE
    ${PROJECT_SOURCE_DIR}/include
)

# Create split view test executable
add_executable(bolt_split_view_tests
    test_split_view.cpp
)

target_link_libraries(bolt_split_view_tests PRIVATE bolt_lib)

target_include_directories(bolt_split_view_tests PRIVATE
    ${PROJECT_SOURCE_DIR}/include
)

# Create multi-cursor test executable
add_executable(bolt_multi_cursor_tests
    test_multi_cursor.cpp
)

target_link_libraries(bolt_multi_cursor_tests PRIVATE bolt_lib)

target_include_directories(bolt_multi_cursor_tests PRIVATE
    ${PROJECT_SOURCE_DIR}/include
)

# Create keyboard shortcuts test executable
add_executable(bolt_keyboard_shortcuts_tests
    test_keyboard_shortcuts.cpp
)

target_link_libraries(bolt_keyboard_shortcuts_tests PRIVATE bolt_lib)

target_include_directories(bolt_keyboard_shortcuts_tests PRIVATE
    ${PROJECT_SOURCE_DIR}/include
)

# Create theme system test executable
add_executable(bolt_theme_system_tests
    test_theme_system.cpp
)

target_link_libraries(bolt_theme_system_tests PRIVATE bolt_lib)

target_include_directories(bolt_theme_system_tests PRIVATE
    ${PROJECT_SOURCE_DIR}/include
)

# Create tab bar test executable
add_executable(bolt_tab_bar_tests
    test_runner.cpp
    test_tab_bar.cpp
)

target_link_libraries(bolt_tab_bar_tests PRIVATE bolt_lib)

target_include_directories(bolt_tab_bar_tests PRIVATE
    ${PROJECT_SOURCE_DIR}/include
)

# Create plugin system test executable
add_executable(bolt_plugin_system_tests
    test_plugin_system.cpp
)

target_link_libraries(bolt_plugin_system_tests PRIVATE bolt_lib)

target_include_directories(bolt_plugin_system_tests PRIVATE
    ${PROJECT_SOURCE_DIR}/include
)

# Create integration test runner
add_executable(bolt_integration_tests
    test_runner.cpp
    test_integration.cpp
)

target_link_libraries(bolt_integration_tests PRIVATE bolt_lib)

target_include_directories(bolt_integration_tests PRIVATE
    ${PROJECT_SOURCE_DIR}/include
)

# Create collaboration test
add_executable(bolt_collaboration_tests
    test_collaboration.cpp
)

target_link_libraries(bolt_collaboration_tests PRIVATE bolt_lib)

target_include_directories(bolt_collaboration_tests PRIVATE
    ${PROJECT_SOURCE_DIR}/include
)

# Apply sanitizer flags to test executables if enabled
if(SANITIZERS_ENABLED)
    add_sanitizer_flags(bolt_unit_tests)
    add_sanitizer_flags(bolt_sanitizer_tests)
    add_sanitizer_flags(bolt_split_view_tests)
    add_sanitizer_flags(bolt_multi_cursor_tests)
    add_sanitizer_flags(bolt_keyboard_shortcuts_tests)
    add_sanitizer_flags(bolt_theme_system_tests)
    add_sanitizer_flags(bolt_tab_bar_tests)
    add_sanitizer_flags(bolt_plugin_system_tests)
    add_sanitizer_flags(bolt_integration_tests)
    add_sanitizer_flags(bolt_collaboration_tests)
endif()

# Add our custom test framework tests to CTest
add_test(NAME bolt_all_tests COMMAND bolt_unit_tests)
add_test(NAME bolt_chat_tests COMMAND bolt_unit_tests Chat)
add_test(NAME bolt_memory_tests COMMAND bolt_unit_tests MemoryManager)
add_test(NAME bolt_store_tests COMMAND bolt_unit_tests ChatStore)
add_test(NAME bolt_string_tests COMMAND bolt_unit_tests StringUtils)
add_test(NAME bolt_file_tree_tests COMMAND bolt_unit_tests FileTree)
add_test(NAME bolt_minimap_tests COMMAND bolt_unit_tests Minimap)
add_test(NAME bolt_split_view_tests COMMAND bolt_split_view_tests)
add_test(NAME bolt_multi_cursor_tests COMMAND bolt_multi_cursor_tests)
add_test(NAME bolt_keyboard_shortcuts_tests COMMAND bolt_keyboard_shortcuts_tests)
add_test(NAME bolt_theme_system_tests COMMAND bolt_theme_system_tests)
add_test(NAME bolt_plugin_system_tests COMMAND bolt_plugin_system_tests)
add_test(NAME bolt_error_handling_tests COMMAND bolt_unit_tests ErrorHandling)
add_test(NAME bolt_memory_error_tests COMMAND bolt_unit_tests MemoryManagerErrors)
add_test(NAME bolt_message_error_tests COMMAND bolt_unit_tests MessageHandlerErrors)
add_test(NAME bolt_store_error_tests COMMAND bolt_unit_tests ChatStoreErrors)
add_test(NAME bolt_editor_error_tests COMMAND bolt_unit_tests EditorStoreErrors)
add_test(NAME bolt_workbench_error_tests COMMAND bolt_unit_tests WorkbenchStoreErrors)
add_test(NAME bolt_error_recovery_tests COMMAND bolt_unit_tests ErrorRecovery)
add_test(NAME bolt_ggml_tests COMMAND bolt_unit_tests GGMLTest)
add_test(NAME bolt_ai_models_tests COMMAND bolt_unit_tests AIModels)
add_test(NAME bolt_debugger_tests COMMAND bolt_unit_tests Debugger)
add_test(NAME bolt_logging_tests COMMAND bolt_unit_tests Logging)
add_test(NAME bolt_memory_leak_detector_tests COMMAND bolt_unit_tests MemoryLeakDetector)
add_test(NAME bolt_sanitizer_integration_tests COMMAND bolt_sanitizer_tests SanitizerIntegration)
add_test(NAME bolt_collaboration_tests COMMAND bolt_collaboration_tests)

# Add integration tests to CTest
add_test(NAME bolt_integration_all COMMAND bolt_integration_tests)
add_test(NAME bolt_integration_basic COMMAND bolt_integration_tests Integration)

# Add Valgrind tests if enabled
if(ENABLE_VALGRIND)
    add_valgrind_test(bolt_unit_tests bolt_unit_tests)
    add_valgrind_test(bolt_sanitizer_tests bolt_sanitizer_tests)
    add_valgrind_test(bolt_integration_tests bolt_integration_tests)
    add_valgrind_test(memory_leak_detection_example memory_leak_detection_example)
endif()

