# Cogutil lib directory
# This directory is for pkg-config and cmake config files

# Install pkg-config file if it exists
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/cogutil.pc.in")
    configure_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/cogutil.pc.in"
        "${CMAKE_CURRENT_BINARY_DIR}/cogutil.pc"
        @ONLY
    )
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/cogutil.pc"
            DESTINATION lib/pkgconfig)
endif()
