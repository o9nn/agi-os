# OpenCogLibOptions.cmake - Library build options for OpenCog
#
# This file sets up library build options

# Build shared libraries by default
option(BUILD_SHARED_LIBS "Build shared libraries" ON)

# RPATH handling for installed libraries
if(UNIX)
    # Use RPATH for installed binaries
    set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
    
    # Add the automatically determined parts of the RPATH
    # which point to directories outside the build tree to the install RPATH
    set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
    
    # Don't skip the full RPATH for the build tree
    set(CMAKE_SKIP_BUILD_RPATH FALSE)
    
    # When building, don't use the install RPATH already
    set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
endif()

# Library versioning
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
