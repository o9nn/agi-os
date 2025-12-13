#
# AtomSpace-9P
# Exposes AtomSpace hypergraph as 9P file servers
#

CMAKE_MINIMUM_REQUIRED(VERSION 3.12)
PROJECT(atomspace-9p)

# Find dependencies
find_package(InfernoKernel REQUIRED)
find_package(CogUtil REQUIRED)
find_package(AtomSpace REQUIRED)

# AtomSpace-9P sources
set(ATOMSPACE_9P_SOURCES
    atom-server.cpp
    node-server.cpp
    link-server.cpp
    truth-value-files.cpp
    attention-files.cpp
    hypergraph-namespace.cpp
    query-walker.cpp
)

# Create AtomSpace-9P library
add_library(atomspace-9p SHARED ${ATOMSPACE_9P_SOURCES})

# Set library properties
set_target_properties(atomspace-9p PROPERTIES
    VERSION 1.0.0
    SOVERSION 1
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED ON
)

# Include directories
target_include_directories(atomspace-9p PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

# Link libraries
target_link_libraries(atomspace-9p PUBLIC
    InfernoKernel::9p-protocol
    CogUtil::cogutil
    AtomSpace::atomspace
)

# Install targets
install(TARGETS atomspace-9p
    EXPORT atomspace-9p-targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    PUBLIC_HEADER DESTINATION include/opencog/atomspace-9p
)

# Export configuration
install(EXPORT atomspace-9p-targets
    FILE AtomSpace9PTargets.cmake
    NAMESPACE AtomSpace9P::
    DESTINATION lib/cmake/AtomSpace9P
)

MESSAGE(STATUS "  AtomSpace-9P configured")
