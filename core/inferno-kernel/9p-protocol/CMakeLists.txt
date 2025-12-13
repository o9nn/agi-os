#
# 9P Protocol Stack
# File protocol for distributed systems
#

CMAKE_MINIMUM_REQUIRED(VERSION 3.12)

# 9P Protocol sources
set(9P_PROTOCOL_SOURCES
    9p-server.c
    9p-client.c
    9p-messages.c
    9p-marshal.c
    9p-unmarshal.c
    styx-protocol.c
    file-operations.c
    directory-operations.c
)

# 9P Protocol headers
set(9P_PROTOCOL_HEADERS
    ../include/9p-protocol.h
    ../include/9p-types.h
    ../include/styx.h
)

# Create 9P Protocol library
add_library(9p-protocol SHARED ${9P_PROTOCOL_SOURCES})
add_library(9p-protocol-static STATIC ${9P_PROTOCOL_SOURCES})

# Set library properties
set_target_properties(9p-protocol PROPERTIES
    VERSION 1.0.0
    SOVERSION 1
    PUBLIC_HEADER "${9P_PROTOCOL_HEADERS}"
)

set_target_properties(9p-protocol-static PROPERTIES
    OUTPUT_NAME 9p-protocol
)

# Include directories
target_include_directories(9p-protocol PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include>
    $<INSTALL_INTERFACE:include>
)

target_include_directories(9p-protocol-static PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include>
    $<INSTALL_INTERFACE:include>
)

# Link libraries
target_link_libraries(9p-protocol PRIVATE pthread)
target_link_libraries(9p-protocol-static PRIVATE pthread)

# Install targets
install(TARGETS 9p-protocol 9p-protocol-static
    EXPORT inferno-kernel-targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    PUBLIC_HEADER DESTINATION include/inferno-kernel
)

MESSAGE(STATUS "  9P Protocol configured")
