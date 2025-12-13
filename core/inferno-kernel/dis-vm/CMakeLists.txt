#
# Dis Virtual Machine
# Bytecode interpreter for Limbo language
#

CMAKE_MINIMUM_REQUIRED(VERSION 3.12)

# Dis VM sources
set(DIS_VM_SOURCES
    vm-core.c
    bytecode-loader.c
    instruction-dispatch.c
    memory-manager.c
    garbage-collector.c
    module-loader.c
    type-system.c
)

# Dis VM headers
set(DIS_VM_HEADERS
    ../include/dis-vm.h
    ../include/dis-bytecode.h
    ../include/dis-types.h
)

# Create Dis VM library
add_library(dis-vm SHARED ${DIS_VM_SOURCES})
add_library(dis-vm-static STATIC ${DIS_VM_SOURCES})

# Set library properties
set_target_properties(dis-vm PROPERTIES
    VERSION 1.0.0
    SOVERSION 1
    PUBLIC_HEADER "${DIS_VM_HEADERS}"
)

set_target_properties(dis-vm-static PROPERTIES
    OUTPUT_NAME dis-vm
)

# Include directories
target_include_directories(dis-vm PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include>
    $<INSTALL_INTERFACE:include>
)

target_include_directories(dis-vm-static PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include>
    $<INSTALL_INTERFACE:include>
)

# Link libraries
target_link_libraries(dis-vm PRIVATE pthread m)
target_link_libraries(dis-vm-static PRIVATE pthread m)

# Install targets
install(TARGETS dis-vm dis-vm-static
    EXPORT inferno-kernel-targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    PUBLIC_HEADER DESTINATION include/inferno-kernel
)

MESSAGE(STATUS "  Dis VM configured")
