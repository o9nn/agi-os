#
# Echo Angel 9P Server for Inferno Kernel
#
CMAKE_MINIMUM_REQUIRED(VERSION 3.12)

add_library(echo_angel_9p_server STATIC
    echo_angel_9p_server.c
)

target_include_directories(echo_angel_9p_server PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/../../avatar/echo-angel/include
)

# Link against the echo_angel kernel module
if(TARGET echo_angel_kernel)
    target_link_libraries(echo_angel_9p_server echo_angel_kernel)
endif()

install(TARGETS echo_angel_9p_server
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
)

message(STATUS "  Echo Angel 9P Server configured")
