# Summary.cmake - Build summary utilities for OpenCog
#
# This file provides macros for generating build configuration summaries

# Initialize summary variables
set(_SUMMARY_ITEMS "" CACHE INTERNAL "Summary items list")

# SUMMARY_ADD - Add an item to the build summary
macro(SUMMARY_ADD _name _value)
    set(_SUMMARY_ITEMS "${_SUMMARY_ITEMS}${_name}: ${_value}\n" CACHE INTERNAL "Summary items list")
endmacro()

# SUMMARY_SHOW - Display the build summary
macro(SUMMARY_SHOW)
    if(_SUMMARY_ITEMS)
        message(STATUS "")
        message(STATUS "========================================")
        message(STATUS "Build Configuration Summary")
        message(STATUS "========================================")
        message(STATUS "${_SUMMARY_ITEMS}")
        message(STATUS "========================================")
    endif()
endmacro()
