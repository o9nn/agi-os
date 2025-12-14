#!/bin/bash
# Script to propagate cmake helper files to all OpenCog components

REPO_ROOT="/home/ubuntu/agi-os"
CMAKE_HELPERS=(
    "Summary.cmake"
    "OpenCogGccOptions.cmake"
    "OpenCogLibOptions.cmake"
    "OpenCogInstallOptions.cmake"
)

# Find all cmake directories in core/cognition
find "$REPO_ROOT/core/cognition" -type d -name "cmake" | while read cmake_dir; do
    echo "Processing: $cmake_dir"
    
    # Copy each helper file if it doesn't exist
    for helper in "${CMAKE_HELPERS[@]}"; do
        if [ ! -f "$cmake_dir/$helper" ]; then
            # Try to copy from atomspace first
            if [ -f "$REPO_ROOT/core/cognition/foundation/atomspace/cmake/$helper" ]; then
                cp "$REPO_ROOT/core/cognition/foundation/atomspace/cmake/$helper" "$cmake_dir/"
                echo "  Copied $helper"
            fi
        fi
    done
done

echo "CMake helper propagation complete!"
