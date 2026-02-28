#!/bin/bash
# AGI-OS Component Path Mapping
# Source this script to get correct paths for all subsystem components.
# Usage: source .github/scripts/component-paths.sh

# Layer 0: Build Tools
export MIG_PATH="core/microkernel/cognumach/mig"
export INFERNO_KERNEL_PATH="core/inferno-kernel"

# Layer 1: Microkernel
export COGNUMACH_PATH="core/microkernel/cognumach"

# Layer 2: Operating System
export HURDCOG_PATH="core/os/hurdcog"

# Layer 3: OpenCog Foundation
export COGUTIL_PATH="core/cognition/foundation/cogutil"
export ATOMSPACE_PATH="core/cognition/foundation/atomspace"
export ATOMSPACE_STORAGE_PATH="core/cognition/foundation/atomspace-storage"
export COGSERVER_PATH="core/cognition/foundation/cogserver"
export ATTENTION_PATH="core/cognition/foundation/attention"
export LEARN_PATH="core/cognition/foundation/learn"
export AGENTS_PATH="core/cognition/foundation/agents"
export ASMOSES_PATH="core/cognition/foundation/asmoses"

# Layer 3: Reasoning
export URE_PATH="core/cognition/reasoning/ure"
export PLN_PATH="core/cognition/reasoning/pln"
export UNIFY_PATH="core/cognition/reasoning/unify"
export SPACETIME_PATH="core/cognition/reasoning/spacetime"

# Layer 3: Learning
export MINER_PATH="core/cognition/learning/miner"
export MOSES_PATH="core/cognition/learning/moses"

# Layer 3: Language
export LG_ATOMESE_PATH="core/cognition/language/lg-atomese"
export LINK_GRAMMAR_PATH="core/cognition/language/link-grammar"
export RELEX_PATH="core/cognition/language/relex"

# Layer 3: Storage
export ATOMSPACE_ROCKS_PATH="core/cognition/storage/atomspace-rocks"
export ATOMSPACE_COG_PATH="core/cognition/storage/atomspace-cog"
export ATOMSPACE_PGRES_PATH="core/cognition/storage/atomspace-pgres"
export ATOMSPACE_ACCELERATOR_PATH="core/cognition/storage/atomspace-accelerator"

# Layer 3: Generation
export GENERATE_PATH="core/cognition/generation/generate"

# Layer 3.7: LLM
export KOBOLDCPP_COG_PATH="core/cognition/llm/koboldcpp-cog"
export APHRODITECHO_PATH="core/cognition/llm/aphroditecho"
export NODE_LLAMA_COG_PATH="core/cognition/llm/node-llama-cog"

# Layer 4: IDE
export COGBOLT_PATH="cogbolt"

# Integration
export COGNITIVE_GRIP_PATH="core/integration/cognitive-grip"

# Packaging
export DEBIAN_PACKAGING_PATH="infrastructure/packaging/debian"

# Helper function: resolve component path
resolve_component() {
    local component="$1"
    case "$component" in
        cogutil)            echo "$COGUTIL_PATH" ;;
        atomspace)          echo "$ATOMSPACE_PATH" ;;
        atomspace-storage)  echo "$ATOMSPACE_STORAGE_PATH" ;;
        cogserver)          echo "$COGSERVER_PATH" ;;
        attention)          echo "$ATTENTION_PATH" ;;
        learn)              echo "$LEARN_PATH" ;;
        agents)             echo "$AGENTS_PATH" ;;
        asmoses)            echo "$ASMOSES_PATH" ;;
        ure)                echo "$URE_PATH" ;;
        pln)                echo "$PLN_PATH" ;;
        unify)              echo "$UNIFY_PATH" ;;
        miner)              echo "$MINER_PATH" ;;
        cognumach)          echo "$COGNUMACH_PATH" ;;
        hurdcog)            echo "$HURDCOG_PATH" ;;
        mig)                echo "$MIG_PATH" ;;
        koboldcpp-cog)      echo "$KOBOLDCPP_COG_PATH" ;;
        cogbolt)            echo "$COGBOLT_PATH" ;;
        cognitive-grip)     echo "$COGNITIVE_GRIP_PATH" ;;
        *)                  echo "" ;;
    esac
}

# Helper: build a cmake component
build_cmake_component() {
    local name="$1"
    local path="$2"
    local extra_cmake_args="${3:-}"

    if [ -d "$path" ] && [ -f "$path/CMakeLists.txt" ]; then
        echo "=== Building $name from $path ==="
        mkdir -p "$path/build"
        cd "$path/build"
        cmake .. -DCMAKE_BUILD_TYPE=${BUILD_TYPE:-Release} $extra_cmake_args
        make ${MAKEFLAGS:--j$(nproc)}
        sudo make install
        sudo ldconfig
        cd "$GITHUB_WORKSPACE" 2>/dev/null || cd -
        echo "$name build complete"
        return 0
    else
        echo "WARNING: $name not found at $path"
        return 1
    fi
}

export -f resolve_component
export -f build_cmake_component
