#!/usr/bin/env bash
set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

declare -A DEPS
declare -A STAGE
declare -A STATUS
declare -A BUILT

register() {
    local pkg="$1"
    local stage="$2"
    local status="$3"
    shift 3
    DEPS["$pkg"]="$*"
    STAGE["$pkg"]="$stage"
    STATUS["$pkg"]="$status"
}

init_dependencies() {
    register mig 0 realized
    register inferno-kernel 0 planned
    register cognumach 0.5 realized mig

    register cogutil 1 realized
    register ggml-tensor 1 planned
    register opennars-native 1 realized
    register webvm 1 realized

    register atomspace 2 realized cogutil
    register cognumach-cognitive-scheduler 2 planned mig cognumach cogutil
    register cogplan9 2 planned inferno-kernel
    register d81p9p9 2 planned inferno-kernel

    register atomspace-cog 3 planned atomspace
    register atomspace-rocks 3 planned atomspace atomspace-storage
    register atomspace-pgres 3 planned atomspace atomspace-storage
    register atomspace-storage 3 realized atomspace
    register cogcities-kernel 3 planned cogplan9
    register node-llama-cog 3 realized ggml-tensor

    register atomspace-9p 4 planned atomspace inferno-kernel
    register cogserver 4 realized atomspace atomspace-storage
    register ure 4 planned atomspace

    register hurdcog 4.5 realized mig cognumach cogutil atomspace
    register hurdcog-cogkernel-core 4.5 planned hurdcog
    register hurdcog-machspace 4.5 planned hurdcog
    register hurdcog-occ-bridge 4.5 planned hurdcog atomspace

    register attention 5 planned atomspace cogserver
    register cognitive-grip 5 planned cogutil atomspace atomspace-storage
    register hurdcog-atomspace-bridge 5 planned hurdcog atomspace
    register koboldcpp-cog 5 planned cogutil atomspace ggml-tensor
    register miner 5 planned atomspace ure
    register pln 5 realized atomspace ure
    register spacetime 5 planned atomspace
    register unify 5 planned atomspace

    register cogbolt 6 planned cogutil atomspace
    register das 6 planned atomspace
    register generate 6 planned atomspace ure
    register hyperon-metta 6 planned atomspace
    register learn 6 planned atomspace atomspace-rocks ure

    register aphroditecho 7 planned ggml-tensor atomspace cogserver
    register das-atomspace 7 planned das atomspace
    register deltecho 7 planned atomspace
    register lg-atomese 7 planned atomspace
    register relex 7 planned

    register agi-bio 8 planned atomspace pln ure
    register asmoses 8 planned moses atomspace
    register moses 8 planned cogutil atomspace
    register vision 8 planned atomspace

    register opencog 9 realized cogutil atomspace atomspace-storage cogserver ure pln attention miner unify spacetime learn generate lg-atomese moses asmoses agi-bio vision
    register agi-os-unified 10 planned mig cognumach hurdcog opencog inferno-kernel
}

is_built() {
    local pkg="$1"
    [[ "${BUILT[$pkg]:-0}" = "1" ]]
}

mark_built() {
    local pkg="$1"
    BUILT["$pkg"]=1
}

deps_satisfied() {
    local pkg="$1"
    local dep
    for dep in ${DEPS[$pkg]}; do
        if ! is_built "$dep"; then
            return 1
        fi
    done
    return 0
}

topo_sort() {
    local sorted=()
    local remaining=("${!DEPS[@]}")
    while [ ${#remaining[@]} -gt 0 ]; do
        local found=0
        local new_remaining=()
        local pkg
        for pkg in "${remaining[@]}"; do
            if deps_satisfied "$pkg"; then
                sorted+=("$pkg")
                mark_built "$pkg"
                found=1
            else
                new_remaining+=("$pkg")
            fi
        done
        if [ $found -eq 0 ] && [ ${#new_remaining[@]} -gt 0 ]; then
            echo -e "${RED}ERROR: Circular dependency detected!${NC}" >&2
            echo "Remaining packages: ${new_remaining[*]}" >&2
            return 1
        fi
        remaining=("${new_remaining[@]}")
    done
    printf '%s\n' "${sorted[@]}"
}

generate_build_order() {
    echo -e "${CYAN}========================================${NC}"
    echo -e "${CYAN}AGI-OS Package Build Order${NC}"
    echo -e "${CYAN}========================================${NC}"
    echo

    local current_stage=""
    local pkg
    while IFS= read -r pkg; do
        local stage="${STAGE[$pkg]}"
        local deps="${DEPS[$pkg]}"
        local status="${STATUS[$pkg]}"
        if [ "$stage" != "$current_stage" ]; then
            [ -n "$current_stage" ] && echo
            echo -e "${BLUE}Stage $stage:${NC}"
            current_stage="$stage"
        fi
        [ -z "$deps" ] && deps="(none)"
        echo -e "  ${GREEN}$pkg${NC} ${YELLOW}[$status]${NC}"
        echo -e "    Dependencies: ${YELLOW}$deps${NC}"
    done < <(topo_sort)
    echo
}

generate_makefile() {
    local output="$1"
    cat > "$output" <<'MAKEFILE'
.PHONY: all clean help
PARALLEL_JOBS ?= $(shell nproc)
all: opencog hurdcog cognumach mig
help:
@echo "AGI-OS Debian Package Build System"
@echo ""
@echo "Targets:"
@echo "  all              - Build all realized package directories"
@echo "  clean            - Clean build artifacts"
@echo "  <package>        - Build a specific package"
clean:
find . -name "*.deb" -delete
find . -name "*.changes" -delete
find . -name "*.buildinfo" -delete
MAKEFILE

    local pkg deps status
    while IFS= read -r pkg; do
        status="${STATUS[$pkg]}"
        [ "$status" != "realized" ] && continue
        deps=""
        local dep
        for dep in ${DEPS[$pkg]}; do
            [ "${STATUS[$dep]}" = "realized" ] && deps="$deps $dep"
        done
        cat >> "$output" <<EOF_RULE
$pkg:$deps
@echo "Building $pkg..."
cd $pkg && ./update-$pkg.sh && cd build/* && dpkg-buildpackage -rfakeroot -us -uc -j\$(PARALLEL_JOBS)
EOF_RULE
    done < <(topo_sort)

    echo -e "${GREEN}Makefile generated: $output${NC}"
}

generate_cmake() {
    local output="$1"
    cat > "$output" <<'EOF_CMAKE'
cmake_minimum_required(VERSION 3.16)
project(AGIOSPackages NONE)
include(ExternalProject)
EOF_CMAKE

    local pkg status dep dep_targets
    while IFS= read -r pkg; do
        status="${STATUS[$pkg]}"
        [ "$status" != "realized" ] && continue
        dep_targets=""
        for dep in ${DEPS[$pkg]}; do
            if [ "${STATUS[$dep]}" = "realized" ]; then
                dep_targets="$dep_targets $dep"
            fi
        done
        cat >> "$output" <<EOF_ENTRY
ExternalProject_Add($pkg
    SOURCE_DIR \${CMAKE_CURRENT_SOURCE_DIR}/$pkg
    CONFIGURE_COMMAND ./update-$pkg.sh
    BUILD_COMMAND bash -lc 'cd build/* && dpkg-buildpackage -rfakeroot -us -uc'
    BUILD_IN_SOURCE 1
    DEPENDS$dep_targets
)
EOF_ENTRY
    done < <(topo_sort)

    echo -e "${GREEN}CMakeLists.txt generated: $output${NC}"
}

visualize_graph() {
    local output="$1"
    cat > "$output" <<'EOF_DOT'
digraph AGIOSDependencies {
    rankdir=TB;
    node [shape=box, style=filled, fillcolor=lightblue];
EOF_DOT

    local pkg dep stage color deps status
    for pkg in "${!DEPS[@]}"; do
        stage="${STAGE[$pkg]}"
        deps="${DEPS[$pkg]}"
        status="${STATUS[$pkg]}"
        color="lightblue"
        case "$stage" in
            0|0.5) color="lightcoral" ;;
            1) color="lightgreen" ;;
            2) color="lightyellow" ;;
            3) color="lightcyan" ;;
            4|4.5) color="lavender" ;;
            5) color="peachpuff" ;;
            6) color="lightpink" ;;
            7) color="lightgray" ;;
            8) color="lightsalmon" ;;
            9) color="lightsteelblue" ;;
            10) color="gold" ;;
        esac
        echo "    \"$pkg\" [fillcolor=$color, label=\"$pkg\\nStage $stage\\n$status\"];" >> "$output"
        for dep in $deps; do
            echo "    \"$dep\" -> \"$pkg\";" >> "$output"
        done
    done
    echo "}" >> "$output"
    echo -e "${GREEN}Dependency graph generated: $output${NC}"
}

check_missing() {
    echo -e "${CYAN}Checking realized package directories...${NC}"
    local missing=()
    local planned_missing=()
    local pkg
    for pkg in "${!DEPS[@]}"; do
        if [ ! -d "$pkg" ]; then
            if [ "${STATUS[$pkg]}" = "realized" ]; then
                missing+=("$pkg")
            else
                planned_missing+=("$pkg")
            fi
        fi
    done
    if [ ${#missing[@]} -gt 0 ]; then
        echo -e "${RED}Missing realized package directories:${NC}"
        local item
        for item in "${missing[@]}"; do
            echo -e "  ${RED}✗${NC} $item"
        done
        return 1
    fi
    echo -e "${GREEN}All realized package directories are present${NC}"
    if [ ${#planned_missing[@]} -gt 0 ]; then
        echo -e "${YELLOW}Planned package directories not yet materialized:${NC}"
        local item
        for item in "${planned_missing[@]}"; do
            echo -e "  ${YELLOW}•${NC} $item"
        done
    fi
}

main() {
    local action="${1:-order}"
    init_dependencies
    case "$action" in
        order) generate_build_order ;;
        makefile) generate_makefile Makefile.packages ;;
        cmake) generate_cmake CMakeLists.packages.txt ;;
        graph) visualize_graph dependencies.dot ;;
        check) check_missing ;;
        *)
            echo "Usage: $0 {order|makefile|cmake|graph|check}" >&2
            exit 1
            ;;
    esac
}

main "$@"
