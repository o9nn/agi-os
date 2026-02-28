#!/bin/bash
set -e
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'
declare -A DEPS
declare -A STAGE
declare -A BUILT
init_dependencies() {
DEPS[cognumach]=""
STAGE[cognumach]=0
DEPS[cogutil]=""
STAGE[cogutil]=1
DEPS[atomspace]="cogutil"
STAGE[atomspace]=2
DEPS[atomspace-cog]="atomspace"
STAGE[atomspace-cog]=3
DEPS[atomspace-rocks]="atomspace"
STAGE[atomspace-rocks]=3
DEPS[atomspace-pgres]="atomspace"
STAGE[atomspace-pgres]=3
DEPS[cogserver]="atomspace"
STAGE[cogserver]=4
DEPS[ure]="atomspace"
STAGE[ure]=4
DEPS[hurdcog]="cognumach cogutil atomspace"
STAGE[hurdcog]=4.5
DEPS[hurdcog-cogkernel-core]="hurdcog"
STAGE[hurdcog-cogkernel-core]=4.5
DEPS[hurdcog-machspace]="hurdcog"
STAGE[hurdcog-machspace]=4.5
DEPS[hurdcog-occ-bridge]="hurdcog atomspace"
STAGE[hurdcog-occ-bridge]=4.5
DEPS[attention]="atomspace cogserver"
STAGE[attention]=5
DEPS[pln]="atomspace ure"
STAGE[pln]=5
DEPS[miner]="atomspace ure"
STAGE[miner]=5
DEPS[unify]="atomspace"
STAGE[unify]=5
DEPS[spacetime]="atomspace"
STAGE[spacetime]=5
DEPS[learn]="atomspace atomspace-rocks ure"
STAGE[learn]=6
DEPS[generate]="atomspace ure"
STAGE[generate]=6
DEPS[lg-atomese]="atomspace"
STAGE[lg-atomese]=7
DEPS[relex]=""
STAGE[relex]=7
DEPS[moses]="cogutil atomspace"
STAGE[moses]=8
DEPS[asmoses]="moses atomspace"
STAGE[asmoses]=8
DEPS[agi-bio]="atomspace pln ure"
STAGE[agi-bio]=8
DEPS[vision]="atomspace"
STAGE[vision]=8
DEPS[opencog]="cogutil atomspace cogserver ure pln attention miner unify spacetime learn generate lg-atomese moses asmoses agi-bio vision"
STAGE[opencog]=9
# Layer 0: Inferno Kernel
DEPS[inferno-kernel]=""
STAGE[inferno-kernel]=0

# Cognitive extensions
DEPS[atomspace-storage]="atomspace"
STAGE[atomspace-storage]=3
DEPS[atomspace-9p]="atomspace inferno-kernel"
STAGE[atomspace-9p]=4
DEPS[cognitive-grip]="cogutil atomspace atomspace-storage"
STAGE[cognitive-grip]=5
DEPS[cogbolt]="cogutil atomspace"
STAGE[cogbolt]=6
DEPS[hurdcog-atomspace-bridge]="hurdcog atomspace"
STAGE[hurdcog-atomspace-bridge]=5
DEPS[cognumach-cognitive-scheduler]="cognumach cogutil"
STAGE[cognumach-cognitive-scheduler]=2

# LLM and AI components
DEPS[ggml-tensor]=""
STAGE[ggml-tensor]=1
DEPS[aphroditecho]="ggml-tensor atomspace"
STAGE[aphroditecho]=7
DEPS[node-llama-cog]="ggml-tensor"
STAGE[node-llama-cog]=3
DEPS[deltecho]="atomspace"
STAGE[deltecho]=7
DEPS[koboldcpp-cog]="cogutil atomspace ggml-tensor"
STAGE[koboldcpp-cog]=5

# Distributed and external
DEPS[das]="atomspace"
STAGE[das]=6
DEPS[das-atomspace]="das atomspace"
STAGE[das-atomspace]=7
DEPS[hyperon-metta]="atomspace"
STAGE[hyperon-metta]=6
DEPS[opennars-native]=""
STAGE[opennars-native]=1

# Plan 9 and kernel
DEPS[cogplan9]="inferno-kernel"
STAGE[cogplan9]=2
DEPS[cogcities-kernel]="cogplan9"
STAGE[cogcities-kernel]=3
DEPS[d81p9p9]="inferno-kernel"
STAGE[d81p9p9]=2
DEPS[webvm]=""
STAGE[webvm]=1

# Unified
DEPS[agi-os-unified]="cognumach hurdcog opencog inferno-kernel"
STAGE[agi-os-unified]=10
}
is_built() {
local pkg=$1
[ "${BUILT[$pkg]}" = "1" ]
}
mark_built() {
local pkg=$1
BUILT[$pkg]=1
}
deps_satisfied() {
local pkg=$1
local deps="${DEPS[$pkg]}"
if [ -z "$deps" ]; then
return 0
fi
for dep in $deps; do
if ! is_built "$dep"; then
return 1
fi
done
return 0
}
get_stage_packages() {
local stage=$1
local packages=()
for pkg in "${!STAGE[@]}"; do
if [ "${STAGE[$pkg]}" = "$stage" ]; then
packages+=("$pkg")
fi
done
echo "${packages[@]}"
}
topo_sort() {
local sorted=()
local remaining=("${!DEPS[@]}")
while [ ${#remaining[@]} -gt 0 ]; do
local found=0
local new_remaining=()
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
echo "Remaining packages: ${new_remaining[@]}" >&2
return 1
fi
remaining=("${new_remaining[@]}")
done
echo "${sorted[@]}"
}
generate_build_order() {
echo -e "${CYAN}========================================${NC}"
echo -e "${CYAN}OpenCog Package Build Order${NC}"
echo -e "${CYAN}========================================${NC}"
echo ""
local current_stage=""
local stage_packages=()
for pkg in $(topo_sort); do
local stage="${STAGE[$pkg]}"
if [ "$stage" != "$current_stage" ]; then
if [ -n "$current_stage" ]; then
echo ""
fi
echo -e "${BLUE}Stage $stage:${NC}"
current_stage="$stage"
fi
local deps="${DEPS[$pkg]}"
if [ -z "$deps" ]; then
deps="(none)"
fi
echo -e "  ${GREEN}$pkg${NC}"
echo -e "    Dependencies: ${YELLOW}$deps${NC}"
done
echo ""
}
generate_makefile() {
local output="$1"
cat > "$output" << 'EOF'
.PHONY: all clean help
PARALLEL_JOBS ?= $(shell nproc)
all: opencog
help:
@echo "OpenCog Debian Package Build System"
@echo ""
@echo "Targets:"
@echo "  all              - Build all packages"
@echo "  clean            - Clean build artifacts"
@echo "  <package>        - Build specific package"
@echo ""
@echo "Variables:"
@echo "  PARALLEL_JOBS    - Number of parallel jobs (default: $(PARALLEL_JOBS))"
clean:
find . -name "*.deb" -delete
find . -name "*.changes" -delete
find . -name "*.buildinfo" -delete
EOF
for pkg in $(topo_sort); do
local deps="${DEPS[$pkg]}"
cat >> "$output" << EOF
$pkg: $deps
@echo "Building $pkg..."
cd $pkg && ./update-$pkg.sh && cd $pkg-* && dpkg-buildpackage -rfakeroot -us -uc -j\$(PARALLEL_JOBS)
sudo dpkg -i $pkg/*.deb || sudo apt-get install -f -y
EOF
done
echo -e "${GREEN}Makefile generated: $output${NC}"
}
generate_cmake() {
local output="$1"
cat > "$output" << 'EOF'
cmake_minimum_required(VERSION 3.16)
project(OpenCogPackages NONE)
include(ExternalProject)
EOF
for pkg in $(topo_sort); do
local deps="${DEPS[$pkg]}"
local dep_targets=""
for dep in $deps; do
dep_targets="$dep_targets $dep"
done
cat >> "$output" << EOF
ExternalProject_Add($pkg
SOURCE_DIR \${CMAKE_CURRENT_SOURCE_DIR}/$pkg
CONFIGURE_COMMAND ./update-$pkg.sh
BUILD_COMMAND dpkg-buildpackage -rfakeroot -us -uc
INSTALL_COMMAND sudo dpkg -i ../*.deb || sudo apt-get install -f -y
BUILD_IN_SOURCE 1
DEPENDS $dep_targets
)
EOF
done
echo -e "${GREEN}CMakeLists.txt generated: $output${NC}"
}
visualize_graph() {
local output="$1"
cat > "$output" << 'EOF'
digraph OpenCogDependencies {
rankdir=TB;
node [shape=box, style=filled, fillcolor=lightblue];
EOF
for pkg in "${!DEPS[@]}"; do
local deps="${DEPS[$pkg]}"
local stage="${STAGE[$pkg]}"
local color="lightblue"
case $stage in
0) color="lightcoral" ;;
1) color="lightgreen" ;;
2) color="lightyellow" ;;
3) color="lightcyan" ;;
4) color="lavender" ;;
5) color="peachpuff" ;;
6) color="lightpink" ;;
7) color="lightgray" ;;
8) color="lightsalmon" ;;
9) color="lightsteelblue" ;;
10) color="gold" ;;
esac
echo "    \"$pkg\" [fillcolor=$color, label=\"$pkg\\nStage $stage\"];" >> "$output"
for dep in $deps; do
echo "    \"$dep\" -> \"$pkg\";" >> "$output"
done
done
echo "}" >> "$output"
echo -e "${GREEN}Dependency graph generated: $output${NC}"
echo -e "${YELLOW}Render with: dot -Tpng $output -o dependencies.png${NC}"
}
check_missing() {
echo -e "${CYAN}Checking for missing packages...${NC}"
local missing=()
for pkg in "${!DEPS[@]}"; do
if [ ! -d "$pkg" ]; then
missing+=("$pkg")
fi
done
if [ ${#missing[@]} -gt 0 ]; then
echo -e "${YELLOW}Missing package directories:${NC}"
for pkg in "${missing[@]}"; do
echo -e "  ${RED}✗${NC} $pkg"
done
return 1
else
echo -e "${GREEN}All package directories present${NC}"
return 0
fi
}
main() {
local action="${1:-order}"
init_dependencies
case $action in
order)
generate_build_order
;;
makefile)
generate_makefile "Makefile.packages"
;;
cmake)
generate_cmake "CMakeLists.packages.txt"
;;
graph)
visualize_graph "dependencies.dot"
;;
check)
check_missing
;;
*)
echo "Usage: $0 {order|makefile|cmake|graph|check}"
echo ""
echo "Commands:"
echo "  order     - Display build order (default)"
echo "  makefile  - Generate Makefile"
echo "  cmake     - Generate CMakeLists.txt"
echo "  graph     - Generate dependency graph (DOT format)"
echo "  check     - Check for missing packages"
exit 1
;;
esac
}
main "$@"