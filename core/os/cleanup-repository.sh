#!/bin/bash
set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"
log() {
echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}
error_exit() {
log "ERROR: $1"
exit 1
}
backup_file() {
local file="$1"
local backup_dir="backup/$(date '+%Y%m%d_%H%M%S')"
if [ -f "$file" ]; then
mkdir -p "$backup_dir"
cp "$file" "$backup_dir/"
log "Backed up $file to $backup_dir/"
fi
}
log "Starting GNU Hurd Cognitive Architecture repository cleanup..."
mkdir -p backup
log "Created backup directory: backup/"
log "Phase 1: Removing financial intelligence components..."
if [ -f ".github/workflows/financial-intelligence-engine.yml" ]; then
backup_file ".github/workflows/financial-intelligence-engine.yml"
rm ".github/workflows/financial-intelligence-engine.yml"
log "Removed financial-intelligence-engine.yml"
fi
if [ -f ".github/workflows/cogsplit.yml" ]; then
backup_file ".github/workflows/cogsplit.yml"
log "Backed up cogsplit.yml - needs manual review to extract cognitive components"
fi
if [ -f ".github/workflows/ci-tests.yml" ]; then
backup_file ".github/workflows/ci-tests.yml"
log "Backed up ci-tests.yml - needs manual review to replace GnuCash with Hurd-specific tests"
fi
log "Phase 2: Identifying ElizaOS components for separation..."
find . -name "*.md" -exec grep -l "ElizaOS\|eliza" {} \; 2>/dev/null | while read file; do
log "Found ElizaOS reference in: $file"
done
log "Phase 3: Cleaning documentation..."
find docs/ -name "*.md" -exec grep -l "financial\|trading\|investment\|market" {} \; 2>/dev/null | while read file; do
log "Found financial content in: $file"
done
log "Phase 4: Creating clean repository structure..."
mkdir -p {cognitive,distributed,performance,development,build}
if [ -d "docs" ]; then
mkdir -p cognitive/docs
find docs/ -name "*OpenCog*" -o -name "*cognitive*" -o -name "*AtomSpace*" | while read file; do
if [ -f "$file" ]; then
cp "$file" "cognitive/docs/"
log "Moved cognitive doc: $file"
fi
done
fi
if [ -d "docs" ]; then
mkdir -p distributed/docs
find docs/ -name "*Plan9*" -o -name "*Inferno*" -o -name "*distributed*" | while read file; do
if [ -f "$file" ]; then
cp "$file" "distributed/docs/"
log "Moved distributed doc: $file"
fi
done
fi
if [ -d "docs" ]; then
mkdir -p performance/docs
find docs/ -name "*Kokkos*" -o -name "*performance*" | while read file; do
if [ -f "$file" ]; then
cp "$file" "performance/docs/"
log "Moved performance doc: $file"
fi
done
fi
if [ -d "docs" ]; then
mkdir -p development/docs
find docs/ -name "*Compiler*" -o -name "*Theia*" -o -name "*development*" | while read file; do
if [ -f "$file" ]; then
cp "$file" "development/docs/"
log "Moved development doc: $file"
fi
done
fi
if [ -d "docs" ]; then
mkdir -p build/docs
find docs/ -name "*Guix*" -o -name "*Mes*" -o -name "*build*" | while read file; do
if [ -f "$file" ]; then
cp "$file" "build/docs/"
log "Moved build doc: $file"
fi
done
fi
log "Phase 5: Creating clean README..."
cat > README_CLEAN.md << 'EOF'
**Project:** GNU Hurd Cognitive Microkernel Operating System
**Version:** 2.0 - Clean Architecture Focus
**Status:** Reorganization Phase
This repository represents the world's first cognitive microkernel operating system, integrating GNU Hurd's modular architecture with advanced cognitive computing frameworks. The project aims to solve 350+ open GNU Hurd issues through intelligent, self-optimizing system components.
Transform GNU Hurd from a traditional microkernel OS into a cognitive operating system that can:
- **Self-diagnose** and **self-heal** system issues
- **Optimize performance** through machine learning
- **Adapt** to changing workloads and hardware
- **Learn** from system behavior patterns
- **Coordinate** distributed resources intelligently
```
├── cognitive/
├── distributed/
├── performance/
├── development/
├── build/
├── hurd-ecosystem/
├── docs/
└── backup/
```
- **GNU Hurd**: Microkernel-based operating system foundation
- **GNU Mach**: Microkernel providing core system services
- **MIG**: Interface generator for IPC
- **OpenCog**: Artificial General Intelligence framework
- **AtomSpace**: Hypergraph database for knowledge representation
- **CogServer**: Distributed cognitive processing
- **Plan9**: Distributed operating system with 9P protocol
- **Inferno**: Virtual machine with Limbo programming language
- **SingularityNET**: Distributed AI marketplace
- **Kokkos**: Performance portability programming ecosystem
- **Compiler Explorer**: Interactive compilation analysis
- **Theia**: Custom development environment framework
- **GNU Guix**: Declarative, transactional package management
- **GNU Mes**: Scheme interpreter and C compiler for bootstrapping
- Repository cleanup and reorganization
- Core GNU Hurd integration
- Development environment setup
- Plan9 and Inferno integration
- OpenCog cognitive architecture
- GNU Guix build system
- SingularityNET distributed AI services
- Kokkos performance optimization
- Compiler Explorer development tools
- Theia IDE framework
- AI model ecosystem
- Community development tools
- GCC configured for i686-gnu target
- GNU Make and Autotools
- Git for development
```bash
./configure --host=i686-gnu
make
make install
```
- **Performance**: IPC optimization, memory management
- **Hardware Support**: Device drivers, architecture ports
- **Security**: Capability system enhancements
- **Testing**: Automated testing framework
- **Documentation**: Improved developer guides
1. Read the [Development Roadmap](DEVELOPMENT_ROADMAP.md)
2. Choose a component to work on
3. Follow existing code style and conventions
4. Add tests for new functionality
5. Update documentation as needed
- **Project Website**: <http://www.gnu.org/software/hurd/>
- **Development Roadmap**: [DEVELOPMENT_ROADMAP.md](DEVELOPMENT_ROADMAP.md)
- **Architecture Overview**: [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md)
- **Bug Reports**: <bug-hurd@gnu.org>
- **Help**: <help-hurd@gnu.org>
- **IRC**:
The GNU Hurd is free software. All components are covered by the GNU General Public License. See [COPYING](COPYING) for details.
---
*The future of operating systems is cognitive, and it starts with GNU Hurd.*
EOF
log "Created clean README: README_CLEAN.md"
log "Phase 6: Creating action items list..."
cat > CLEANUP_ACTION_ITEMS.md << 'EOF'
- [ ] Delete `.github/workflows/financial-intelligence-engine.yml`
- [ ] Clean `.github/workflows/cogsplit.yml` (extract cognitive parts, remove financial)
- [ ] Fix `.github/workflows/ci-tests.yml` (replace GnuCash with Hurd-specific tests)
- [ ] Remove all financial references from documentation
- [ ] Create separate repository for ElizaOS components
- [ ] Move ElizaOS-related files to new repository
- [ ] Update documentation to remove ElizaOS references
- [ ] Clean up any remaining ElizaOS dependencies
- [ ] Move cognitive docs to `cognitive/docs/`
- [ ] Move distributed system docs to `distributed/docs/`
- [ ] Move performance docs to `performance/docs/`
- [ ] Move development docs to `development/docs/`
- [ ] Move build system docs to `build/docs/`
- [ ] Update all documentation links
- [ ] Remove financial intelligence engine workflow
- [ ] Clean cogsplit workflow (keep cognitive, remove financial)
- [ ] Fix ci-tests workflow (Hurd-specific tests)
- [ ] Update workflow documentation
- [ ] Replace README.md with README_CLEAN.md
- [ ] Update DEVELOPMENT_ROADMAP.md
- [ ] Clean up any remaining financial references
- [ ] Update project description and tags
- [ ] No financial/trading references remain
- [ ] No ElizaOS components in main repository
- [ ] All documentation properly organized
- [ ] GitHub Actions workflows cleaned
- [ ] Build system functional
- [ ] Tests pass
- [ ] Documentation links updated
- [ ] Repository focuses on GNU Hurd + Cognitive Architecture
- [ ] Clear separation of concerns
- [ ] Proper documentation structure
- [ ] Clean development workflow
- [ ] Community guidelines updated
- All removed files are backed up in `backup/` directory
- Review backup before permanent deletion
- Consider creating separate repositories for removed components
- Update all documentation to reflect new structure
EOF
log "Created action items: CLEANUP_ACTION_ITEMS.md"
log "Phase 7: Cleanup summary..."
echo ""
echo "=========================================="
echo "GNU Hurd Cognitive Architecture Cleanup"
echo "=========================================="
echo ""
echo "✅ Backup created: backup/"
echo "✅ Clean README created: README_CLEAN.md"
echo "✅ Action items created: CLEANUP_ACTION_ITEMS.md"
echo "✅ Directory structure created:"
echo "   - cognitive/"
echo "   - distributed/"
echo "   - performance/"
echo "   - development/"
echo "   - build/"
echo ""
echo "📋 Next Steps:"
echo "1. Review CLEANUP_ACTION_ITEMS.md"
echo "2. Manually remove financial components"
echo "3. Separate ElizaOS components"
echo "4. Update documentation structure"
echo "5. Test build system"
echo ""
echo "🎯 Goal: Focus on GNU Hurd + Cognitive Architecture"
echo ""
log "Cleanup script completed successfully!"
log "Review CLEANUP_ACTION_ITEMS.md for next steps"