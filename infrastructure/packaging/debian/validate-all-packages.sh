#!/bin/bash
set -e
DEBIAN_DIR="/home/ubuntu/agi-os/infrastructure/packaging/debian"
REPORT_FILE="$DEBIAN_DIR/PACKAGE_VALIDATION_REPORT.md"
echo "
echo "" >> "$REPORT_FILE"
echo "**Date:** $(date '+%Y-%m-%d %H:%M:%S')" >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"
echo "
echo "" >> "$REPORT_FILE"
CORE_PACKAGES=(
"cogutil"
"atomspace"
"atomspace-storage"
"cogserver"
"pln"
"ure"
"unify"
"spacetime"
"attention"
"learn"
"miner"
"asmoses"
"lg-atomese"
"generate"
"vision"
)
OS_PACKAGES=(
"cognumach"
"hurdcog"
"hurdcog-atomspace-bridge"
"hurdcog-cogkernel-core"
"hurdcog-machspace"
"hurdcog-occ-bridge"
)
INTEGRATION_PACKAGES=(
"cognitive-grip"
"agi-os-unified"
)
echo "
echo "" >> "$REPORT_FILE"
for pkg in "${CORE_PACKAGES[@]}"; do
if [ -d "$DEBIAN_DIR/$pkg" ]; then
if [ -f "$DEBIAN_DIR/$pkg/debian/control" ]; then
echo "- ✅ **$pkg**: Package directory and control file present" >> "$REPORT_FILE"
else
echo "- ⚠️  **$pkg**: Package directory exists but missing debian/control" >> "$REPORT_FILE"
fi
else
echo "- ❌ **$pkg**: Package directory missing" >> "$REPORT_FILE"
fi
done
echo "" >> "$REPORT_FILE"
echo "
echo "" >> "$REPORT_FILE"
for pkg in "${OS_PACKAGES[@]}"; do
if [ -d "$DEBIAN_DIR/$pkg" ]; then
if [ -f "$DEBIAN_DIR/$pkg/debian/control" ]; then
echo "- ✅ **$pkg**: Package directory and control file present" >> "$REPORT_FILE"
else
echo "- ⚠️  **$pkg**: Package directory exists but missing debian/control" >> "$REPORT_FILE"
fi
else
echo "- ❌ **$pkg**: Package directory missing" >> "$REPORT_FILE"
fi
done
echo "" >> "$REPORT_FILE"
echo "
echo "" >> "$REPORT_FILE"
for pkg in "${INTEGRATION_PACKAGES[@]}"; do
if [ -d "$DEBIAN_DIR/$pkg" ]; then
if [ -f "$DEBIAN_DIR/$pkg/debian/control" ]; then
echo "- ✅ **$pkg**: Package directory and control file present" >> "$REPORT_FILE"
else
echo "- ⚠️  **$pkg**: Package directory exists but missing debian/control" >> "$REPORT_FILE"
fi
else
echo "- ❌ **$pkg**: Package directory missing" >> "$REPORT_FILE"
fi
done
echo "" >> "$REPORT_FILE"
echo "
echo "" >> "$REPORT_FILE"
TOTAL_PACKAGES=$((${
PRESENT_PACKAGES=$(find "$DEBIAN_DIR" -mindepth 2 -maxdepth 2 -name "control" | wc -l)
echo "- **Total Expected Packages**: $TOTAL_PACKAGES" >> "$REPORT_FILE"
echo "- **Packages with debian/control**: $PRESENT_PACKAGES" >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"
echo "Validation report generated: $REPORT_FILE"
cat "$REPORT_FILE"