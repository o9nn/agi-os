#!/bin/bash
# Unified AGI-OS Build System
# Ensures correct build order with atomspace-storage before cogserver

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_LOG="${SCRIPT_DIR}/build.log"

echo "AGI-OS Unified Build System" | tee "$BUILD_LOG"
echo "===========================" | tee -a "$BUILD_LOG"
echo "Build started: $(date)" | tee -a "$BUILD_LOG"

# Layer 0: Microkernel Foundation
echo -e "\n[Layer 0] Building Cognumach microkernel..." | tee -a "$BUILD_LOG"
cd "${SCRIPT_DIR}/cognumach" && bash update-cognumach.sh 2>&1 | tee -a "$BUILD_LOG"

# Layer 1: Foundation Libraries
echo -e "\n[Layer 1] Building CogUtil..." | tee -a "$BUILD_LOG"
cd "${SCRIPT_DIR}/cogutil" && bash update-cogutil.sh 2>&1 | tee -a "$BUILD_LOG"

# Layer 2: Core AtomSpace
echo -e "\n[Layer 2] Building AtomSpace..." | tee -a "$BUILD_LOG"
cd "${SCRIPT_DIR}/atomspace" && bash update-atomspace.sh 2>&1 | tee -a "$BUILD_LOG"

# Layer 2.5: CRITICAL - AtomSpace Storage (MUST be before cogserver)
echo -e "\n[Layer 2.5] Building AtomSpace Storage (CRITICAL - must be before cogserver)..." | tee -a "$BUILD_LOG"
cd "${SCRIPT_DIR}/atomspace-storage" && bash update-atomspace-storage.sh 2>&1 | tee -a "$BUILD_LOG"

# Layer 3: Storage Backends
echo -e "\n[Layer 3] Building Storage Backends..." | tee -a "$BUILD_LOG"
for backend in atomspace-cog atomspace-rocks atomspace-pgres; do
    if [ -d "${SCRIPT_DIR}/${backend}" ]; then
        echo "  Building ${backend}..." | tee -a "$BUILD_LOG"
        cd "${SCRIPT_DIR}/${backend}" && bash update-${backend}.sh 2>&1 | tee -a "$BUILD_LOG"
    fi
done

# Layer 4: Core Services (cogserver now has atomspace-storage available)
echo -e "\n[Layer 4] Building Core Services..." | tee -a "$BUILD_LOG"
cd "${SCRIPT_DIR}/cogserver" && bash update-cogserver.sh 2>&1 | tee -a "$BUILD_LOG"

# Layer 4: URE
echo -e "\n[Layer 4] Building Unified Rule Engine..." | tee -a "$BUILD_LOG"
if [ -d "${SCRIPT_DIR}/ure" ]; then
    cd "${SCRIPT_DIR}/ure" && bash update-ure.sh 2>&1 | tee -a "$BUILD_LOG"
fi

# Layer 4.5: Cognitive OS
echo -e "\n[Layer 4.5] Building HurdCog Cognitive OS..." | tee -a "$BUILD_LOG"
cd "${SCRIPT_DIR}/hurdcog" && bash update-hurdcog.sh 2>&1 | tee -a "$BUILD_LOG"

# Layer 4.6: AGI-OS Integration
echo -e "\n[Layer 4.6] Building AGI-OS Integration Components..." | tee -a "$BUILD_LOG"
for component in hurdcog-atomspace-bridge cognumach-cognitive-scheduler; do
    if [ -d "${SCRIPT_DIR}/${component}" ]; then
        echo "  Building ${component}..." | tee -a "$BUILD_LOG"
        cd "${SCRIPT_DIR}/${component}" && bash update-${component}.sh 2>&1 | tee -a "$BUILD_LOG"
    fi
done

# Layer 5: Cognitive Components
echo -e "\n[Layer 5] Building Cognitive Components..." | tee -a "$BUILD_LOG"
for component in attention pln miner unify spacetime; do
    if [ -d "${SCRIPT_DIR}/${component}" ]; then
        echo "  Building ${component}..." | tee -a "$BUILD_LOG"
        cd "${SCRIPT_DIR}/${component}" && bash update-${component}.sh 2>&1 | tee -a "$BUILD_LOG"
    fi
done

echo -e "\n[SUCCESS] AGI-OS build completed successfully!" | tee -a "$BUILD_LOG"
echo "Build finished: $(date)" | tee -a "$BUILD_LOG"
