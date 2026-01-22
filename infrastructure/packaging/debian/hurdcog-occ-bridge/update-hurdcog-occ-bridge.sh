#!/bin/bash
set -e
PACKAGE_NAME="hurdcog-occ-bridge"
VERSION="1.0.0"
OCC_ROOT="../.."
echo "Preparing $PACKAGE_NAME source package..."
rm -rf ${PACKAGE_NAME}-${VERSION}
rm -f ${PACKAGE_NAME}_${VERSION}.orig.tar.gz
mkdir -p ${PACKAGE_NAME}-${VERSION}
echo "  Copying Python bridge..."
cp ${OCC_ROOT}/hurdcog-integration/atomspace-hurdcog-bridge.py ${PACKAGE_NAME}-${VERSION}/
echo "  Copying Scheme interfaces..."
cp ${OCC_ROOT}/cognumach.scm ${PACKAGE_NAME}-${VERSION}/
cp ${OCC_ROOT}/hurdcog.scm ${PACKAGE_NAME}-${VERSION}/
cp ${OCC_ROOT}/occ-hurdcog-unified.scm ${PACKAGE_NAME}-${VERSION}/
echo "  Copying AGI-OS synergy framework..."
cp ${OCC_ROOT}/agi-os-synergy.scm ${PACKAGE_NAME}-${VERSION}/
echo "  Creating README..."
cat > ${PACKAGE_NAME}-${VERSION}/README.md << 'EOF'
Integration layer connecting OpenCog Collection, HurdCog, and Cognumach for three-layer cognitive synergy.
The HurdCog-OCC Bridge enables bidirectional communication and cognitive synergy between:
- **Layer 0**: Cognumach microkernel
- **Layer 1**: HurdCog cognitive operating system
- **Layer 2**: OpenCog Collection
Bidirectional communication between OpenCog AtomSpace and HurdCog MachSpace:
```python
from hurdcog import AtomSpaceHurdCogBridge
bridge = AtomSpaceHurdCogBridge()
bridge.connect()
bridge.send_atom(atom_id, atom_type, atom_data)
bridge.sync_atomspace()
```
Microkernel cognitive primitives for Layer 0 integration.
OS-level cognitive operations for Layer 1 integration.
Unified integration framework coordinating all three layers.
Complete cognitive synergy implementation:
```scheme
(use-modules (agi-os synergy))
;; Initialize three-layer architecture
(agi-os-init)
;; Execute cognitive fusion
(agi-os-cognitive-fusion)
;; Check system status
(agi-os-layer-status)
```
Links ECAN (Economic Attention Networks) to system resource allocation.
High-importance atoms receive more OS-level resources.
Mines patterns from system behavior and applies them to OS configuration.
Enables continuous OS adaptation through cognitive learning.
Enables PLN (Probabilistic Logic Networks) inference at kernel level.
Kernel decisions informed by cognitive reasoning processes.
Four-phase cross-layer cognitive process:
1. **Knowledge Gathering**: Collect from all three layers
2. **Knowledge Integration**: Unified AtomSpace representation
3. **Cross-Layer Reasoning**: PLN inference across layers
4. **Insight Application**: Update all layers with insights
```bash
sudo agi-os-init
```
```bash
agi-os-status
```
```python
from hurdcog import AtomSpaceHurdCogBridge
bridge = AtomSpaceHurdCogBridge()
bridge.connect()
bridge.sync_atomspace()
```
```scheme
(use-modules (agi-os synergy))
(agi-os-init)
(machspace-sync)
```
See `/usr/share/doc/hurdcog-occ-bridge/` for complete documentation.
AGPL-3.0+
EOF
echo "  Copying debian packaging files..."
cp -r debian ${PACKAGE_NAME}-${VERSION}/
echo "  Creating source tarball..."
tar czf ${PACKAGE_NAME}_${VERSION}.orig.tar.gz ${PACKAGE_NAME}-${VERSION}
echo "✓ Source package prepared: ${PACKAGE_NAME}_${VERSION}.orig.tar.gz"
echo ""
echo "To build the package:"
echo "  cd ${PACKAGE_NAME}-${VERSION}"
echo "  dpkg-buildpackage -rfakeroot -us -uc"