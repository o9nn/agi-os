#!/bin/bash
# Validate and complete Debian packaging for all AGI-OS components

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPORT="${SCRIPT_DIR}/packaging-validation-report.txt"

echo "AGI-OS Debian Packaging Validation and Completion" | tee "$REPORT"
echo "=================================================" | tee -a "$REPORT"
echo "Date: $(date)" | tee -a "$REPORT"
echo "" | tee -a "$REPORT"

# Function to check if a package has debian directory
check_package() {
    local pkg_dir="$1"
    local pkg_name=$(basename "$pkg_dir")
    
    if [ -d "${pkg_dir}/debian" ]; then
        if [ -f "${pkg_dir}/debian/control" ]; then
            echo "[OK] $pkg_name: debian/control found" | tee -a "$REPORT"
            return 0
        else
            echo "[WARN] $pkg_name: debian directory exists but no control file" | tee -a "$REPORT"
            return 1
        fi
    else
        echo "[MISSING] $pkg_name: No debian directory" | tee -a "$REPORT"
        return 1
    fi
}

# Function to create minimal debian packaging
create_minimal_packaging() {
    local pkg_dir="$1"
    local pkg_name=$(basename "$pkg_dir")
    
    mkdir -p "${pkg_dir}/debian"
    
    cat > "${pkg_dir}/debian/control" << CONTROL
Source: ${pkg_name}
Section: devel
Priority: optional
Maintainer: OpenCog Developers <opencog@googlegroups.com>
Build-Depends: debhelper (>= 11), build-essential, cmake
Standards-Version: 4.6.2
Homepage: https://opencog.org
Vcs-Git: https://github.com/opencog/agi-os.git
Vcs-Browser: https://github.com/opencog/agi-os

Package: ${pkg_name}
Architecture: any
Depends: \${shlibs:Depends}, \${misc:Depends}
Description: ${pkg_name} component for AGI-OS
 Part of the AGI Operating System framework.
CONTROL

    cat > "${pkg_dir}/debian/changelog" << CHANGELOG
${pkg_name} (1.0.0-1) unstable; urgency=medium

  * Initial release for AGI-OS

 -- OpenCog Developers <opencog@googlegroups.com>  $(date -R)
CHANGELOG

    cat > "${pkg_dir}/debian/rules" << RULES
#!/usr/bin/make -f

%:
dh \$@

override_dh_auto_configure:
dh_auto_configure -- -DCMAKE_BUILD_TYPE=Release

override_dh_auto_build:
dh_auto_build

override_dh_auto_install:
dh_auto_install
RULES

    chmod +x "${pkg_dir}/debian/rules"
    
    echo "[CREATED] $pkg_name: Minimal debian packaging created" | tee -a "$REPORT"
}

# Validate all packages
echo "Validating existing packages..." | tee -a "$REPORT"
echo "" | tee -a "$REPORT"

for pkg_dir in "${SCRIPT_DIR}"/*; do
    if [ -d "$pkg_dir" ] && [ "$(basename "$pkg_dir")" != "generate" ]; then
        if ! check_package "$pkg_dir"; then
            create_minimal_packaging "$pkg_dir"
        fi
    fi
done

echo "" | tee -a "$REPORT"
echo "Packaging validation and completion finished!" | tee -a "$REPORT"
echo "Report saved to: $REPORT" | tee -a "$REPORT"
