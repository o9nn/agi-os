# GitHub Actions Scripts

This directory contains helper scripts used by GitHub Actions workflows for building and packaging OpenCog components.

## Scripts

### parse-package-info.sh

**Purpose**: Extract package metadata from opencog-debian update scripts

**Usage**:
```bash
./parse-package-info.sh <component-name>
```

**Example**:
```bash
./parse-package-info.sh cogutil
```

**Output**:
```
REPO_NAME=cogutil
DEB_NAME=opencog-cogutil
VERSION=2.0.3
DEBIAN_DIR=/path/to/opencog-debian/cogutil/debian
SOURCE_DIR=/path/to/cogutil
```

**What it does**:
- Reads the update-*.sh script from opencog-debian/<component>/
- Extracts REPO_NAME, DEB_NAME, and VERSION variables
- Determines the debian directory and source directory paths
- Outputs in a format that can be sourced by bash or used in GitHub Actions

**Used by**: prepare-package-build.sh, GitHub Actions workflows

---

### prepare-package-build.sh

**Purpose**: Prepare a component directory for Debian package building

**Usage**:
```bash
./prepare-package-build.sh <component-name>
```

**Example**:
```bash
./prepare-package-build.sh atomspace
```

**What it does**:
1. Calls parse-package-info.sh to get package metadata
2. Verifies the source directory exists
3. Verifies the debian packaging directory exists in opencog-debian/
4. Checks that all required debian files are present (control, rules, changelog, compat, copyright)
5. Copies the debian/ directory from opencog-debian/ to the source component directory
6. Makes debian/rules executable
7. Displays package information and next steps

**Requirements**:
- The component directory must exist (e.g., cogutil/, atomspace/)
- The opencog-debian/<component>/debian/ directory must exist
- The opencog-debian/<component>/update-<component>.sh script must exist

**Used by**: GitHub Actions debian-packages.yml workflow

---

### test-debian-packaging.sh

**Purpose**: Test that all components are properly configured for Debian packaging

**Usage**:
```bash
./test-debian-packaging.sh
```

**What it does**:
1. Tests all 10 components defined in the workflow
2. Verifies each component has a valid update script
3. Checks that debian directories exist in opencog-debian/
4. Validates all required debian files are present
5. Reports pass/fail status for each component
6. Returns exit code 0 if all pass, 1 if any fail

**Output Example**:
```
Testing: cogutil
  Package: opencog-cogutil
  Version: 2.0.3
  ✅ Source directory exists
  ✅ Debian directory exists
  ✅ All required debian files present

Test Summary
Total Components: 10
Passed: 10
Failed: 0
✅ All components are properly configured!
```

**Used by**: Development and CI validation

---

### visualize-workflow.sh

**Purpose**: Visualize the Debian package build workflow and dependencies

**Usage**:
```bash
./visualize-workflow.sh
```

**What it does**:
1. Shows all build stages and their dependencies
2. Displays package names and versions for each component
3. Illustrates the build order visually
4. Identifies parallel build opportunities
5. Provides workflow summary statistics

**Output**: Beautiful ASCII visualization of the entire build workflow

**Used by**: Documentation and development understanding

---

## Integration with opencog-debian

The scripts are designed to work with the debian packaging structure in the `opencog-debian/` directory. Each component has:

```
opencog-debian/
├── cogutil/
│   ├── debian/           # Debian packaging files
│   │   ├── control       # Package metadata and dependencies
│   │   ├── rules         # Build instructions
│   │   ├── changelog     # Version history
│   │   ├── compat        # Debhelper compatibility
│   │   ├── copyright     # License information
│   │   └── source/format # Source package format
│   └── update-cogutil.sh # Script with package metadata
├── atomspace/
│   ├── debian/
│   └── update-atomspace.sh
...
```

The update-*.sh scripts contain authoritative information about each package:
- **REPO_NAME**: Source directory name (e.g., "cogutil")
- **DEB_NAME**: Debian package name (e.g., "opencog-cogutil")
- **VERSION**: Package version (e.g., "2.0.3")

## Workflow Usage

In GitHub Actions workflows, use the scripts like this:

```yaml
- name: Checkout Repository
  uses: actions/checkout@v4
  with:
    submodules: recursive
    fetch-depth: 0

- name: Install Prerequisites
  run: |
    sudo apt-get update
    sudo apt-get install -y build-essential debhelper dpkg-dev cmake

- name: Prepare Debian packaging files
  run: |
    chmod +x .github/scripts/prepare-package-build.sh
    .github/scripts/prepare-package-build.sh cogutil

- name: Build Package
  run: |
    cd cogutil
    dpkg-buildpackage -us -uc -b
```

## Benefits

1. **Single Source of Truth**: Package metadata comes from opencog-debian update scripts
2. **Consistency**: Same packaging structure used locally and in CI
3. **Maintainability**: Updates to package info only need to be made in one place
4. **Validation**: Scripts verify required files exist before building
5. **Clarity**: Clear error messages when packaging files are missing

## Maintenance

When adding a new component:

1. Create opencog-debian/<component>/debian/ with required files
2. Create opencog-debian/<component>/update-<component>.sh with REPO_NAME, DEB_NAME, VERSION
3. Add the component to the debian-packages.yml workflow
4. The scripts will automatically handle the rest

## See Also

- [opencog-debian/README.md](../../opencog-debian/README.md) - Debian packaging overview
- [opencog-debian/BUILD_ORDER.md](../../opencog-debian/BUILD_ORDER.md) - Build dependency order
- [opencog-debian/PACKAGING_ARCHITECTURE.md](../../opencog-debian/PACKAGING_ARCHITECTURE.md) - Package architecture
