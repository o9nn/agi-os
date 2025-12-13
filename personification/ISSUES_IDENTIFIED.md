# GitHub Actions Build Issues - Analysis

## Date: 2025-12-12

### Critical Issues Identified

#### 1. **CI Workflow - pnpm Version Not Specified**
**Status**: CRITICAL  
**Affected Jobs**: All airi build jobs (lint, typecheck, build-stage-web, build-stage-tamagotchi, build-ui-transitions, build-ui-loading-screens)

**Error**:
```
Error: No pnpm version is specified.
Please specify it by one of the following ways:
  - in the GitHub Action config with the key "version"
  - in the package.json with the key "packageManager"
```

**Root Cause**: 
- The workflow uses `pnpm/action-setup@v4` without explicitly specifying the version parameter
- While `airi/package.json` contains `"packageManager": "pnpm@10.20.0"`, the action is not detecting it properly
- The action is looking for the package.json in the repository root, but it's in the `airi/` subdirectory

**Solution**:
- Add explicit `version` parameter to `pnpm/action-setup@v4` in the CI workflow
- Alternatively, specify `package_json_file` parameter pointing to `airi/package.json`

---

#### 2. **CI Workflow - Provenance Check Failure**
**Status**: CRITICAL  
**Affected Jobs**: airi - check-provenance

**Error**:
```
Error: ENOENT: no such file or directory, open '/home/runner/work/moeru-ai/moeru-ai/package.json'
```

**Root Cause**:
- The provenance action is looking for `package.json` in the repository root
- The repository is a monorepo with packages in subdirectories (airi, chat, etc.)
- No root-level package.json exists

**Solution**:
- Update the provenance check to specify the correct working directory
- Add `working-directory: airi` to the provenance check step

---

#### 3. **Next.js Deploy Workflow - No Package Manager Detected**
**Status**: CRITICAL  
**Affected Jobs**: Deploy Next.js site to Pages - build

**Error**:
```
Unable to determine package manager
Process completed with exit code 1
```

**Root Cause**:
- The workflow checks for `yarn.lock` or `package.json` in the repository root
- The repository doesn't have a Next.js application in the root
- This workflow appears to be a template that was never properly configured

**Solution**:
- Either remove this workflow if not needed
- Or configure it to point to the correct Next.js application directory
- Based on repo structure, there's no obvious Next.js app, so this workflow should likely be removed

---

#### 4. **Maintenance Workflow - Nix Build Failure**
**Status**: MEDIUM  
**Affected Jobs**: Update Nix pnpm deps hash

**Error**:
```
tee: /dev/tty: No such device or address
+ HASH=
Process completed with exit code 1
```

**Root Cause**:
- The Nix maintenance script tries to write to `/dev/tty` which is not available in GitHub Actions
- The script `nix/update-pnpm-deps-hash.sh` needs to be updated for CI environment

**Solution**:
- Update the script to handle CI environment where `/dev/tty` is not available
- Use alternative output methods for CI

---

### Additional Observations

1. **Monorepo Structure**: The repository is a monorepo with multiple independent packages
2. **Package Manager**: All packages use pnpm with versions specified in their respective package.json files
3. **Workflow Complexity**: The CI workflow is well-structured with change detection, but needs fixes for pnpm setup

### Priority Fix Order

1. Fix pnpm version specification in CI workflow (blocks all builds)
2. Fix provenance check working directory
3. Remove or fix Next.js deployment workflow
4. Fix Nix maintenance script for CI environment
