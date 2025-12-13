# Current Build Errors Analysis - moeru-ai Repository

## Date: December 12, 2025

## Summary

The GitHub Actions CI pipeline is failing with multiple TypeScript type checking errors and build failures. The main issues are:

### 1. **TypeScript Strict Null Checks Violations**
**Location:** Multiple files in `packages/stage-ui/src/`
**Severity:** HIGH - Blocking typecheck

**Affected Files:**
- `stores/mods/api/channel-server.ts` - Missing module '@proj-airi/server-sdk'
- `stores/modules/speech.ts` - Undefined type assignments
- `stores/providers.ts` - Possibly undefined metadata
- `stores/providers/aliyun/token.ts` - Undefined string assignments
- `utils/eye-motions.ts` - Multiple "possibly undefined" errors

**Root Cause:** TypeScript strict null checks are enabled but code doesn't handle undefined cases properly.

### 2. **Missing Module: @proj-airi/server-sdk**
**Location:** `packages/stage-ui/src/stores/mods/api/channel-server.ts`
**Error:** `Cannot find module '@proj-airi/server-sdk' or its corresponding type declarations`

**Root Cause:** The package either:
- Doesn't have proper exports defined
- Hasn't been built yet
- Is missing type declarations

### 3. **Vite Plugin Type Incompatibility**
**Location:** `docs/.vitepress/config.ts`
**Error:** Type mismatch between different Vite plugin versions

**Root Cause:** Multiple versions of Vite are being used (7.1.12 with different @types/node versions), causing type conflicts.

### 4. **Build Failures**
- `stage-tamagotchi` build fails due to typecheck errors
- `docs` typecheck fails with 30+ errors
- `stage-web` build succeeds but depends on docs

### 5. **Cache Path Validation Error**
**Location:** `airi - check-provenance`
**Severity:** WARNING
**Error:** Cache path validation failure

## Priority Fixes Required

### High Priority:
1. Fix @proj-airi/server-sdk module resolution
2. Add null checks to stage-ui files
3. Resolve Vite plugin type conflicts

### Medium Priority:
4. Fix cache path validation in provenance check
5. Ensure build order respects dependencies

### Low Priority:
6. Clean up implicit any types
7. Add proper error handling

## Next Steps

1. Check server-sdk package structure and exports
2. Add proper null checks and type guards
3. Standardize Vite versions across the monorepo
4. Run local typecheck to verify fixes
5. Test full build pipeline
