# Detailed Build Errors Analysis - moeru-ai Repository

**Date:** December 13, 2025  
**Repository:** https://github.com/o9nn/moeru-ai  
**Analysis Session:** Complete CI/CD Pipeline Review

---

## Executive Summary

The GitHub Actions CI pipeline is experiencing **systematic build failures** across multiple workflows. The root cause is a **missing build step** for the `@proj-airi/server-sdk` package, which causes cascading TypeScript errors in dependent packages.

### Critical Issues Identified:

1. **Missing Package Build Artifacts** (CRITICAL)
   - `@proj-airi/server-sdk` package has no `dist/` folder
   - Dependent packages cannot resolve type declarations
   - Blocks typecheck and build jobs

2. **TypeScript Strict Null Checks Violations** (HIGH)
   - Multiple files have unsafe null/undefined access
   - Affects stage-ui, stage-web, and docs packages

3. **Lint Violations** (MEDIUM)
   - Import ordering issues
   - Trailing spaces in markdown
   - Vue component formatting

4. **Build Order Dependencies** (MEDIUM)
   - Packages must be built before apps
   - Current workflow doesn't enforce proper order

---

## Detailed Error Breakdown

### 1. Missing Module: @proj-airi/server-sdk

**Error:**
```
Cannot find module '@proj-airi/server-sdk' or its corresponding type declarations.
```

**Location:**
- `airi/packages/stage-ui/src/stores/mods/api/channel-server.ts`

**Root Cause:**
The `server-sdk` package exists but hasn't been built. The `dist/` folder is missing, so TypeScript cannot find the type declarations specified in `package.json`:

```json
{
  "exports": {
    ".": {
      "types": "./dist/index.d.ts",
      "import": "./dist/index.js"
    }
  }
}
```

**Impact:**
- Blocks typecheck job
- Prevents stage-web build
- Cascades to dependent packages

**Fix Required:**
1. Build server-sdk package before dependent packages
2. Ensure `pnpm run build:packages` runs successfully
3. Verify dist folder is created

---

### 2. TypeScript Strict Null Checks Violations

**Affected Files:**

#### a) `packages/stage-ui/src/stores/providers.ts`
```typescript
// Error: Object is possibly 'undefined'
const metadata = provider.metadata
metadata.displayName // Error here
```

**Fix:** Add null check
```typescript
if (metadata) {
  metadata.displayName
}
```

#### b) `packages/stage-ui/src/stores/providers/aliyun/token.ts`
```typescript
// Error: Type 'undefined' is not assignable to type 'string'
const token: string = maybeUndefined
```

**Fix:** Add type guard or default value
```typescript
const token: string = maybeUndefined ?? ''
```

#### c) `packages/stage-ui/src/utils/eye-motions.ts`
Multiple "possibly undefined" errors on array access and object properties.

**Fix:** Add proper null checks and type guards

---

### 3. Lint Violations

**Import Ordering Issues:**
```
Expected "@proj-airi/live2d-core" to come before "@proj-airi/stage-ui/composables"
Expected "CubismParameterId" to come before "EmotionIntensity"
```

**Location:** `airi/docs/LIVE2D_INTEGRATION.md`

**Fix:** Reorder imports alphabetically

**Trailing Spaces:**
Multiple markdown files have trailing spaces

**Fix:** Run `pnpm run lint:fix`

**Vue Component Formatting:**
```
Expected 1 line break before closing tag (`</button>`), but no line breaks found
```

**Location:** `airi/docs/examples/Live2DEmotionControl.vue`

**Fix:** Format Vue components properly

---

### 4. Build Stage Web Failure

**Error:**
```
Process completed with exit code 1.
```

**Location:** `airi - build-stage-web` job

**Root Cause:**
The build fails because:
1. Typecheck errors prevent successful build
2. Missing server-sdk types
3. Docs build depends on packages being built first

**Build Command:**
```bash
pnpm -F @proj-airi/stage-web run build
pnpm -F @proj-airi/docs run build:base
mv ./docs/.vitepress/dist ./apps/stage-web/dist/docs
pnpm -F @proj-airi/stage-ui run story:build
mv ./packages/stage-ui/.histoire/dist ./apps/stage-web/dist/ui
```

**Fix Required:**
1. Ensure all packages are built before apps
2. Fix typecheck errors
3. Verify dependencies are resolved

---

### 5. Typecheck Failures

**Error:**
```
Process completed with exit code 2.
```

**Location:** `airi - typecheck` job

**Errors Count:** 30+ TypeScript errors

**Categories:**
- Missing module declarations
- Undefined type assignments
- Strict null check violations
- Type incompatibilities

---

## GitHub Actions Workflow Analysis

### Current CI Workflow Structure

```yaml
jobs:
  changes:
    # Detects which projects changed
  
  airi:
    needs: changes
    strategy:
      matrix:
        include:
          - job: lint
          - job: typecheck
          - job: build-stage-web
          - job: build-stage-tamagotchi
          - job: build-ui-transitions
          - job: build-ui-loading-screens
```

### Issues with Current Workflow:

1. **No Package Build Step Before Typecheck**
   - Typecheck runs without building packages first
   - Missing dist folders cause module resolution errors

2. **Parallel Jobs Without Dependencies**
   - All jobs run in parallel
   - No guarantee packages are built before apps

3. **Skip-i18n Logic**
   - Complex conditional logic for i18n PRs
   - May skip necessary installs

---

## Priority Fix Plan

### Phase 1: Critical Fixes (MUST DO)

1. **Add Package Build Step to CI Workflow**
   ```yaml
   - name: Build packages
     run: pnpm run build:packages
     working-directory: airi
   ```
   - Add this step before typecheck and build jobs
   - Ensure it runs for all matrix jobs that need it

2. **Fix server-sdk Build**
   - Verify tsdown configuration
   - Ensure build script works locally
   - Check for missing dependencies

3. **Add Null Checks to stage-ui**
   - Fix all "possibly undefined" errors
   - Add type guards where needed
   - Use optional chaining (?.) and nullish coalescing (??)

### Phase 2: Medium Priority

4. **Fix Lint Violations**
   - Run `pnpm run lint:fix` in airi directory
   - Fix import ordering in docs
   - Remove trailing spaces

5. **Improve Workflow Dependencies**
   - Add proper job dependencies
   - Ensure packages build before apps
   - Add caching for faster builds

### Phase 3: Optimization

6. **Add Better Error Handling**
   - Add try-catch blocks where needed
   - Improve error messages
   - Add validation for undefined values

7. **Standardize TypeScript Config**
   - Ensure consistent strict settings
   - Add proper type declarations
   - Remove any implicit any types

---

## Recommended Workflow Changes

### Option A: Add Build Step to Each Job

```yaml
- name: Build packages
  if: matrix.turbo
  run: pnpm run build:packages
  working-directory: airi
```

**Pros:** Simple, works with existing matrix
**Cons:** Rebuilds packages for each job

### Option B: Separate Build Job

```yaml
jobs:
  build-packages:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v5
      - uses: pnpm/action-setup@v4
      - name: Install
        run: pnpm install --frozen-lockfile
      - name: Build packages
        run: pnpm run build:packages
      - name: Upload artifacts
        uses: actions/upload-artifact@v4
        with:
          name: packages-dist
          path: |
            airi/packages/*/dist
  
  airi:
    needs: [changes, build-packages]
    steps:
      - name: Download artifacts
        uses: actions/download-artifact@v4
```

**Pros:** Build once, use multiple times
**Cons:** More complex, requires artifact handling

### Recommendation: Option A

Use Option A for immediate fix. It's simpler and will work with the existing workflow structure. Option B can be implemented later for optimization.

---

## Testing Plan

### Local Testing Steps:

1. **Clean Install**
   ```bash
   cd airi
   rm -rf node_modules pnpm-lock.yaml
   pnpm install --frozen-lockfile
   ```

2. **Build Packages**
   ```bash
   pnpm run build:packages
   ```

3. **Verify server-sdk Build**
   ```bash
   ls -la packages/server-sdk/dist/
   cat packages/server-sdk/dist/index.d.ts
   ```

4. **Run Typecheck**
   ```bash
   pnpm run typecheck
   ```

5. **Run Lint**
   ```bash
   pnpm run lint
   ```

6. **Build Apps**
   ```bash
   pnpm run build:web
   pnpm run build:tamagotchi
   ```

### Expected Results:

- ✅ All packages build successfully
- ✅ server-sdk dist folder exists
- ✅ Typecheck passes with 0 errors
- ✅ Lint passes or only minor warnings
- ✅ All apps build successfully

---

## Next Steps

1. ✅ Analyze repository structure (COMPLETED)
2. ✅ Identify build errors (COMPLETED)
3. 🔄 Fix critical errors (IN PROGRESS)
   - Add build:packages step to workflow
   - Fix null check violations
   - Fix lint issues
4. ⏳ Test fixes locally
5. ⏳ Commit and push changes
6. ⏳ Verify GitHub Actions pass

---

## Files to Modify

### High Priority:
1. `.github/workflows/ci.yml` - Add build:packages step
2. `airi/packages/stage-ui/src/stores/providers.ts` - Add null checks
3. `airi/packages/stage-ui/src/stores/providers/aliyun/token.ts` - Fix undefined assignment
4. `airi/packages/stage-ui/src/utils/eye-motions.ts` - Add type guards

### Medium Priority:
5. `airi/docs/LIVE2D_INTEGRATION.md` - Fix import ordering and trailing spaces
6. `airi/docs/examples/Live2DEmotionControl.vue` - Fix formatting

### Low Priority:
7. Various TypeScript files - Add explicit types, remove implicit any

---

## Conclusion

The build failures are **fixable** with systematic changes. The root cause is clear: missing package builds before typecheck/build jobs. Once packages are built properly, the cascading errors will be resolved.

**Estimated Time to Fix:** 2-3 hours
**Risk Level:** Low (changes are straightforward)
**Success Probability:** High (root cause is well understood)
