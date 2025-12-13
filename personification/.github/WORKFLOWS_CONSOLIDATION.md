# GitHub Actions Workflows Consolidation

This document explains the consolidation of GitHub Actions workflows in this repository.

## Summary

**Before**: 62 separate workflow files  
**After**: 8 workflow files (4 consolidated + 4 special-purpose)  
**Reduction**: 87%

## Consolidated Workflows

### 1. CI Workflow (`ci.yml`)

**Consolidates**: 20 CI/check workflows

**Triggers**: Push and Pull Request to `main` branch

**Projects covered**:
- Node.js projects: airi, airi-factorio, airi-minecraft, chat, deditor, eventa, gpuu, hf-inspector, hfup, std, talk, three-mmd, velin, xsai, xsai-transformers, xsai-use, xsmcp
- Go projects: inventory, mcp-launcher, unspeech

**Features**:
- Path filtering: Only runs CI for projects that have changes
- Parallel execution: Multiple jobs run in parallel using matrix strategies
- Grouped by pattern: Projects with similar CI patterns are grouped together
- Preserves all original functionality: lint, build, typecheck, tests, provenance checks

**Job categories**:
- `changes`: Detects which projects have changes
- `airi`: Special job for airi project with multiple build targets
- `node-std`: Standard Node.js projects (lint, build, typecheck)
- `node-check`: Simple check projects (lint + build)
- `node-advanced`: Advanced Node.js projects with tests
- `go-projects`: Go projects with lint, build, and tests
- `inventory-ts`: TypeScript part of inventory project

### 2. Release Workflow (`release.yml`)

**Consolidates**: 24 release/publish workflows

**Triggers**: Push with tags (`v*`), workflow_dispatch

**Projects covered**: airi, arpk, chat, cosine-similarity, eventa, gpuu, hfup, inventory, mcp-launcher, ortts, std, three-mmd, unspeech, velin, xsai, xsai-transformers, xsai-use, xsmcp

**Features**:
- Path filtering: Only runs releases for projects that have changes
- Multiple release types:
  - **npm**: Standard npm package publishing
  - **Docker**: Multi-platform Docker image builds (amd64, arm64)
  - **Go**: Releases using goreleaser
  - **Rust**: Crates.io publishing
- Changelog generation with changelogithub
- Provenance attestation for npm packages

**Job categories**:
- `changes`: Detects which projects have changes
- `npm-release`: Standard npm package releases
- `airi-npm`: airi npm packages (special build process)
- `airi-crates`: airi Rust crates
- `go-release`: Go releases with goreleaser
- `docker-release`: Docker multi-platform builds
- `inventory-npm`: inventory TypeScript SDK
- `inventory-docker`: inventory Docker image
- `arpk-release`: arpk Go + Docker release

### 3. Deploy Workflow (`deploy.yml`)

**Consolidates**: 10 deployment workflows

**Triggers**: Push to `main` branch, workflow_dispatch

**Projects covered**: airi, airi-factorio, blog, deck, hf-inspector, moeru-ai.github.io, std, talk, xsai, xsmcp

**Features**:
- Path filtering: Only runs deployments for projects that have changes
- Multiple deployment targets:
  - **HuggingFace Spaces**: For interactive web apps
  - **GitHub Pages**: For documentation and static sites
- Supports different build tools: pnpm, Deno, static files

**Job categories**:
- `changes`: Detects which projects have changes
- `hf-spaces`: HuggingFace Spaces deployments
- `gh-pages-pnpm`: GitHub Pages for pnpm projects
- `gh-pages-deno`: GitHub Pages for Deno projects (blog)
- `gh-pages-static`: GitHub Pages for static sites

### 4. Maintenance Workflow (`maintenance.yml`)

**Consolidates**: 4 maintenance workflows

**Triggers**: 
- Pull Request and Push to `main` branch (for autofix)
- Push to `main` branch (for nix updates)
- workflow_dispatch

**Projects covered**: airi, xsai

**Features**:
- **autofix**: Automatic linting fixes on pull requests
- **nix-assets-hash**: Updates Nix assets hash for airi
- **nix-pnpm-deps-hash**: Updates Nix pnpm dependencies hash for airi
- Uses autofix-ci for automated PR updates

**Job categories**:
- `changes`: Detects which projects have changes
- `autofix`: Runs lint --fix and creates automated PR updates
- `nix-assets-hash`: Updates Nix assets hash
- `nix-pnpm-deps-hash`: Updates Nix pnpm deps hash

## Special-Purpose Workflows (Kept Separate)

### 1. `airi-release-tamagotchi.yml`
Complex multi-platform Tauri desktop application release with:
- Multiple OS support: macOS (x64, arm64), Linux (x64, arm64), Windows (x64)
- Scheduled daily builds
- Custom artifact handling
- Flatpak support

### 2. `deditor-release-app.yml`
Complex multi-platform Tauri desktop application release with:
- Multiple OS support: macOS (x64, arm64), Linux (x64, arm64), Windows (x64)
- Scheduled daily builds
- Custom build processes

### 3. `inventory-model-collection.yml`
Issue-driven workflow that:
- Triggers on GitHub issues (opened, edited, reopened)
- Fetches model information from issues
- Special permissions for issue and PR management

### 4. `inventory-unstable-build.yml`
Manual Docker build workflow for unstable releases:
- Only triggered via workflow_dispatch
- Builds Docker images with `unstable` tag
- Used for testing and development

## How to Use

### Running CI

CI automatically runs when you:
- Push to `main` branch
- Create a pull request to `main` branch
- Modify files in any project directory

The workflow will automatically detect which projects have changes and only run CI for those projects.

### Creating Releases

To create a release:

1. Tag your commit with a version tag:
   ```bash
   git tag v1.0.0
   git push origin v1.0.0
   ```

2. The release workflow will automatically:
   - Detect which projects have changes
   - Build and publish packages (npm, Docker, Go, Rust)
   - Generate changelogs
   - Create GitHub releases (for Go projects)

Alternatively, trigger manually via GitHub Actions UI (workflow_dispatch).

### Deploying

Deployments automatically run when you:
- Push to `main` branch with changes to deployed projects

The workflow will:
- Build your project
- Deploy to HuggingFace Spaces or GitHub Pages

Alternatively, trigger manually via GitHub Actions UI.

### Maintenance

- **Autofix**: Runs automatically on pull requests, fixes linting issues
- **Nix updates**: Run automatically on push to `main` when relevant files change
- Can be triggered manually via workflow_dispatch

## Benefits

1. **Easier maintenance**: Changes to common patterns only need to be made in one place
2. **Better visibility**: All CI jobs for a commit are in one workflow run
3. **Cleaner workflow list**: 87% fewer workflows to navigate through
4. **Preserved functionality**: All original features are maintained
5. **Improved efficiency**: Matrix strategies enable parallel execution
6. **Reduced duplication**: Common setup steps are consolidated

## Troubleshooting

### A workflow didn't run for my project

Check that:
1. Your changes are in the correct project directory
2. The path is listed in the workflow's `paths` filter
3. The workflow trigger conditions are met (e.g., push to `main`)

### A job is being skipped

This is expected behavior when:
- The project didn't have any changes (path filtering)
- The job's condition evaluates to false

### Need to re-run a single project's CI

You can:
1. Use workflow_dispatch to manually trigger workflows
2. Re-run the entire workflow run from GitHub UI
3. Create a new commit that touches the specific project

## Migration Notes

If you have local branches or scripts that reference old workflow files:

**Old**: `.github/workflows/airi-ci.yml`  
**New**: `.github/workflows/ci.yml` (contains all CI jobs)

**Old**: `.github/workflows/airi-release-pkg.yaml`  
**New**: `.github/workflows/release.yml` (contains all release jobs)

**Old**: `.github/workflows/airi-deploy-huggingface-spaces.yml`  
**New**: `.github/workflows/deploy.yml` (contains all deployment jobs)

**Old**: `.github/workflows/airi-autofix.yaml`  
**New**: `.github/workflows/maintenance.yml` (contains all maintenance jobs)

## Future Improvements

Potential future enhancements:
1. Add workflow reusability for common patterns
2. Implement workflow concurrency controls for better resource usage
3. Add more granular failure notifications
4. Consider consolidating Tauri app releases if patterns emerge
