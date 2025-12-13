# Repository Integration Summary

This document tracks the repositories that have been integrated into the monorepo.

## Integration Date
November 10, 2025

## Total Repositories Integrated
30 repositories

## Repository List

| # | Repository Name | Original URL | Status | Primary Language |
|---|----------------|--------------|--------|------------------|
| 1 | airi | https://github.com/moeru-ai/airi | ✅ Integrated | Vue |
| 2 | ortts | https://github.com/moeru-ai/ortts | ✅ Integrated | Rust |
| 3 | xsai | https://github.com/moeru-ai/xsai | ✅ Integrated | TypeScript |
| 4 | std | https://github.com/moeru-ai/std | ✅ Integrated | TypeScript |
| 5 | eventa | https://github.com/moeru-ai/eventa | ✅ Integrated | TypeScript |
| 6 | airi-factorio | https://github.com/moeru-ai/airi-factorio | ✅ Integrated | TypeScript |
| 7 | reproduction-ort-session-segmentation-fault | https://github.com/moeru-ai/reproduction-ort-session-segmentation-fault | ✅ Integrated (archived) | Rust |
| 8 | three-mmd | https://github.com/moeru-ai/three-mmd | ✅ Integrated | TypeScript |
| 9 | xsai-transformers | https://github.com/moeru-ai/xsai-transformers | ✅ Integrated | TypeScript |
| 10 | deditor | https://github.com/moeru-ai/deditor | ✅ Integrated | TypeScript |
| 11 | velin | https://github.com/moeru-ai/velin | ✅ Integrated | TypeScript |
| 12 | hfup | https://github.com/moeru-ai/hfup | ✅ Integrated | TypeScript |
| 13 | gpuu | https://github.com/moeru-ai/gpuu | ✅ Integrated | TypeScript |
| 14 | hf-inspector | https://github.com/moeru-ai/hf-inspector | ✅ Integrated | Vue |
| 15 | inventory | https://github.com/moeru-ai/inventory | ✅ Integrated | Go |
| 16 | unspeech | https://github.com/moeru-ai/unspeech | ✅ Integrated | Go |
| 17 | .github | https://github.com/moeru-ai/.github | ✅ Integrated | - |
| 18 | chat | https://github.com/moeru-ai/chat | ✅ Integrated | TypeScript |
| 19 | xsmcp | https://github.com/moeru-ai/xsmcp | ✅ Integrated (archived) | TypeScript |
| 20 | xsai-use | https://github.com/moeru-ai/xsai-use | ✅ Integrated | TypeScript |
| 21 | blog | https://github.com/moeru-ai/blog | ✅ Integrated | TypeScript |
| 22 | demodel | https://github.com/moeru-ai/demodel | ✅ Integrated | Go |
| 23 | moeru-ai.github.io | https://github.com/moeru-ai/moeru-ai.github.io | ✅ Integrated | TypeScript |
| 24 | mcp-launcher | https://github.com/moeru-ai/mcp-launcher | ✅ Integrated | Go |
| 25 | cosine-similarity | https://github.com/moeru-ai/cosine-similarity | ✅ Integrated | TypeScript |
| 26 | arpk | https://github.com/moeru-ai/arpk | ✅ Integrated | TypeScript |
| 27 | airi-minecraft | https://github.com/moeru-ai/airi-minecraft | ✅ Integrated (archived) | TypeScript |
| 28 | talk | https://github.com/moeru-ai/talk | ✅ Integrated | TypeScript |
| 29 | deck | https://github.com/moeru-ai/deck | ✅ Integrated | TypeScript |
| 30 | L3.1-Moe | https://github.com/moeru-ai/L3.1-Moe | ✅ Integrated | Nix |
| 31 | easiest | https://github.com/moeru-ai/easiest | ✅ Integrated | Docker Compose |

## Integration Process

Each repository was:
1. Cloned from its original GitHub URL
2. Had its `.git` directory removed to prevent submodule conflicts
3. Placed in the root directory of the monorepo
4. Committed as part of the unified repository

## Verification

- ✅ All 30 repositories successfully cloned
- ✅ All `.git` directories removed from cloned repositories
- ✅ Root monorepo `.git` directory preserved
- ✅ Documentation files created (README.md, MONOREPO.md)
- ✅ .gitignore configured for common patterns
- ✅ All files committed to monorepo

## Language Distribution

- TypeScript/JavaScript: 19 projects
- Go: 4 projects
- Rust: 2 projects
- Vue: 2 projects
- Nix: 1 project
- Docker Compose: 1 project
- Other: 1 project (.github)

## Special Notes

### Archived Repositories
Three repositories are marked as archived in their original locations:
- reproduction-ort-session-segmentation-fault
- xsmcp
- airi-minecraft (merged into Airi)

These have been included in the monorepo for completeness and historical reference.

## Post-Integration

All repositories are now part of a unified monorepo structure, allowing for:
- Atomic commits across projects
- Simplified dependency management
- Unified version control
- Cross-project coordination
