# Moeru AI Monorepo Technical Documentation

## Overview

This monorepo contains all Moeru AI projects, integrated as a coherent whole. Each project maintains its original structure and independence while benefiting from unified version control and cross-project coordination.

## Integration Process

All repositories were integrated using the following process:

1. **Cloning**: Each repository was cloned from its original GitHub location
2. **Git History Removal**: The `.git` directory was removed from each cloned repository to prevent submodule conflicts
3. **Directory Structure**: Each project is placed in its own directory at the root level

## Project Organization

### By Technology

#### TypeScript/JavaScript Projects
- airi, xsai, std, eventa, airi-factorio, three-mmd, xsai-transformers
- deditor, velin, hfup, gpuu, chat, xsmcp, xsai-use
- blog, moeru-ai.github.io, cosine-similarity, arpk, airi-minecraft
- talk, deck

#### Rust Projects
- ortts, reproduction-ort-session-segmentation-fault

#### Go Projects
- inventory, unspeech, demodel, mcp-launcher

#### Vue Projects
- airi (Vue-based), hf-inspector

#### Other
- L3.1-Moe (Nix)
- easiest (Docker Compose)

### By Status

#### Active Projects
Most projects are actively maintained and developed.

#### Archived Projects
- `xsmcp` - extra-small MCP SDK (archived)
- `airi-minecraft` - Merged into Airi
- `reproduction-ort-session-segmentation-fault` - Bug reproduction repository

## Working with the Monorepo

### Navigating Projects

Each project is in its own directory. Navigate to any project:

```bash
cd <project-name>
```

### Building Individual Projects

Each project maintains its own build system. Refer to the README.md in each project directory for specific build instructions.

Common patterns:

**TypeScript/Node.js:**
```bash
npm install
npm run build
```

**Rust:**
```bash
cargo build
```

**Go:**
```bash
go build
```

### Development Workflow

1. Navigate to the specific project directory
2. Follow the project's individual development setup
3. Make changes as needed
4. Test within the project context
5. Commit changes to the monorepo

### Cross-Project Dependencies

Some projects may depend on others within this monorepo. When working on such projects:

1. Ensure dependent projects are built first
2. Update import paths if necessary
3. Test integration between projects

## Maintenance

### Adding New Projects

To add a new project to the monorepo:

1. Clone the repository
2. Remove the `.git` directory
3. Move it to the root level
4. Update README.md to include the new project
5. Commit the changes

### Updating Projects

Projects can be updated independently. Changes should be made in their respective directories and committed to the monorepo.

## Best Practices

1. **Keep project independence**: Each project should remain independently buildable and runnable
2. **Respect project boundaries**: Don't mix code between projects unless there's a clear dependency
3. **Document changes**: Update project-specific documentation when making changes
4. **Test thoroughly**: Ensure changes don't break other projects that may depend on the modified project

## Version Control

The monorepo uses a single Git repository for all projects. This allows for:

- Atomic commits across multiple projects
- Unified versioning when needed
- Simplified dependency management
- Easier coordination of breaking changes

## CI/CD Considerations

When setting up CI/CD for the monorepo:

1. Consider using path-based triggers to only build affected projects
2. Set up separate build jobs for different project types
3. Implement cross-project dependency checking
4. Use caching to speed up builds

## Migration Notes

This monorepo was created by integrating the following repositories:

- All repositories were cloned from github.com/moeru-ai
- Git history was removed from each project (`.git` directories deleted)
- Projects maintain their original directory structures
- Original README files and documentation are preserved

## Future Considerations

- Consider setting up workspace management (npm workspaces, Cargo workspaces, Go modules)
- Implement unified tooling for common operations
- Set up automated dependency updates
- Create shared configuration files where appropriate
