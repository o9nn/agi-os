# OCC Build Workflow Documentation

## Overview

The `occ-build.yml` GitHub Actions workflow provides a comprehensive build system for the OpenCog Collection (OCC) monorepo. This workflow was generated from the existing CircleCI configurations and CMakeLists.txt files found throughout the repository.

## Workflow Structure

### Trigger Events

The workflow is triggered on:
- Push to `main` or `master` branches
- Pull requests to `main` or `master` branches
- Manual workflow dispatch

### Build Stages

The workflow builds components in the following dependency order:

#### Stage 1: Foundation
- **CogUtil**: Core utility library providing thread-safe data structures and OS portability

#### Stage 2: Core Database
- **AtomSpace**: Hypergraph knowledge representation database with query engine

#### Stage 3: Storage Backends
- **AtomSpace Storage**: Base storage interface
- **AtomSpace Rocks**: RocksDB-based persistent storage
- **AtomSpace Cog**: Network-based distributed storage

#### Stage 4: Networking
- **CogServer**: Network shell and WebSocket API for AtomSpace

#### Stage 5-9: Reasoning & Learning
- **Unify**: Unification engine for pattern matching
- **URE**: Unified Rule Engine for forward/backward chaining
- **Miner**: Pattern mining for frequent subgraph discovery
- **Attention**: Economic attention allocation mechanism
- **AS-MOSES**: Meta-Optimizing Semantic Evolutionary Search

#### Stage 10-13: Extended Components
- **Matrix**: Sparse vector/matrix operations for graph embeddings
- **SpaceTime**: Spatiotemporal representation system
- **PLN**: Probabilistic Logic Networks reasoning
- **Learn**: Language learning and natural language processing

### Build Features

#### Dependency Management
Each job installs required dependencies:
- CMake 3.12+
- C++ build tools (gcc/g++)
- CxxTest for unit testing
- Boost libraries (filesystem, program-options, system, thread, regex)
- Guile 3.0 for Scheme integration
- Component-specific dependencies (RocksDB, ASIO, etc.)

#### Caching Strategy
The workflow uses GitHub Actions caching for:
- **CCache**: Compiler cache for faster rebuilds
- **GHC**: Glasgow Haskell Compiler for Haskell bindings
- **Haskell Dependencies**: Stack-based dependency cache

Cache keys are based on:
- Operating system
- Component name
- Hash of relevant source files

#### Testing
Each component:
1. Builds tests with `make tests`
2. Runs tests with `make check`
3. Uses `continue-on-error: true` to allow builds to complete even if tests fail
4. Prints test logs for debugging

#### Build Artifacts
- Shared libraries (`.so` files) are uploaded as artifacts
- Artifacts are retained for 1 day
- Final build report is retained for 30 days

## Usage

### Running the Workflow

The workflow runs automatically on pushes and pull requests. To manually trigger:

1. Go to the Actions tab in GitHub
2. Select "OCC Build - Complete Stack"
3. Click "Run workflow"
4. Select the branch
5. Click "Run workflow"

### Viewing Results

After the workflow completes:

1. **Check Job Status**: Each component build is a separate job
2. **View Logs**: Click on any job to see detailed build logs
3. **Download Artifacts**: Access build artifacts from the workflow summary
4. **Read Build Report**: Download the `occ-build-report` artifact for a summary

### Build Report Contents

The final build report includes:
- Build timestamp
- List of artifacts generated
- Complete build order with component descriptions
- Notes about the build process

## Customization

### Adding New Components

To add a new component to the workflow:

1. Add a new job after its dependencies
2. Update the `needs:` field to list dependencies
3. Follow the existing pattern:
   - Install dependencies
   - Rebuild and install prerequisite components
   - Configure with CMake
   - Build with make
   - Run tests
   - Install

4. Add the new job to the `needs:` list of `package-and-report`

Example:
```yaml
build-new-component:
  runs-on: ubuntu-latest
  name: Build New Component
  needs: build-atomspace  # or other dependency
  
  steps:
  - name: Checkout Repository
    uses: actions/checkout@v4
  
  # ... rest of the steps following the pattern
```

### Modifying Build Options

To change CMake configuration options:

1. Find the "Configure" step for the component
2. Add CMake options to the cmake command:
   ```bash
   cmake .. -DCMAKE_BUILD_TYPE=${{ env.BUILD_TYPE }} -DOPTION_NAME=VALUE
   ```

### Adjusting Parallelism

The `MAKEFLAGS` environment variable controls parallel builds:
```yaml
env:
  MAKEFLAGS: -j2  # Use 2 parallel jobs
```

Increase this value for faster builds on machines with more CPU cores.

## Differences from CircleCI

### Key Changes

1. **Job Parallelization**: GitHub Actions runs independent jobs in parallel automatically
2. **Workspace Sharing**: Uses artifact upload/download instead of workspace persistence
3. **Caching**: Uses GitHub Actions cache instead of CircleCI cache
4. **Docker Images**: Uses Ubuntu latest instead of custom Docker images
5. **Dependency Installation**: Installs dependencies in each job instead of using pre-built images

### Compatibility

The workflow maintains compatibility with CircleCI by:
- Following the same build order
- Using the same CMake commands
- Running the same tests
- Installing to the same system locations

## Troubleshooting

### Build Failures

If a component fails to build:

1. Check the job logs for error messages
2. Verify dependencies are correctly installed
3. Ensure prerequisite components built successfully
4. Check if the component's CMakeLists.txt exists
5. Review the conditional checks (`if: hashFiles(...)`)

### Missing Dependencies

If builds fail due to missing dependencies:

1. Update the "Install Dependencies" step
2. Add required packages to the `apt-get install` command
3. Test locally with Docker if needed

### Test Failures

Tests are allowed to fail (`continue-on-error: true`) to ensure builds complete. To fix test failures:

1. Review the test logs
2. Run tests locally: `cd <component>/build && make check`
3. Fix the underlying issues in the component code
4. Remove `continue-on-error` once tests pass reliably

## Performance Optimization

### Build Time Optimization

1. **Enable CCache**: Already configured for faster rebuilds
2. **Increase Parallelism**: Adjust `MAKEFLAGS` to use more cores
3. **Use Self-Hosted Runners**: For faster builds on dedicated hardware
4. **Cache More Aggressively**: Add more paths to cache configuration

### Resource Usage

Current setup:
- Uses `ubuntu-latest` runners
- 2 parallel make jobs
- Multiple jobs running concurrently

For large builds, consider:
- Self-hosted runners with more resources
- Sequential builds if hitting resource limits
- Splitting into multiple workflows

## Integration with Other Workflows

The OCC build workflow can be integrated with:

- **CodeQL**: Security scanning (already in repository)
- **Docker**: Building container images with built artifacts
- **Release**: Creating releases with build artifacts
- **Documentation**: Building and deploying documentation

## Maintenance

### Regular Updates

- Update dependency versions as needed
- Adjust for new components added to the monorepo
- Monitor build times and optimize as needed
- Update cache strategies based on hit rates

### Version Compatibility

- CMake: Minimum 3.12, tested with latest
- Ubuntu: Uses `ubuntu-latest` (currently 22.04)
- Boost: System-provided version
- Guile: 3.0 series

## References

- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [OpenCog Wiki](https://wiki.opencog.org/)
- [CMake Documentation](https://cmake.org/documentation/)
- [CircleCI Migration Guide](https://docs.github.com/en/actions/migrating-to-github-actions/migrating-from-circleci-to-github-actions)

## Contributing

To contribute improvements to this workflow:

1. Test changes locally using [act](https://github.com/nektos/act) or in a fork
2. Update this documentation for significant changes
3. Submit a pull request with a clear description
4. Ensure all jobs pass before merging

## License

This workflow is part of the OpenCog Collection and follows the same license terms as the repository.
