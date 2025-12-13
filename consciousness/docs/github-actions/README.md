# GitHub Actions Workflows for Echo9llama

This directory contains GitHub Actions workflows for automated testing, building, and quality assurance of the echo9llama project.

## Workflows

### 1. `test.yml` - Main Test Suite

**Triggers:**
- Push to `main` or `develop` branches
- Pull requests to `main` or `develop` branches
- Manual workflow dispatch

**Jobs:**

#### `test`
- Runs on: Ubuntu Latest
- Go versions: 1.21, 1.22
- Steps:
  1. Checkout code
  2. Set up Go environment
  3. Download and verify dependencies
  4. Build `echoself` binary
  5. Run standard Go tests with race detection and coverage
  6. Build and run autonomous agent test suite
  7. Upload coverage to Codecov
  8. Archive test artifacts

**Environment Variables:**
- `ANTHROPIC_API_KEY` (from repository secrets)
- `OPENROUTER_API_KEY` (from repository secrets)
- `OPENAI_API_KEY` (from repository secrets)

#### `build-verification`
- Verifies all packages build successfully
- Runs `go vet` for common issues
- Runs `staticcheck` for static analysis

#### `integration-test`
- Runs only on manual dispatch or push to `main`
- Tests echoself with real LLM provider for 30 seconds
- Validates autonomous operation with actual API calls

---

### 2. `ci.yml` - Continuous Integration

**Triggers:**
- Push to `main` or `develop` branches
- Pull requests to `main` or `develop` branches

**Jobs:**

#### `lint`
- Checks code formatting with `gofmt`
- Runs `go vet` for common mistakes
- Runs `golangci-lint` for comprehensive linting

#### `security`
- Runs `gosec` security scanner
- Uploads security scan results as artifacts

#### `dependency-check`
- Checks for known vulnerabilities with `govulncheck`
- Lists outdated dependencies

#### `build-matrix`
- Builds on multiple platforms: Ubuntu, macOS, Windows
- Verifies cross-platform compatibility

---

## Setting Up Repository Secrets

To enable the workflows, add the following secrets to your GitHub repository:

1. Go to **Settings** → **Secrets and variables** → **Actions**
2. Click **New repository secret**
3. Add the following secrets:

| Secret Name | Description |
|-------------|-------------|
| `ANTHROPIC_API_KEY` | Anthropic Claude API key |
| `OPENROUTER_API_KEY` | OpenRouter API key |
| `OPENAI_API_KEY` | OpenAI API key |

**Note:** At least one API key is required for the integration tests to run successfully.

---

## Workflow Behavior

### Test Execution

The test suite includes:

1. **Unit Tests**: Standard Go tests with race detection
2. **Integration Tests**: Autonomous agent tests (with mock LLM by default)
3. **Real LLM Tests**: Optional integration test with actual API calls (manual or main branch only)

### Timeouts

- Autonomous agent tests: 60 seconds
- Integration tests with real LLM: 30 seconds

Timeouts are expected and handled gracefully, as the autonomous agent is designed to run continuously.

### Coverage

Test coverage is automatically uploaded to Codecov after each test run.

---

## Running Tests Locally

To run the same tests locally:

```bash
# Set API keys (optional for unit tests)
export ANTHROPIC_API_KEY="your-key"
export OPENROUTER_API_KEY="your-key"
export OPENAI_API_KEY="your-key"

# Run standard tests
go test -v -race -coverprofile=coverage.txt ./...

# Build and run autonomous agent tests
go build -o test_autonomous test_autonomous_agent_nov22.go
./test_autonomous

# Build echoself
go build -o echoself cmd/echoself/main.go
./echoself
```

---

## Artifacts

The workflows generate the following artifacts:

| Artifact | Retention | Description |
|----------|-----------|-------------|
| `test-artifacts-go-X.XX` | 7 days | Built binaries and coverage reports |
| `security-scan-results` | 7 days | Security scan JSON report |

---

## Status Badges

Add these badges to your README.md:

```markdown
[![Tests](https://github.com/cogpy/echo9llama/actions/workflows/test.yml/badge.svg)](https://github.com/cogpy/echo9llama/actions/workflows/test.yml)
[![CI](https://github.com/cogpy/echo9llama/actions/workflows/ci.yml/badge.svg)](https://github.com/cogpy/echo9llama/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/cogpy/echo9llama/branch/main/graph/badge.svg)](https://codecov.io/gh/cogpy/echo9llama)
```

---

## Troubleshooting

### Tests Failing Due to Missing API Keys

If you see errors about missing LLM providers, ensure at least one API key secret is configured. The test suite will use a mock provider if no keys are available, but integration tests require real keys.

### Build Failures

Check the build logs for specific errors. Common issues:
- Missing dependencies: Run `go mod download`
- Go version mismatch: Ensure Go 1.21+ is installed
- Import path issues: Verify module path in `go.mod`

### Timeout Issues

The autonomous agent is designed to run continuously. Timeouts in tests are expected and handled. If tests fail for other reasons, check the logs for specific error messages.

---

*Last updated: November 22, 2025*
