# GitHub Actions Workflow Installation Guide

## Overview

This directory contains GitHub Actions workflows for automated testing of the echo9llama autonomous agent. Due to GitHub App permissions, these workflows cannot be automatically pushed to the repository and must be manually added.

## Files Included

1. **autonomous-agent-test.yml** - Main test suite
2. **autonomous-agent-ci.yml** - Continuous integration checks
3. **README.md** - Workflow documentation

## Installation Steps

### Step 1: Set Up Repository Secrets

Before installing the workflows, you need to add API keys as repository secrets:

1. Go to your repository on GitHub: https://github.com/cogpy/echo9llama
2. Click **Settings** → **Secrets and variables** → **Actions**
3. Click **New repository secret**
4. Add the following secrets (at least one is required):

| Secret Name | Description | Required |
|-------------|-------------|----------|
| `ANTHROPIC_API_KEY` | Anthropic Claude API key | Recommended |
| `OPENROUTER_API_KEY` | OpenRouter API key | Optional |
| `OPENAI_API_KEY` | OpenAI API key | Optional |

### Step 2: Add Workflow Files

You have two options for adding the workflow files:

#### Option A: Using GitHub Web Interface

1. Navigate to your repository: https://github.com/cogpy/echo9llama
2. Click on the `.github/workflows/` directory
3. Click **Add file** → **Create new file**
4. Name the file `autonomous-agent-test.yml`
5. Copy the contents from `docs/github-actions/autonomous-agent-test.yml`
6. Paste into the editor
7. Click **Commit changes**
8. Repeat for `autonomous-agent-ci.yml`

#### Option B: Using Git Locally

If you have direct push access (not via GitHub App):

```bash
# Clone the repository
git clone https://github.com/cogpy/echo9llama.git
cd echo9llama

# Copy workflow files
cp docs/github-actions/autonomous-agent-test.yml .github/workflows/
cp docs/github-actions/autonomous-agent-ci.yml .github/workflows/
cp docs/github-actions/README.md .github/workflows/

# Commit and push
git add .github/workflows/autonomous-agent-test.yml
git add .github/workflows/autonomous-agent-ci.yml
git add .github/workflows/README.md
git commit -m "Add autonomous agent GitHub Actions workflows"
git push origin main
```

### Step 3: Verify Installation

1. Go to your repository on GitHub
2. Click the **Actions** tab
3. You should see the new workflows:
   - "Autonomous Agent Tests"
   - "Autonomous Agent CI"
4. The workflows will automatically run on the next push to `main` or `develop`

### Step 4: Manual Test Run (Optional)

To verify the workflows work correctly:

1. Go to **Actions** tab
2. Click on "Autonomous Agent Tests"
3. Click **Run workflow** dropdown
4. Select branch `main`
5. Click **Run workflow**

This will trigger a test run including the integration test with real LLM.

## Workflow Triggers

### Autonomous Agent Tests (`autonomous-agent-test.yml`)

Runs on:
- Push to `main` or `develop` branches
- Pull requests to `main` or `develop` branches
- Manual workflow dispatch

### Autonomous Agent CI (`autonomous-agent-ci.yml`)

Runs on:
- Push to `main` or `develop` branches
- Pull requests to `main` or `develop` branches

## What Gets Tested

### Test Suite Job
- ✅ Builds `echoself` binary
- ✅ Runs standard Go tests with race detection
- ✅ Runs autonomous agent integration tests
- ✅ Generates test coverage reports
- ✅ Uploads coverage to Codecov

### Build Verification Job
- ✅ Verifies all packages build successfully
- ✅ Runs `go vet` for common issues
- ✅ Runs `staticcheck` for static analysis

### Integration Test Job (manual/main only)
- ✅ Tests echoself with real LLM for 30 seconds
- ✅ Validates autonomous operation

### CI Jobs
- ✅ Code formatting checks
- ✅ Linting with golangci-lint
- ✅ Security scanning with gosec
- ✅ Dependency vulnerability checks
- ✅ Multi-platform builds (Ubuntu, macOS, Windows)

## Troubleshooting

### Workflows Not Appearing

If workflows don't appear in the Actions tab:
- Ensure files are in `.github/workflows/` directory
- Check that YAML syntax is valid
- Verify files have `.yml` extension

### Tests Failing

Common issues:
- **Missing API keys**: Ensure at least one API key secret is configured
- **Build failures**: Check Go version (requires 1.21+)
- **Timeout issues**: Expected for autonomous agent tests; check logs for actual errors

### Permission Issues

If you see permission errors:
- Ensure you have write access to the repository
- Check that GitHub Actions is enabled in repository settings
- Verify secrets are properly configured

## Status Badges

After installation, add these badges to your `README.md`:

```markdown
[![Autonomous Agent Tests](https://github.com/cogpy/echo9llama/actions/workflows/autonomous-agent-test.yml/badge.svg)](https://github.com/cogpy/echo9llama/actions/workflows/autonomous-agent-test.yml)
[![Autonomous Agent CI](https://github.com/cogpy/echo9llama/actions/workflows/autonomous-agent-ci.yml/badge.svg)](https://github.com/cogpy/echo9llama/actions/workflows/autonomous-agent-ci.yml)
```

## Support

For issues or questions:
- Check the workflow README: `docs/github-actions/README.md`
- Review GitHub Actions logs in the Actions tab
- Consult the iteration report: `ITERATION_REPORT_NOV22_2025.md`

---

*Installation guide created: November 22, 2025*
