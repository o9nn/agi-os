# Autonomous Agent Workflows - Installation Instructions

The autonomous agent test workflows are currently in `docs/github-actions/` and need to be added to `.github/workflows/` to activate them.

## Why Manual Installation?

GitHub App tokens (used by Manus) don't have permission to create or modify workflow files for security reasons. You need to install them manually using one of the methods below.

---

## Method 1: Automated Script (Recommended)

If you have local access to the repository:

```bash
# Clone the repository (if not already cloned)
git clone https://github.com/cogpy/echo9llama.git
cd echo9llama

# Run the installation script
./install-workflows.sh
```

The script will:
- ✅ Copy workflow files to `.github/workflows/`
- ✅ Show you what changed
- ✅ Offer to commit and push automatically

---

## Method 2: Manual Copy via Command Line

```bash
# Navigate to repository
cd echo9llama

# Copy workflow files
cp docs/github-actions/autonomous-agent-test.yml .github/workflows/
cp docs/github-actions/autonomous-agent-ci.yml .github/workflows/

# Commit and push
git add .github/workflows/autonomous-agent-*.yml
git commit -m "Add autonomous agent workflows"
git push origin main
```

---

## Method 3: GitHub Web Interface

### For autonomous-agent-test.yml:

1. Go to: https://github.com/cogpy/echo9llama/new/main?filename=.github/workflows/autonomous-agent-test.yml
2. Copy the content from `docs/github-actions/autonomous-agent-test.yml`
3. Paste into the editor
4. Scroll down and click **Commit changes**

### For autonomous-agent-ci.yml:

1. Go to: https://github.com/cogpy/echo9llama/new/main?filename=.github/workflows/autonomous-agent-ci.yml
2. Copy the content from `docs/github-actions/autonomous-agent-ci.yml`
3. Paste into the editor
4. Scroll down and click **Commit changes**

---

## Method 4: Direct File Content (Copy-Paste)

If you prefer to create the files directly via GitHub web interface, the complete file contents are available in:

- `docs/github-actions/autonomous-agent-test.yml` (148 lines)
- `docs/github-actions/autonomous-agent-ci.yml` (139 lines)

You can view these files on GitHub and copy their contents.

---

## After Installation

### 1. Set Up Repository Secrets

The workflows require API keys. Add at least one:

1. Go to: https://github.com/cogpy/echo9llama/settings/secrets/actions
2. Click **New repository secret**
3. Add one or more of:
   - `ANTHROPIC_API_KEY` (recommended)
   - `OPENROUTER_API_KEY`
   - `OPENAI_API_KEY`

### 2. Verify Installation

1. Go to: https://github.com/cogpy/echo9llama/actions
2. You should see two new workflows:
   - **Autonomous Agent Tests**
   - **Autonomous Agent CI**

### 3. Test the Workflows

Trigger a manual test run:

1. Click on **Autonomous Agent Tests**
2. Click **Run workflow**
3. Select branch: `main`
4. Click **Run workflow**

This will run the full test suite including integration tests with real LLM.

---

## What Gets Installed

### autonomous-agent-test.yml
- Runs on: Push/PR to main or develop, manual dispatch
- Jobs:
  - **test**: Builds echoself, runs Go tests, runs autonomous agent tests
  - **build-verification**: Verifies all packages build, runs static analysis
  - **integration-test**: Tests with real LLM (manual/main only)

### autonomous-agent-ci.yml
- Runs on: Push/PR to main or develop
- Jobs:
  - **lint**: Code formatting and linting checks
  - **security**: Security scanning with gosec
  - **dependency-check**: Vulnerability scanning
  - **build-matrix**: Multi-platform builds (Ubuntu, macOS, Windows)

---

## Troubleshooting

### "Workflows not appearing in Actions tab"

- Ensure files are in `.github/workflows/` (not `docs/github-actions/`)
- Check file extensions are `.yml` (not `.yaml`)
- Verify YAML syntax is valid
- Wait a few seconds and refresh the page

### "Tests failing due to missing API keys"

- Add at least one API key secret (see step 1 above)
- The test suite will use mock LLM if no keys are available
- Integration tests require real API keys

### "Permission denied when pushing"

- Ensure you have write access to the repository
- If using HTTPS, check your GitHub credentials
- If using SSH, verify your SSH key is added to GitHub

---

## Quick Reference

| File | Purpose | Triggers |
|------|---------|----------|
| `autonomous-agent-test.yml` | Main test suite | Push, PR, Manual |
| `autonomous-agent-ci.yml` | Quality checks | Push, PR |

**Required Secrets:** At least one of `ANTHROPIC_API_KEY`, `OPENROUTER_API_KEY`, `OPENAI_API_KEY`

**Documentation:** See `docs/github-actions/README.md` for full workflow details

---

## Status Badges

After installation, add these to your README.md:

```markdown
[![Autonomous Agent Tests](https://github.com/cogpy/echo9llama/actions/workflows/autonomous-agent-test.yml/badge.svg)](https://github.com/cogpy/echo9llama/actions/workflows/autonomous-agent-test.yml)
[![Autonomous Agent CI](https://github.com/cogpy/echo9llama/actions/workflows/autonomous-agent-ci.yml/badge.svg)](https://github.com/cogpy/echo9llama/actions/workflows/autonomous-agent-ci.yml)
```

---

## Need Help?

- Check the workflow documentation: `docs/github-actions/README.md`
- Review the installation guide: `docs/github-actions/INSTALLATION.md`
- See the iteration report: `ITERATION_REPORT_NOV22_2025.md`

---

*Installation guide created: November 22, 2025*
