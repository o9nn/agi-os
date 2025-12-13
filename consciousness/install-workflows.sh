#!/bin/bash
# Install Autonomous Agent Workflows
# This script copies workflow files from docs/github-actions/ to .github/workflows/

set -e

echo "🔧 Installing Autonomous Agent Workflows"
echo "========================================"
echo ""

# Check if we're in the right directory
if [ ! -d "docs/github-actions" ]; then
    echo "❌ Error: docs/github-actions/ directory not found"
    echo "   Please run this script from the echo9llama repository root"
    exit 1
fi

# Check if workflow templates exist
if [ ! -f "docs/github-actions/autonomous-agent-test.yml" ]; then
    echo "❌ Error: Workflow templates not found in docs/github-actions/"
    exit 1
fi

echo "📋 Checking current state..."
echo ""

# Show current workflows
echo "Current workflows in .github/workflows/:"
ls -1 .github/workflows/ | grep -E '\.ya?ml$' || echo "  (none found)"
echo ""

# Copy workflow files
echo "📦 Copying workflow files..."
cp -v docs/github-actions/autonomous-agent-test.yml .github/workflows/
cp -v docs/github-actions/autonomous-agent-ci.yml .github/workflows/
echo ""

# Show what was added
echo "✅ Workflows installed:"
echo "  • autonomous-agent-test.yml"
echo "  • autonomous-agent-ci.yml"
echo ""

# Check git status
echo "📊 Git status:"
git status --short .github/workflows/
echo ""

# Offer to commit
read -p "Would you like to commit and push these workflows? (y/n) " -n 1 -r
echo ""
if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo ""
    echo "📝 Committing workflows..."
    git add .github/workflows/autonomous-agent-test.yml
    git add .github/workflows/autonomous-agent-ci.yml
    
    git commit -m "Add autonomous agent GitHub Actions workflows

- autonomous-agent-test.yml: Main test suite
  * Tests on Go 1.21 and 1.22
  * Builds echoself binary
  * Runs standard Go tests with race detection
  * Runs autonomous agent integration tests
  * Integration test with real LLM (manual/main only)
  * Uploads coverage to Codecov

- autonomous-agent-ci.yml: CI quality checks
  * Code formatting and linting
  * Security scanning
  * Dependency vulnerability checks
  * Multi-platform builds

Workflows use repository secrets for API keys:
ANTHROPIC_API_KEY, OPENROUTER_API_KEY, OPENAI_API_KEY"
    
    echo ""
    echo "🚀 Pushing to GitHub..."
    git push origin main
    
    echo ""
    echo "✅ Workflows successfully installed and pushed!"
    echo ""
    echo "Next steps:"
    echo "1. Add API key secrets at: https://github.com/cogpy/echo9llama/settings/secrets/actions"
    echo "2. View workflows at: https://github.com/cogpy/echo9llama/actions"
else
    echo ""
    echo "⏸️  Workflows copied but not committed."
    echo "   Run 'git add .github/workflows/*.yml' and commit when ready."
fi

echo ""
echo "🌳 Deep Tree Echo workflows ready!"
