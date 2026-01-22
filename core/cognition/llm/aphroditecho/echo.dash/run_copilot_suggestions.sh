#!/bin/bash
if [ -f .env ]; then
source .env
fi
if [ -z "$GITHUB_TOKEN" ]; then
echo "GITHUB_TOKEN environment variable is not set."
echo "Please enter your GitHub token (it will not be saved):"
read -s GITHUB_TOKEN
echo ""
if [ -z "$GITHUB_TOKEN" ]; then
echo "No token provided. Exiting."
exit 1
fi
fi
export GITHUB_TOKEN
python copilot_suggestions.py
unset GITHUB_TOKEN