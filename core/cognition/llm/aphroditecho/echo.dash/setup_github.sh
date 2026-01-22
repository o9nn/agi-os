#!/bin/bash
ssh-keygen -t ed25519 -C "deep.tree.echo@shells.com" -f ~/.ssh/github_key -N ""
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/github_key
echo "Copy this public key to GitHub (https://github.com/settings/ssh/new):"
echo "-------------------"
cat ~/.ssh/github_key.pub
echo "-------------------"
git init
git config --global user.name "Deep Tree Echo"
git config --global user.email "deep.tree.echo@shells.com"
git add .
git commit -m "Initial commit: Deep Tree Echo project setup"
echo ""
echo "After adding the SSH key to GitHub:"
echo "1. Create a new repository on GitHub (don't initialize with README)"
echo "2. Run these commands (replace YOUR_REPO with your repository name):"
echo ""
echo "git remote add origin git@github.com:YOUR_USERNAME/YOUR_REPO.git"
echo "git branch -M main"
echo "git push -u origin main"