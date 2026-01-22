#!/bin/bash
echo "🧹 Cleaning up repository to fix 'Argument list too long' error..."
echo "📁 Removing large directories from git tracking..."
if [ -d "todo" ]; then
    echo "Removing todo/ from git tracking..."
    git rm -r --cached todo/ 2>/dev/null || echo "todo/ not tracked by git"
fi
if [ -d "deep_tree_echo_profile" ]; then
    echo "Removing deep_tree_echo_profile/ from git tracking..."
    git rm -r --cached deep_tree_echo_profile/ 2>/dev/null || echo "deep_tree_echo_profile/ not tracked by git"
fi
if [ -d "chrome_user_data" ]; then
    echo "Removing chrome_user_data/ from git tracking..."
    git rm -r --cached chrome_user_data/ 2>/dev/null || echo "chrome_user_data/ not tracked by git"
fi
if [ -d "browser_data" ]; then
    echo "Removing browser_data/ from git tracking..."
    git rm -r --cached browser_data/ 2>/dev/null || echo "browser_data/ not tracked by git"
fi
echo "📊 Current file count in repository:"
find . -type f | wc -l
echo "✅ Repository cleanup complete!"
echo ""
echo "Next steps:"
echo "1. Commit these changes: git add . && git commit -m 'Clean up repository to fix argument list too long error'"
echo "2. Push the changes: git push"
echo "3. The GitHub Actions should now work without the 'Argument list too long' error"
echo ""
echo "Note: The large directories are still present locally but are now ignored by git."