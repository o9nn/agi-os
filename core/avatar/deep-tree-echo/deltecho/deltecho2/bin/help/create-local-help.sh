#!/bin/bash
set -e
../deltachat-pages/tools/create-local-help.py ../deltachat-pages/result static/help --add_pagefind
rm -rf ./static/help/pagefind
npx pagefind --site ./static/help/
node ./bin/help/help-translations.js
echo
echo "☝️ Compliance Warning: Add the following line to CHANGELOG.md:"
echo "- Update local help ("`date "+%Y-%m-%d"`")"
echo