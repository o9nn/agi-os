#! /bin/bash
echo "Category:"
find . -name 'Category:*' -print | wc
find . -name 'MediaWiki:*' -print | wc
find . -name 'Help:*' -print | wc
echo "File:"
find . -name 'File:*' -print | wc
find . -name 'Image:*' -print | wc
echo "Template"
find . -name 'Template:*' -print | wc
find . -name 'Wikipedia:*' -print | wc
find . -name '"List of "*' -print | wc
find . -name '"Lists of "*' -print | wc
echo "Category:"
time find . -name 'Category:*' -exec rm {} \;
time find . -name 'MediaWiki:*' -exec rm {} \;
time find . -name 'Help:*' -exec rm {} \;
echo "File:"
time find . -name 'File:*' -exec rm {} \;
time find . -name 'Image:*' -exec rm {} \;
echo "Template"
time find . -name 'Template:*' -exec rm {} \;
time find . -name 'Wikipedia:*' -exec rm {} \;
time find . -name '"List of "*' -exec rm {} \;
time find . -name '"Lists of "*' -exec rm {} \;