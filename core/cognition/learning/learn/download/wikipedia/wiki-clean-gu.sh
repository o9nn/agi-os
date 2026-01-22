#! /bin/bash
echo "Category:"
find . -name 'શ્રેણી:*' -print | wc
find . -name 'મીડિયાવિકિ:*' -print | wc
find . -name 'મદદ:*' -print | wc
echo "File:"
find . -name 'Module:*' -print | wc
find . -name 'ચિતર:*' -print | wc
find . -name 'છબી:*' -print | wc
echo "Template"
find . -name 'ઢાંચો:*' -print | wc
find . -name 'વિકિપીડિયા:*' -print | wc
echo "Category:"
time find . -name 'શ્રેણી:*' -exec rm {} \;
time find . -name 'મીડિયાવિકિ:*' -exec rm {} \;
time find . -name 'મદદ:*' -exec rm {} \;
echo "File:"
time find . -name 'ચિતર:*' -exec rm {} \;
time find . -name 'છબી:*' -exec rm {} \;
time find . -name 'Module:*' -exec rm {} \;
echo "Template"
time find . -name 'ઢાંચો:*' -exec rm {} \;
time find . -name 'વિકિપીડિયા:*' -exec rm {} \;