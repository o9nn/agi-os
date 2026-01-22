#! /bin/bash
echo "Kategorya:"
find . -name 'Kategorya:*' -print | wc
find . -name 'MediaWiki:*' -print | wc
find . -name 'Tulong:*' -print | wc
echo "Talaksan:"
find . -name 'Talaksan:*' -print | wc
find . -name 'Padron:*' -print | wc
echo "Template"
find . -name 'Template:*' -print | wc
find . -name 'Wikipedia:*' -print | wc
find . -name '"Listahan ng mga "*' -print | wc
echo "Translations"
find . -name '(Ingles: *)' -print | wc
echo "Kategorya:"
time find . -name 'Category:*' -exec rm {} \;
time find . -name 'MediaWiki:*' -exec rm {} \;
time find . -name 'Tulong:*' -exec rm {} \;
echo "Talaksan:"
time find . -name 'Talaksan:*' -exec rm {} \;
time find . -name 'Padron:*' -exec rm {} \;
echo "Template"
time find . -name 'Template:*' -exec rm {} \;
time find . -name 'Wikipedia:*' -exec rm {} \;
time find . -name '"Listahan ng mga "*' -exec rm {} \;
echo "Translations"
time find . -name '(Ingles: *)' -exec rm {} \;