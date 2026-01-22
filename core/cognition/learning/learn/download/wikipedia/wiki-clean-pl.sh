#! /bin/bash
echo "Kategoria:"
find . -name 'Kategoria:*' -print | wc
find . -name 'MediaWiki:*' -print | wc
find . -name 'Pomoc:*' -print | wc
echo "Plik:"
find . -name 'Plik:*' -print | wc
find . -name 'Obraz:*' -print | wc
echo "Szablon"
find . -name 'Szablon:*' -print | wc
find . -name 'Wikipedia:*' -print | wc
find . -name 'Wikiprojekt:*' -print | wc
find . -name 'Portal:*' -print | wc
echo "Kategoria:"
time find . -name 'Kategoria:*' -exec rm {} \;
time find . -name 'MediaWiki:*' -exec rm {} \;
time find . -name 'Pomoc:*' -exec rm {} \;
echo "Plik:"
time find . -name 'Plik:*' -exec rm {} \;
time find . -name 'Obraz:*' -exec rm {} \;
echo "Szablon"
time find . -name 'Szablon:*' -exec rm {} \;
time find . -name 'Wikipedia:*' -exec rm {} \;
time find . -name 'Wikiprojekt:*' -exec rm {} \;
time find . -name 'Portal:*' -exec rm {} \;