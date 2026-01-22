#! /bin/bash
echo "Catégorie:"
find . -name 'Catégorie:*' -print | wc
find . -name 'MediaWiki:*' -print | wc
find . -name 'Aide:*' -print | wc
echo "Fichier:"
find . -name 'Fichier:*' -print | wc
find . -name 'Module:*' -print | wc
find . -name 'Modèle:*' -print | wc
echo "Wikipédia:"
find . -name 'Wikipédia:*' -print | wc
find . -name 'Référence:*' -print | wc
find . -name 'Projet:*' -print | wc
find . -name 'Portail:*' -print | wc
find . -name '"Liste de"*' -print | wc
echo "Catégorie:"
time find . -name 'Catégorie:*' -exec rm {} \;
time find . -name 'MediaWiki:*' -exec rm {} \;
time find . -name 'Aide:*' -exec rm {} \;
echo "Fichier:"
time find . -name 'Fichier:*' -exec rm {} \;
time find . -name 'Module:*' -exec rm {} \;
time find . -name 'Modèle:*' -exec rm {} \;
echo "Wikipédia:"
time find . -name 'Wikipédia:*' -exec rm {} \;
time find . -name 'Référence:*' -exec rm {} \;
time find . -name 'Projet:*' -exec rm {} \;
time find . -name 'Portail:*' -exec rm {} \;
time find . -name '"Liste de"*' -exec rm {} \;