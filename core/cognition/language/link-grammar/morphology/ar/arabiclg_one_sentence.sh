#!/bin/sh
export ARAMORPH_HOME=../whereever/aramporh-1.2.1/
time echo -e "$*"			 		|
${ARAMORPH_HOME}/aramorph_fast.pl -i roman 2>/dev/null	|
./buck2lg.pl	|
(echo -e '!width=118\n!max-length=300\n'; cat; echo -e "\n\n\n\n\n")|
link-parser ar 2>/dev/null		|
egrep -v 'Opening|width set to|\+Time|RETURN'			|
egrep -C 70 --color "UNUSED=[0-9]+|Found [0-9]+ linkages"