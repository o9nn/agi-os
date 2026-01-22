#! /bin/bash
for i in `seq 0 499`;
do
let ART=53000+$i;
echo ./process-gutenberg.sh http://www.gutenberg.org/files/$ART/$ART-0.txt
./process-gutenberg.sh http://www.gutenberg.org/files/$ART/$ART-0.txt
done