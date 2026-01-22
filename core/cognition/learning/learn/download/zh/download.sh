#!/bin/bash
mkdir tmp
mkdir whole-books
for f in `seq 1 12`; do
	let NUM=5000+$f
	wget http://www.ixdzs.com/down/$NUM"_1"
	7z x -otmp $NUM"_1"
	rm $NUM"_1"
	find tmp/*.txt | while read f; \
		do \
			cat $f | iconv -f  GB18030 -t UTF-8 > foo; \
			mv foo `echo $f.utf8 | iconv -f  GB18030 -t UTF-8`; \
			rm $f; \
		done
	mv tmp/*.txt.utf8 whole-books
done
rmdir tmp