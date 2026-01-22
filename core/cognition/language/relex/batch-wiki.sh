#!/bin/bash
export LANG=en_US.UTF-8
VM_OPTS="-Xmx1024m"
RELEX_OPTS="\
	-Djava.library.path=/usr/local/lib:/usr/local/lib/jni \
	-DEnglishModelFilename=data/opennlp/models-1.5/en-sent.bin \
	"
CLASSPATH='-classpath ./target/classes:./target/lib/*'
lettre=S
filepat=Sa*
FILES=enwiki-20080524-alpha/$lettre/$filepat
for fpath in $FILES
do
	f=${fpath
	echo "Processing \"${f}\""
	url="http://en.wikipedia.org/wiki/${f}"
	echo "url $url"
	cat "${fpath}" | \
	nice java $VM_OPTS $RELEX_OPTS $CLASSPATH relex.WebFormat  -g -n 20 \
	--url "${url}" > "parsed/$lettre/${f}.xml" 2> "err/$lettre/${f}"
	mv "enwiki-20080524-alpha/$lettre/${f}" done/$lettre
done