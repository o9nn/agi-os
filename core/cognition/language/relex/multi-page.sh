#!/bin/bash
filelist=../wiki/simplewiki-20080629-stripped/*
maxjobs=4;
export LANG=en_US.UTF-8
VM_OPTS="-Xmx1024m"
RELEX_OPTS="\
	-Drelex.algpath=data/relex-semantic.algs \
	-Dwordnet.configfile=data/wordnet/file_properties.xml \
	-Djava.library.path=../../lib \
	-Dgate.home=../../share/java \
	-Dgate.plugins.home=../../share/java \
	-Dgate.site.config=../../share/java \
	"
CLASSPATH='-classpath ./target/classes:./target/lib/*'
function parseit {
	fn="`basename "$1"`";
	in="$1"
	url="http://simple.wikipedia.org/wiki/$fn"
	out="../wiki/parsed/$fn.xml"
	err="../wiki/err/err-$fn"
	echo $url
	cat "$in" | nice java $VM_OPTS $RELEX_OPTS $CLASSPATH relex.WebFormat -g -n 4 \
 	       --url "$url" > "$out" 2>"$err" &
}
jobsrunning=0;
echo $jobsrunning
for filename in $filelist;
do
	if [ $jobsrunning -lt $maxjobs ] ;
	then
		let jobsrunning=jobsrunning+1
		parseit "$filename"
	else
		wait
		let jobsrunning=0
	fi
done