#!/bin/bash
export LANG=en_US.UTF-8
VM_OPTS="-Xmx2048m"
RELEX_OPTS="\
	-Djava.library.path=/usr/lib:/usr/lib/jni:/usr/local/lib:/usr/local/lib/jni \
	-Drelex.algpath=data/relex-semantic.algs \
	-Dwordnet.configfile=data/wordnet/file_properties.xml \
	"
CLASSPATH='-classpath ./target/classes:./target/lib/*'
java $VM_OPTS $RELEX_OPTS $CLASSPATH relex.Server --link --relex