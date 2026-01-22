#!/bin/bash
export LANG=en_US.UTF-8
VM_OPTS="-Xmx1024m"
RELEX_OPTS="\
-Djava.library.path=/usr/lib:/usr/lib/jni:/usr/local/lib:/usr/local/lib/jni \
-Drelex.algpath=data/relex-semantic.algs \
-Dwordnet.configfile=data/wordnet/file_properties.xml \
"
CLASSPATH='-classpath ./target/classes:./target/lib/*'
cat test-corpus.txt | \
java $VM_OPTS $RELEX_OPTS $CLASSPATH relex.WebFormat  -n 4 -g
exit 1;
cat ../../data/voa_sentences-clean.txt | \
java $VM_OPTS $RELEX_OPTS $CLASSPATH relex.WebFormat  -n 4 \
--url "voa_sentences-clean.txt" > ../../data/voa_sentences-parsed.xml