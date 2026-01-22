#!/bin/bash
export LANG=en_US.UTF-8
VM_OPTS="-Xmx1024m \
-Djava.library.path=/usr/lib:/usr/lib/jni:/usr/local/lib:/usr/local/lib/jni"
CLASSPATH='-classpath ./target/classes:./target/lib/*'
java $VM_OPTS $RELEX_OPTS $CLASSPATH relex.RelationExtractor -n 4 -l -t -a --stanford --penn -s "Alice ate the mushroom."