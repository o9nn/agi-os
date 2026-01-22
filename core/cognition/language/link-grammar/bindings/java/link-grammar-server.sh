#!/bin/bash
export LANG=en_US.UTF-8
VM_OPTS="-Xmx1024m -Djava.library.path=/usr/lib:/usr/lib/jni:/usr/local/lib:/usr/local/lib/jni"
CLASSPATH="-classpath bin:../../build/bindings/java/bin:../../bindings/java/bin:/usr/share/java/linkgrammar.jar:/usr/local/share/java/linkgrammar.jar"
java $VM_OPTS $CLASSPATH org.linkgrammar.LGService 9000