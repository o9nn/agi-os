#!/bin/bash
CONF_FILE=$1
if [ -r $CONF_FILE ]; then
. $CONF_FILE
else
echo "Cannot find configuration file!"
exit -1
fi
export HOSTNAME
export PORT
export OBSERVE
export IN_PROCESS_DIR
export COMPLETED_DIR
export MSG
cwd=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
if $SENTENCE_SPLIT; then
time find $CORPORA_DIR -type f \
-exec $cwd/file-split-process.sh $SPLIT_LANG {} $CORPORA_DIR \;
elif $XFORM_SPLIT; then
time find $CORPORA_DIR -type f \
-exec $cwd/file-xform-process.sh {} $CORPORA_DIR $XFORM_CMD \;
else
time find $CORPORA_DIR -type f \
-exec $cwd/file-nosplit-process.sh {} $CORPORA_DIR \;
fi