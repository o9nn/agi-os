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
export WEBPORT
export OBSERVE
export IN_PROCESS_DIR
export COMPLETED_DIR
export MSG
cwd=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
if $BLOCK_SUBMIT; then
	time find $CORPORA_DIR -type f \
		-exec $cwd/file-block-process.sh {} $CORPORA_DIR \;
elif $SENTENCE_SPLIT; then
	time find $CORPORA_DIR -type f \
		-exec $cwd/file-split-process.sh $SPLIT_LANG {} $CORPORA_DIR \;
elif $XFORM_SPLIT; then
	time find $CORPORA_DIR -type f \
		-exec $cwd/file-xform-process.sh {} $CORPORA_DIR $XFORM_CMD \;
elif $LINE_SPLIT; then
	time find $CORPORA_DIR -type f \
		-exec $cwd/file-line-process.sh {} $CORPORA_DIR \;
else
	time find $CORPORA_DIR -type f \
		-exec $cwd/file-block-process.sh {} $CORPORA_DIR \;
fi