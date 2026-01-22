#! /bin/bash
export CORPORA_DIR=$TEXT_DIR/input-pages
export IN_PROCESS_DIR=pair-split
export COMPLETED_DIR=pair-counted
export OBSERVE="observe-block-pairs"
export MSG="Block word-pair counting"
export HOSTNAME=localhost
export PORT=17002
export WEBPORT=18082
export PROMPT="scheme@(count-pairs) "
export OCPROMPT="[0;32mcogserv@(count-pairs) [0m"
export LOGFILE=/tmp/cogserver-pairs-en.log
export PAIRS_DB=${ROCKS_DATA_DIR}/word-pairs.rdb
export STORAGE_NODE="(MonoStorageNode \"monospace://${PAIRS_DB}\")"