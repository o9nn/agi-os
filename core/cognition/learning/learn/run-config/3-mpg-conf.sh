#! /bin/bash
export CORPORA_DIR=$TEXT_DIR/pair-counted
export OBSERVE="observe-block-mpg"
export IN_PROCESS_DIR=mpg-split
export COMPLETED_DIR=mpg-done
export MSG="MPG-Processing"
export HOSTNAME=localhost
export PORT=17003
export WEBPORT=18083
export PROMPT="scheme@(mpg-parse) "
export OCPROMPT="[0;32mcogserv@(mpg-parse) [0m"
export LOGFILE=/tmp/cogserver-mpg-en.log
export MST_DB=rocks://${ROCKS_DATA_DIR}/mpg-parse.rdb
export STORAGE_NODE="(RocksStorageNode \"${MST_DB}\")"