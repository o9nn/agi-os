#! /bin/bash
export HOSTNAME=localhost
export PORT=17004
export WEBPORT=18084
export PROMPT="scheme@(gram-class) "
export OCPROMPT="[0;32mcogserv@(gram-class) [0m"
export LOGFILE=/tmp/cogserver-gram-en.log
export GRAM_DB=${ROCKS_DATA_DIR}/gram-2.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${GRAM_DB}\")"
API="(define psa star-obj)"