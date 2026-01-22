#! /bin/bash
export CORPORA_DIR=$TEXT_DIR/beta-pages
export IN_PROCESS_DIR=pair-split
export COMPLETED_DIR=pair-counted
export MSG="Splitting and word-pair counting"
export SENTENCE_SPLIT=true
export SPLIT_LANG=en
export OBSERVE="observe-text"
export HOSTNAME=localhost
export PORT=17005
export PROMPT="scheme@(en-pairs)"
export OCPROMPT="[0;32mcogserv@(en-pairs) [0m"
export LOGFILE=/tmp/cogserver-pairs-en.log
export PAIRS_DB=${ROCKS_DATA_DIR}/en_pairs.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${PAIRS_DB}\")"