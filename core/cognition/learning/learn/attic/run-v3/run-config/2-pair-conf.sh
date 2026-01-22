#! /bin/bash
export CORPORA_DIR=$TEXT_DIR/beta-pages
export SENTENCE_SPLIT=false
export SPLIT_LANG=en
export HOSTNAME=localhost
export PORT=17001
export PROMPT="scheme@(count-pairs)"
export COGSERVER_CONF=""
export OBSERVE="observe-text"
export PAIRS_DB=${ROCKS_DATA_DIR}/word_pairs.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${PAIRS_DB}\")"
export PAIRS_DB=word_pairs
export STORAGE_NODE="(PostgresStorageNode \"postgres:///${PAIRS_DB}\")"
export IN_PROCESS_DIR=pair-split
export COMPLETED_DIR=pair-counted
export MSG="Splitting and word-pair counting"