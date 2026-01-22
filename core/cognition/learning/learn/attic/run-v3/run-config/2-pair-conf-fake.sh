#! /bin/bash
export CORPORA_DIR=$TEXT_DIR/fake-corpus
export SENTENCE_SPLIT=false
export HOSTNAME=localhost
export PORT=17008
export PROMPT="scheme@(fake-pairs)"
export COGSERVER_CONF=${CONFIG_DIR}/2-cogserver/cogserver-pairs-fake.conf
export OBSERVE="observe-text"
export PAIRS_DB=${ROCKS_DATA_DIR}/fake_pairs.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${PAIRS_DB}\")"
export MSG="Splitting and word-pair counting"
export IN_PROCESS_DIR=pair-split
export COMPLETED_DIR=pair-counted