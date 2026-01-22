#! /bin/bash
export CORPORA_DIR=$TEXT_DIR/pair-counted
export SENTENCE_SPLIT=false
export SPLIT_LANG=en
export HOSTNAME=localhost
export PORT=17001
export PROMPT="scheme@(mst-parse)"
export COGSERVER_CONF=""
export OBSERVE="observe-mst"
export MST_DB=${ROCKS_DATA_DIR}/mst_parse.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${MST_DB}\")"
export IN_PROCESS_DIR=mst-split
export COMPLETED_DIR=mst-done
export MSG="MST-Processing"