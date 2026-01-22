#! /bin/bash
export CORPORA_DIR=$TEXT_DIR/pair-counted
export SENTENCE_SPLIT=true
export SPLIT_LANG=en
export HOSTNAME=localhost
export PORT=18005
export PROMPT="scheme@(mpg-parse)"
export COGSERVER_CONF=${CONFIG_DIR}/3-cogserver/cogserver-mst-en.conf
export OBSERVE="observe-mpg"
export MST_DB=${ROCKS_DATA_DIR}/mpg_parse.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${MST_DB}\")"
export IN_PROCESS_DIR=mpg-split
export COMPLETED_DIR=mpg-done
export MSG="MPG-Processing"