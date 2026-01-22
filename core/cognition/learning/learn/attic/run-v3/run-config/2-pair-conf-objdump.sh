#! /bin/bash
export CORPORA_DIR=$TEXT_DIR/binaries
export SENTENCE_SPLIT=false
export XFORM_SPLIT=true
export XFORM_CMD=${COMMON_DIR}/split-objdump.pl
export HOSTNAME=localhost
export PORT=17009
export PROMPT="scheme@(objdump)"
export COGSERVER_CONF=${CONFIG_DIR}/2-cogserver/cogserver-pairs-objdump.conf
export OBSERVE="observe-window 30"
export PAIRS_DB=${ROCKS_DATA_DIR}/objdump_pairs.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${PAIRS_DB}\")"
export MSG="Objdump and pair counting"
export IN_PROCESS_DIR=pair-split
export COMPLETED_DIR=pair-counted