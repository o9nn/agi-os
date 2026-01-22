#! /bin/bash
export HOSTNAME=localhost
export PORT=19008
export PROMPT="scheme@(gram-class)"
export COGSERVER_CONF=${CONFIG_DIR}/4-cogserver/cogserver-gram-fake.conf
export GRAM_DB=${ROCKS_DATA_DIR}/gram-2.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${GRAM_DB}\")"
API="(define psa star-obj)"
export GRAM_CLUSTER="${API} (gram-classify-greedy-mifuzz psa 3.0 0.0 4)"