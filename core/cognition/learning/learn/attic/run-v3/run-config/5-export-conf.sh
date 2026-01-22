#! /bin/bash
export HOSTNAME=localhost
export PORT=20008
export PROMPT="scheme@(export)"
export COGSERVER_CONF=${CONFIG_DIR}/5-cogserver/cogserver-export-fake.conf
export EXPORT_DB=${ROCKS_DATA_DIR}/gram-2.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${EXPORT_DB}\")"
export LG_DICT_EXPORT=${TEXT_DIR}/learned-2/dict.db
export LG_DICT_LOCALE=en_US