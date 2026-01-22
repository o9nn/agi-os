#! /bin/bash
export COMMON_DIR=/home/ubuntu/src/learn/run-common
export TEXT_DIR=/home/ubuntu/text/expt-42
export ROCKS_DATA_DIR=/home/ubuntu/data/expt-42
export CONFIG_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
export MASTER_CONFIG_FILE=${CONFIG_DIR}/$( basename "${BASH_SOURCE[0]}" )
export GEN_CONF_FILE=$CONFIG_DIR/1-corpus-conf.sh
export PAIR_CONF_FILE=$CONFIG_DIR/2-pair-conf-en.sh
export PAIR_CONF_FILE=$CONFIG_DIR/2-pair-conf-fake.sh
export MST_CONF_FILE=$CONFIG_DIR/3-mst-conf.sh
export MST_CONF_FILE=$CONFIG_DIR/3-mpg-conf-fake.sh
export GRAM_CONF_FILE=$CONFIG_DIR/4-gram-conf.sh
export EXPORT_CONF_FILE=$CONFIG_DIR/5-export-conf.sh
export SENTENCE_SPLIT=false
export XFORM_SPLIT=false