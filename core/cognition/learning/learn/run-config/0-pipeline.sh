#! /bin/bash
export COMMON_DIR=/home/opencog/experiments/run-common
export TEXT_DIR=/home/opencog/text/
export ROCKS_DATA_DIR=/home/opencog/data/
export CONFIG_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
export MASTER_CONFIG_FILE=${CONFIG_DIR}/$( basename "${BASH_SOURCE[0]}" )
export PAIR_CONF_FILE=$CONFIG_DIR/2-pair-conf.sh
export MST_CONF_FILE=$CONFIG_DIR/3-mpg-conf.sh
export GRAM_CONF_FILE=$CONFIG_DIR/4-gram-conf.sh
export OCPROMPT="[0;32mcogserver> [0m"
export LOGFILE=/tmp/cogserver.log