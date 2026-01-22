#!/bin/bash
if [ -z $MASTER_CONFIG_FILE ]; then
echo "MASTER_CONFIG_FILE not defined!"
exit -1
fi
if [ -r $MASTER_CONFIG_FILE ]; then
source $MASTER_CONFIG_FILE
else
echo "Cannot find master configuration file!"
exit -1
fi
if ! [ -z ${PAIR_CONF_FILE} ] && [ -r ${PAIR_CONF_FILE} ]; then
source ${PAIR_CONF_FILE}
else
echo "Cannot find pair-counting configuration file!"
exit -1
fi
notify_done () {
echo -e "(finish-pair-submit)\n.\n." | nc $HOSTNAME $PORT >> /dev/null
}
if [ ! -d $CORPORA_DIR ]; then
echo "Cannot find a text corpus at $CORPORA_DIR"
notify_done
exit -1
fi
if [ 0 -eq `find $CORPORA_DIR -type f |wc -l` ]; then
echo "Empty text corpus directory at $CORPORA_DIR"
notify_done
exit -1
fi
echo -e "(start-pair-submit)\n.\n." | nc $HOSTNAME $PORT >> /dev/null
${COMMON_DIR}/process-corpus.sh $PAIR_CONF_FILE
notify_done