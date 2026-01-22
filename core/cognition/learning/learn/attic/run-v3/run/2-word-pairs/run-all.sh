#! /bin/bash
if [ -z $MASTER_CONFIG_FILE ]; then
echo "MASTER_CONFIG_FILE not defined!"
exit -1
fi
if [ -r $MASTER_CONFIG_FILE ]; then
. $MASTER_CONFIG_FILE
else
echo "Cannot find master configuration file!"
exit -1
fi
if ! [ -z ${PAIR_CONF_FILE} ] && [ -r ${PAIR_CONF_FILE} ]; then
. ${PAIR_CONF_FILE}
else
echo "Cannot find pair-counting configuration file!"
exit -1
fi
guile -l ${COMMON_DIR}/cogserver.scm -c "(sleep 150000000)" &
sleep 3
echo -e "(block-until-idle 0.01)\n.\n." | nc $HOSTNAME $PORT >> /dev/null
${COMMON_DIR}/process-corpus.sh $PAIR_CONF_FILE
echo Done pair counting
echo "(exit-server)" | nc $HOSTNAME $PORT >> /dev/null
sleep 1
echo "Start computing the pair marginals"
guile -s ${COMMON_DIR}/marginals-pair.scm
echo "Finish computing the pair marginals"
echo -e "\n\n\n"
echo Done processing word-pairs