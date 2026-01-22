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
if ! [ -z ${GRAM_CONF_FILE} ] && [ -r ${GRAM_CONF_FILE} ]; then
. ${GRAM_CONF_FILE}
else
echo "Cannot find grammatical class clustering configuration file!"
exit -1
fi
exec guile -l ${COMMON_DIR}/cogserver-gram.scm