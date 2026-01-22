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
if ! [ -z ${GEN_CONF_FILE} ] && [ -r ${GEN_CONF_FILE} ]; then
   . ${GEN_CONF_FILE}
else
   echo "Cannot find dictionary configuration file!"
   exit -1
fi
if [ -d ${DICT_DIR} ]; then
	echo "Dictionary already exists; not generating a new one!"
	echo "Remove ${DICT_DIR} if you want to generate a new dict."
	exit -1
fi
${COMMON_DIR}/gen-dict.scm ${CONFIG_DIR}/${DICT_CONF} $DICT_DIR