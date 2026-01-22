#! /bin/bash
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
if ! [ -z ${EXPORT_CONF_FILE} ] && [ -r ${EXPORT_CONF_FILE} ]; then
	source ${EXPORT_CONF_FILE}
else
	echo "Cannot find grammatical class export configuration file!"
	exit -1
fi
EXPORT_DIR=$(dirname $LG_DICT_EXPORT)
mkdir -p $EXPORT_DIR
cp -p /usr/local/share/link-grammar/demo-sql/4.0.* $EXPORT_DIR
exec guile -s ${COMMON_DIR}/export-dictionary.scm