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
if ! [ -z ${MST_CONF_FILE} ] && [ -r ${MST_CONF_FILE} ]; then
	source ${MST_CONF_FILE}
else
	echo "Cannot find MST configuration file!"
	exit -1
fi
if ! [ -z ${GRAM_CONF_FILE} ] && [ -r ${GRAM_CONF_FILE} ]; then
	source ${GRAM_CONF_FILE}
else
	echo "Cannot find grammatical class clustering configuration file!"
	exit -1
fi
if [[ $STORAGE_NODE = "(RocksStorageNode"* ]]; then
	cp -pr ${MST_DB} ${GRAM_DB}
elif [[ $STORAGE_NODE = "(MonoStorageNode"* ]]; then
	cp -pr ${MST_DB} ${GRAM_DB}
elif [[ $STORAGE_NODE = "(PostgresStorageNode"* ]]; then
	createdb -T ${MST_DB} ${GRAM_DB}
else
	echo "Unknown storage medium!"
	exit -1
fi
guile -l ${COMMON_DIR}/cogserver-gram.scm -c "(sleep 150000000)" &
sleep 3
echo -e "(block-until-idle 0.01)\n.\n." | nc $HOSTNAME $PORT >> /dev/null
echo -e "$GRAM_CLUSTER\n.\n." | nc $HOSTNAME $PORT
sleep 1
echo -e "((make-store (make-pseudo-cset-api)) 'store-all)\n.\n." | nc $HOSTNAME $PORT
echo -e "((make-store (make-gram-class-api)) 'store-all)\n.\n." | nc $HOSTNAME $PORT
sleep 1
echo Done clustering
echo "(exit-server)" | nc $HOSTNAME $PORT >> /dev/null
sleep 1
echo Done