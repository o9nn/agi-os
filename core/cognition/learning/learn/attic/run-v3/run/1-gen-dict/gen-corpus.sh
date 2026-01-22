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
if [ -r $GEN_CONF_FILE ]; then
	. $GEN_CONF_FILE
else
	echo "Cannot find corpus configuration file!"
	exit -1
fi
DICT=$DICT_DIR
CORP=$GEN_CORPUS_DIR
if [[ -d $CORP ]]; then
	echo Corpus directory exists: $CORP
	echo Delete or move this directory and try again
	exit -1
fi
echo "Using dictionary found in $DICT"
echo "Placing generated corpus in $CORP"
mkdir $CORP
for (( n=$SENT_SHORTEST; n<=$SENT_LONGEST; n++)); do
	echo "Generating sentences of length $n"
	link-generator -l $DICT -s $n -c $NUM_SENTENCES > $GEN_CORPUS_DIR/corpus-$n.txt
done
exit 0