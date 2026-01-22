#!/bin/bash
netcat="nc -N"
lang=$1
filename="$2"
basepath="$3"
splitter=$COMMON_DIR/split-sentences.pl
coghost=$HOSTNAME
cogport=$PORT
observe=$OBSERVE
haveping=`echo foo | $netcat $coghost $cogport`
if [[ $? -ne 0 ]] ; then
	echo "Error: Unable to ping cogserver; not processing file."
	exit 1
fi
alen=${
blen=$(($alen+2))
rest=`echo $filename | cut -c$blen-500`
echo "$MSG file >>>$rest<<<"
base=`echo ${basepath%/*}`
splitdir=${base}/${IN_PROCESS_DIR}
subdir=${base}/${COMPLETED_DIR}
mkdir -p $(dirname "$splitdir/$rest")
mkdir -p $(dirname "$subdir/$rest")
cat "$filename" | $splitter -l $lang >  "$splitdir/$rest"
cwd=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
cat "$splitdir/$rest" | $cwd/submit-lines.pl $coghost $cogport "$observe"
haveping=`echo foo | $netcat $coghost $cogport`
if [[ $? -ne 0 ]] ; then
	echo "Error: Failed to ping cogserver after processing $rest"
	exit 1
fi
mv "$splitdir/$rest" "$subdir/$rest"
rm "$basepath/$rest"