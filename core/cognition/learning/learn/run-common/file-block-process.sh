#!/bin/bash
netcat="nc -N"
filename="$1"
basepath="$2"
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
cp "$filename" "$splitdir/$rest"
cwd=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
cat "$splitdir/$rest" | $cwd/submit-block.pl $coghost $cogport "$observe"
haveping=`echo foo | $netcat $coghost $cogport`
if [[ $? -ne 0 ]] ; then
	echo "Error: Failed to ping cogserver after processing $rest"
	exit 1
fi
mv "$splitdir/$rest" "$subdir/$rest"
rm "$basepath/$rest"