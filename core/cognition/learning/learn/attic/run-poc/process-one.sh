#!/bin/bash
lang=$2
filename="$3"
coghost="$4"
cogport=$5
splitter=./split-sentences.pl
splitdir=split-articles
parsesdir=mst-parses
cnt_mode="clique-dist"
cnt_reach=6
mst_dist=(1)
exp_parses="EXPORT"
split_sents="
source ./config/params.txt
base=`echo $filename | cut -d \/ -f 1`
rest=`echo $filename | cut -d \/ -f 2-6`
case $1 in
pairs)
subdir=submitted-articles
observe="observe-text-mode"
params="$cnt_mode $cnt_reach"
;;
mst)
subdir=mst-articles
observe="observe-mst-mode"
if [[ "$exp_parses" != "NONE" ]]; then
mkdir -p $(dirname "$parsesdir/$rest");
params="$cnt_mode $mst_dist ${rest}.ull";
else
params="$cnt_mode $mst_dist $exp_parses";
fi
;;
esac
haveping=`echo foo | nc -N $coghost $cogport`
if [[ $? -ne 0 ]] ; then
exit 1
fi
echo "Processing file >>>$rest<<<"
mkdir -p $(dirname "$splitdir/$rest")
mkdir -p $(dirname "$subdir/$rest")
if [[ "$split_sents" == "
cat "$filename" | $splitter -l $lang >  "$splitdir/$rest"
else
cat "$filename" | sed -e 's/\\/\\\\/g' -e 's/\"/\\\"/g' > "$splitdir/$rest"
fi
cat "$splitdir/$rest" | ./submit-one.pl $coghost $cogport $observe $params
haveping=`echo foo | nc -N $coghost $cogport`
if [[ $? -ne 0 ]] ; then
exit 1
fi
if [ -f "${rest}.ull" ]; then
mv ${rest}.ull "$parsesdir/${rest}.ull"
fi
mv "$splitdir/$rest" "$subdir/$rest"
rm "$base/$rest"