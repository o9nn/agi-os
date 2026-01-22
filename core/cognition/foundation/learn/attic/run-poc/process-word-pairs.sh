#!/bin/bash
if [ $
then
echo "Usage: ./process-word-pairs.sh <mode> <language>"
exit 0
fi
cnt_mode="clique-dist"
source ./config/params.txt
source ./config/det-port-num.sh $1 $2
case $1 in
cmi)
module="compute-mi.scm"
func="comp-mi"
;;
mst)
module="fetch-word-pairs.scm"
func="fetch-wp"
;;
*)
echo "Usage: ./process-word-pairs.sh <mode> <language>"
echo "<mode> must be either cmi or mst"
exit 0
esac
haveping=`echo foo | nc -N localhost $PORT`
if [[ $? -ne 0 ]] ; then
exit 1
fi
echo -e "(load \"$module\")" | nc -N localhost $PORT
echo -e "($func \"$cnt_mode\")" | nc -N localhost $PORT