#!/bin/bash
STD=0
SUPER=0
IGN_BSC=0
IGN_MOSHE=0
for i in `seq 1 100`;
do
    echo "Iteration $i"
    EVALS_STR=`moses-exec -H pa -k3 -a hc -r"$i" -V1 -m 500000 -P1 | grep "
    EVALS=${EVALS_STR
    STD=$(($EVALS+$STD))
    echo "Std Reduct: Evals = $EVALS, Total Evals = $STD"
    EVALS_STR=`moses-exec -H pa -k3 -a hc -r"$i" -V1 -m 500000 -P1 -E3 | grep "
    EVALS=${EVALS_STR
    SUPER=$(($EVALS+$SUPER))
    echo "Super Reduct: Evals = $EVALS, Total Evals = $SUPER"
    EVALS_STR=`moses-exec -H pa -k3 -a hc -r"$i" -V1 -m 500000 -P1 -I1 | grep "
    EVALS=${EVALS_STR
    IGN_BSC=$(($EVALS+$IGN_BSC))
    echo "Ignore Bscore: Evals = $EVALS, Total Evals = $IGN_BSC"
    EVALS_STR=`moses-exec -H pa -k3 -a hc -r"$i" -V1 -m 500000 -P1 -S0 | grep "
    EVALS=${EVALS_STR
    IGN_MOSHE=$(($EVALS+$IGN_MOSHE))
    echo "Ignore Moshe: Evals = $EVALS, Total Evals = $IGN_MOSHE"
done
STD_MEAN=$(($STD/100))
SUPER_MEAN=$(($SUPER/100))
IGN_BSC_MEAN=$(($IGN_BSC/100))
IGN_MOSHE_MEAN=$(($IGN_MOSHE/100))
echo "Std Reduct Mean Evals = $STD_MEAN"
echo "Super Reduct Mean Evals = $SUPER_MEAN"
echo "Ignore Bscore Mean Evals = $IGN_BSC_MEAN"
echo "Ignore Moshe Tweak Mean Evals = $IGN_MOSHE_MEAN"