#!/bin/bash
set -u
PRG_PATH="$(readlink -f "$0")"
PRG_DIR="$(dirname "$PRG_PATH")"
. "$PRG_DIR/common.sh"
if [[ $
echo "Usage: $0 MODEL_CSV_FILE PRED_NAME [-o OUTPUT_FILE]"
echo "Example: $0 chr10_moses.5x10.csv \"aging\" -o chr10_moses.5x10.scm"
exit 1
fi
readonly MODEL_CSV_FILE="$1"
readonly BASE_MODEL_CSV_FILE="$(basename "$MODEL_CSV_FILE")"
readonly PRED_NAME="$2"
shift
OUTPUT_FILE="/dev/stdout"
while getopts "o:" opt; do
case $opt in
o) OUTPUT_FILE="$OPTARG"
;;
esac
done
model_name_def() {
local name="$1"
local model="$2"
cat <<EOF
(EquivalenceLink (stv 1.0 1.0)
(PredicateNode "${name}")
$model)
EOF
}
model_accuracy_def() {
local name="$1"
local target="$2"
local accuracy="$3"
cat <<EOF
(EvaluationLink (stv $accuracy 1)
(PredicateNode "accuracy")
(ListLink
(PredicateNode "$name")
(PredicateNode "$target")))
EOF
}
model_balanced_accuracy_def() {
local name="$1"
local target="$2"
local accuracy="$3"
cat <<EOF
(EvaluationLink (stv $accuracy 1)
(PredicateNode "balancedAccuracy")
(ListLink
(PredicateNode "$name")
(PredicateNode "$target")))
EOF
}
model_precision_def() {
local name="$1"
local target="$2"
local precision="$3"
cat <<EOF
(ImplicationLink (stv $precision 1)
(PredicateNode "$name")
(PredicateNode "$target"))
EOF
}
model_recall_def() {
local name="$1"
local target="$2"
local recall="$3"
cat <<EOF
(ImplicationLink (stv $recall 1)
(PredicateNode "$target")
(PredicateNode "$name"))
EOF
}
rows=$(nrows "$MODEL_CSV_FILE")
npads=$(python -c "import math; print(int(math.log($rows, 10) + 1))")
tmp_pipe=$(mktemp -u)
mkfifo "$tmp_pipe"
OLDIFS="$IFS"
IFS=","
i=0
while read combo recall precision; do
model_name="${BASE_MODEL_CSV_FILE}:moses_model_$(pad $i $npads)"
scm_model="$(combo-fmt-converter -c "$combo" -f scheme)"
echo ";;begin_model"
echo "$(model_name_def "$model_name" "$scm_model")"
echo ";;end_model"
echo ";;model_${i} precision"
echo "$(model_precision_def "$model_name" $PRED_NAME $precision)"
echo ";;model_${i} precision"
echo ";;model_${i} recall"
echo "$(model_recall_def "$model_name" $PRED_NAME $recall)"
echo ";;model_${i} recall"
((++i))
done < <(tail -n +2 "$MODEL_CSV_FILE") > "$OUTPUT_FILE"
IFS="$OLDIFS"