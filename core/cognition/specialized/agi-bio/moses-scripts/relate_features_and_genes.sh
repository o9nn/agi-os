#!/bin/bash
set -u
PRG_PATH="$(readlink -f "$0")"
PRG_DIR="$(dirname "$PRG_PATH")"
. "$PRG_DIR/common.sh"
if [[ $
echo "Usage: $0 FEATURE_CSV_FILE [-o OUTPUT_FILE]"
echo "Example: $0 oldvscontrolFeatures.csv -o oldvscontrol-features-and-genes.scm"
exit 1
fi
readonly FEATURE_CSV_FILE="$1"
shift
OUTPUT_FILE="/dev/stdout"
while getopts "o:" opt; do
case $opt in
o) OUTPUT_FILE="$OPTARG"
;;
esac
done
header=$(head -n 1 "$FEATURE_CSV_FILE")
expected_header='"gene","Freq","level"'
if [[ "$header" != "$expected_header" ]]; then
fatalError "Wrong header format: expect '$expected_header' but got '$header'"
fi
OLDIFS="$IFS"
IFS=","
while read feature freq level; do
cat <<EOF
(EquivalenceLink
(PredicateNode $feature)
(LambdaLink
(VariableNode "\$X")
(EvaluationLink
(PredicateNode "overexpressed")
(ListLink
(GeneNode $feature)
(VariableNode "\$X"))))
EOF
done < <(tail -n +2 "$FEATURE_CSV_FILE") > "$OUTPUT_FILE"
IFS="$OLDIFS"