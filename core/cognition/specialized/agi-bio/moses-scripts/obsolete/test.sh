#!/bin/bash
set -u
if [[ $
echo "Usage: $0 SETTINGS_FILE"
exit 1
fi
PRG_PATH="$(readlink -f "$0")"
PRG_DIR="$(dirname "$PRG_PATH")"
ROOT_DIR="$(dirname "$PRG_DIR")"
SET_PATH="$1"
SET_BASENAME="$(basename "$SET_PATH")"
fatalError() {
echo "[ERROR] $@" 1>&2
exit 1
}
warnEcho() {
echo "[WARN] $@"
}
infoEcho() {
echo "[INFO] $@"
}
hr2i() {
local val=$1
local val=${val/M/000K}
local val=${val/K/000}
echo $val
}
pad() {
local pad_expression="%0${2}d"
printf "$pad_expression" "$1"
}
train_test_split() {
local DATAFILE="$1"
local RATIO="$2"
RANDOM="$3"
local DATAFILE_TRAIN=${DATAFILE//.csv/_train.csv}
local DATAFILE_TEST=${DATAFILE//.csv/_test.csv}
head -n 1 "$DATAFILE" > "${DATAFILE_TRAIN}"
head -n 1 "$DATAFILE" > "${DATAFILE_TEST}"
while read line; do
if [[ $(bc <<< "$RATIO * 32767 >= $RANDOM") == 1 ]]; then
echo "$line" >> "${DATAFILE_TRAIN}"
else
echo "$line" >> "${DATAFILE_TEST}"
fi
done < <(tail -n +2 "$DATAFILE")
}
model_def() {
name="$1"
model="$2"
echo "(EquivalenceLink (stv 1.0 1.0) (PredicateNode \"${name}\") $model)"
}
infoEcho "Copy $SET_PATH to current directory"
cp "$SET_PATH" .
. "$SET_BASENAME"
infoEcho "Launch cogserver"
cd "$opencog_repo_path/scripts/"
./run_cogserver.sh "$build_dir_name" &
cd -
sleep 5
infoEcho "Load background knowledge"
if [[ "$scheme_file_path" =~ ^[^/] ]]; then
scheme_file_path="$ROOT_DIR/$scheme_file_path"
fi
(echo "scm"; cat "$scheme_file_path") \
| "$opencog_repo_path/scripts/run_telnet_cogserver.sh"
infoEcho "Create train and test data"
if [[ "$data_path" =~ ^[^/] ]]; then
data_path="$ROOT_DIR/$data_path"
fi
cp $data_path .
data_basename="$(basename "$data_path")"
train_test_split "$data_basename" "$train_ratio" "$init_seed"
data_basename_train=${data_basename//.csv/_train.csv}
data_basename_test=${data_basename//.csv/_test.csv}
infoEcho "Run MOSES"
moses_output_file=results.moses
. "$PRG_DIR/moses.sh"
infoEcho "Load MOSES models into the AtomSpace"
(echo "scm";
i=0
while read line; do
moses_model_name="moses_$(pad $i 3)"
echo "$(model_def "$moses_model_name" "$line")"
((++i))
done < "$moses_output_file"
) | "$opencog_repo_path/scripts/run_telnet_cogserver.sh"