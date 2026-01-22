REPEAT=10
PRG_PATH="$(readlink -f "$0")"
PRG_DIR="$(dirname "$PRG_PATH")"
echo "Run asmoses $REPEAT times on multiple problems of various complexity"
for pbl in pa dj mux maj sr; do
for k in {3..5}; do
$PRG_DIR/asmoses-bm.sh $REPEAT -H$pbl -k$k -m10000
done
done