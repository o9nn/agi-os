set -u
N=20
BUILD_DIR_NAME=build
PRG_PATH="$(readlink -f "$0")"
PRG_DIR="$(dirname "$PRG_PATH")"
UTEST_DIR="$PRG_DIR/../../$BUILD_DIR_NAME/tests/query"
get_real_time() {
    grep "real" | cut -d' ' -f2
}
run_all_utests() {
    for ut in "$UTEST_DIR"/*UTest; do
        "$ut" > /dev/null
    done
}
for i in $(seq 1 $N); do
    time -p run_all_utests 
done |& get_real_time | st | column -t