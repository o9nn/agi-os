set -u
N=10
BUILD_DIR_NAME=build
PRG_PATH="$(readlink -f "$0")"
PRG_DIR="$(dirname "$PRG_PATH")"
UTEST_DIR="$PRG_DIR/../$BUILD_DIR_NAME"
get_real_time() {
    grep "real" | cut -d' ' -f2
}
run_utests() {
    cd "$UTEST_DIR";
    make -j4 test ARGS=-j4
    cd -
}
for i in $(seq 1 $N); do
    echo "Run unit test suite ($i/$N)" 1>&2
    (time -p run_utests 1>/dev/null) 2>&1
done | get_real_time | st | column -t