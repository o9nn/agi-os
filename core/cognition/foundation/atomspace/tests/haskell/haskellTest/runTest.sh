BIN_DIR=$1
if [ "$(id -u)" -ne 0 ]
then
    stack test --extra-lib-dirs=${BIN_DIR}
else
    echo "Can't run Haskell-Tests as root"
fi