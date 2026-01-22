set -e -x
export BRANCH=${CIRCLE_BRANCH:-test7}
if [ -n "$DOCS" ] ; then 
    docker run --rm -it -v $PWD:/mnt -w /mnt/docs deltachat/doxygen doxygen
fi
docker run -e BRANCH -e MESONARGS -e TESTS -e DOCS \
           --rm -it -v $(pwd):/mnt -w /mnt \
           deltachat/coredeps ci_scripts/run_all.sh