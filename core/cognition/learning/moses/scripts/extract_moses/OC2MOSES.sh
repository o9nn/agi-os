#!/bin/bash
PROJECT=moses3
STATIC_LINK=0
if [ $
    echo "Wrong number of arguments"
    echo "Usage: $0 MOSES_DIR"
    echo "MOSES_DIR is the name of the directory to create and put the"
    echo "MOSES project under"
    echo "Once created, compile as follows:"
    echo "cd $PROJECT && mkdir bin-release && cd bin-release"
    echo "cmake -DCMAKE_BUILT_TYPE=Release .. && make -j4"
    exit 1
fi
echo "Extracting MOSES out of OpenCog, please wait..."
PRG_DIR="$( cd "$( dirname "$0" )" && pwd )"
OC_PATH="$PRG_DIR/../../../../"
MOSES_DIR="$1"
INIT_DIR="$PWD"
mkdir "$MOSES_DIR"
ABS_MOSES_DIR="$INIT_DIR/$MOSES_DIR"
cd "$MOSES_DIR"
mkdir lib
mkdir "$PROJECT"
mkdir doc
cd "$PROJECT"
mkdir util
cd "$OC_PATH/opencog"
for dir in comboreduct learning/moses learning/feature-selection; do
    find $dir -type d -exec mkdir -p "$ABS_MOSES_DIR/$PROJECT/{}" \;
done
cp "$OC_PATH/CMakeLists.txt" "$ABS_MOSES_DIR/CMakeLists.txt.orig"
cp "$OC_PATH/lib/Summary.cmake" "$ABS_MOSES_DIR/lib"
UTIL_FILES=(algorithm.h based_variant.h concurrent_queue.h Config.cc Config.h Counter.h digraph.h dorepeat.h exceptions.cc exceptions.h functional.h hashing.h iostreamContainer.h KLD.h lazy_normal_selector.cc lazy_normal_selector.h lazy_random_selector.cc lazy_random_selector.h lazy_selector.cc lazy_selector.h Logger.cc Logger.h log_prog_name.h log_prog_name.cc lru_cache.h macros.h MannWhitneyU.h mt19937ar.cc mt19937ar.h numeric.h oc_assert.cc oc_assert.h oc_omp.cc oc_omp.h platform.cc platform.h pool.h RandGen.h random.h ranking.h selection.h tree.cc tree.h comprehension.h backtrace-symbols.h backtrace-symbols.c jaccard_index.h boost_ext/accumulators/statistics/geometric_mean_mirror.h)
mkdir -p "$ABS_MOSES_DIR/$PROJECT/util/boost_ext/accumulators/statistics"
for f in ${UTIL_FILES[@]}; do
    sed s/opencog/"$PROJECT"/g "$OC_PATH"/opencog/util/$f > "$ABS_MOSES_DIR/$PROJECT/util/$f"
done
cd "$OC_PATH/opencog"
for dir in comboreduct learning/moses learning/feature-selection; do
    find $dir -path "*/diary" -prune , -path "*/sample" -prune , -not -name "*~" -and -not -name "*.bak.?" -and -type f > find.txt
    while read -r; do
        if [ $(basename $REPLY) == "CMakeLists.txt" ]; then
            if [ $STATIC_LINK -eq 1 ]; then
                sed -e s/opencog/"$PROJECT"/g  -e s/SHARED/STATIC/ "$REPLY" > "$ABS_MOSES_DIR/$PROJECT/$REPLY"
            else
                sed -e s/opencog/"$PROJECT"/g  "$REPLY" > "$ABS_MOSES_DIR/$PROJECT/$REPLY"
            fi
        else
            sed s/opencog/"$PROJECT"/g "$REPLY" > "$ABS_MOSES_DIR/$PROJECT/$REPLY"
        fi
    done < find.txt
    rm find.txt
done
cp -fr "$OC_PATH/doc/comboreduct" "$ABS_MOSES_DIR/doc"
cp -fr "$OC_PATH/doc/moses" "$ABS_MOSES_DIR/doc"
cd "$OC_PATH/scripts/learning/moses"
mkdir "$ABS_MOSES_DIR/scripts"
for f in *; do
    if [[ "$f" != extract_moses ]]; then
        cp -fr "$f" "$ABS_MOSES_DIR/scripts"
    fi
done
rm -f "$ABS_MOSES_DIR"/moses3/learning/moses/example-data/wine*
rm -f "$ABS_MOSES_DIR"/moses3/learning/moses/example-data/magic*
echo "PROJECT($PROJECT)" > "$ABS_MOSES_DIR/CMakeLists.txt"
echo "SET(CMAKE_BUILD_TYPE Release)" >> "$ABS_MOSES_DIR/CMakeLists.txt"
if [ $STATIC_LINK -eq 1 ]; then
    echo "SET(CMAKE_FIND_LIBRARY_SUFFIXES .a \${CMAKE_FIND_LIBRARY_SUFFIXES})" >> $ABS_MOSES_DIR/CMakeLists.txt
fi
if [ $STATIC_LINK -eq 1 ]; then
    grep -Ei 'cmake_minimum_required|cmake_c_flags|rpath|confdir|cmake_policy|dproject_source_dir|dproject_binary_dir|man_install|boost|_mpi|mpi_|mpi\)|\(mpi|mpi |have_util|have_comboreduct|have_moses|have_feature_selection|summary_show|summary\.cmake' "$ABS_MOSES_DIR/CMakeLists.txt.orig" | sed s/opencog/$PROJECT/g | sed s/'SET(CMAKE_C_FLAGS_RELEASE "-O2 -fstack-protector")'/'SET(CMAKE_C_FLAGS_RELEASE "-O2 -fstack-protector -static-libstdc++ -static-libgcc")'/ >> "$ABS_MOSES_DIR/CMakeLists.txt"
else
    grep -Ei 'cmake_minimum_required|cmake_c_flags|rpath|confdir|cmake_policy|dproject_source_dir|dproject_binary_dir|man_install|boost|_mpi|mpi_|mpi\)|\(mpi|mpi |have_util|have_comboreduct|have_moses|have_feature_selection|summary_show|summary\.cmake' "$ABS_MOSES_DIR/CMakeLists.txt.orig" | sed s/opencog/$PROJECT/g | sed s/'SET(CMAKE_C_FLAGS_RELEASE "-O2 -fstack-protector")'/'SET(CMAKE_C_FLAGS_RELEASE "-O2 -fstack-protector -fPIC")'/ >> "$ABS_MOSES_DIR/CMakeLists.txt"
fi
echo "ADD_SUBDIRECTORY($PROJECT)" >> "$ABS_MOSES_DIR/CMakeLists.txt"
rm "$ABS_MOSES_DIR/CMakeLists.txt.orig"
echo "ADD_SUBDIRECTORY(util)" > "$ABS_MOSES_DIR/$PROJECT/CMakeLists.txt"
echo "ADD_SUBDIRECTORY(comboreduct)" >> "$ABS_MOSES_DIR/$PROJECT/CMakeLists.txt"
echo "ADD_SUBDIRECTORY(learning)" >> "$ABS_MOSES_DIR/$PROJECT/CMakeLists.txt"
ABS_PLM_DIR="$ABS_MOSES_DIR/$PROJECT/learning/moses/"
cd "$OC_PATH"
GIT_DESCRIBE=$(git describe)
cd "$INIT_DIR"
sed -i s/'EXECUTE_PROCESS(COMMAND git describe "${PROJECT_SOURCE_DIR}" OUTPUT_VARIABLE MOSES_BZR_REVNO)'/'
sed -i "1i SET (MOSES_BZR_REVNO $GIT_DESCRIBE)" "$ABS_PLM_DIR/CMakeLists.txt"
echo "ADD_SUBDIRECTORY(moses)" > "$ABS_MOSES_DIR/$PROJECT/learning/CMakeLists.txt"
echo "ADD_SUBDIRECTORY(feature-selection)" >> "$ABS_MOSES_DIR/$PROJECT/learning/CMakeLists.txt"
echo "ADD_LIBRARY(cogutil STATIC algorithm based_variant digraph dorepeat exceptions functional.h iostreamContainer lazy_normal_selector lazy_random_selector lazy_selector random oc_assert Config Logger log_prog_name lru_cache macros mt19937ar platform tree oc_omp Counter KLD MannWhitneyU ranking)" > "$ABS_MOSES_DIR/$PROJECT/util/CMakeLists.txt"
echo "TARGET_LINK_LIBRARIES(cogutil \${Boost_FILESYSTEM_LIBRARY} \${Boost_SYSTEM_LIBRARY} \${Boost_REGEX_LIBRARY} \${Boost_THREAD_LIBRARY})" >> "$ABS_MOSES_DIR/$PROJECT/util/CMakeLists.txt"
echo "INSTALL(FILES ${UTIL_FILES[@]} DESTINATION \"include/\${PROJECT_NAME}/util\")" >> "$ABS_MOSES_DIR/$PROJECT/util/CMakeLists.txt"
find "$ABS_MOSES_DIR/$PROJECT" -type f -exec "$PRG_DIR/AGPL2Apache.py" {} \;
mv "$ABS_MOSES_DIR/$PROJECT/learning/moses/LICENSE" "$ABS_MOSES_DIR"
cp "$PRG_DIR/readme_to_copy" "$ABS_MOSES_DIR/README"
cp "$PRG_DIR/moses.spec" "$ABS_MOSES_DIR"
tar -jcf "$MOSES_DIR.tar.bz2" "$MOSES_DIR"
echo "Done"