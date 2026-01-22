#!/bin/bash
SOURCE=$1
SOURCE_PATH=`dirname $1`
SOURCE_NAME=`basename $1`
DEPTH=`echo $SOURCE_PATH | awk -F "/" '{ print NF }'`
LAST_DIR=`echo "$SOURCE_PATH" | awk -F "/" '{ print $NF }'`
ROOT_SRC_DIR=`dirname $0 | awk -F "/" '{ for (i=2; i<NF; i++) printf "/"$i; \
print "" }'`
if [ "${SOURCE_PATH}" = "${ROOT_SRC_DIR}" ]; then
IS_MAINPAGE=1
fi
if [ -n "${IS_MAINPAGE}" ]; then
echo "//----
/*! @mainpage
"
tail -n+4 $SOURCE
cat $SOURCE_PATH/AUTHORS
echo "
@section download Download
http://opencog.org/wiki/BuildingOpenCog
"
else
IN_CODE_DIR="${SOURCE_PATH
PATH_UNDERSCORE=`echo $IN_CODE_DIR | sed -e 's/\//_/g'`
echo "//----
/*! @page ${PATH_UNDERSCORE} ${LAST_DIR}
From directory \\ref ${SOURCE_PATH
"
fi
echo "@section submodules Components:"
for i in `find ${SOURCE_PATH}/ -name README | sort`
do
SUBDIR_PATH=`dirname $i`
if [ `echo ${SUBDIR_PATH} | awk -F "/" '{ print $NF }'` != ".bzr"  ]; then
SUB_DEPTH=$(( ${DEPTH} + 1 ))
if [ `echo ${SUBDIR_PATH} | awk -F "/" '{ print NF }'` -eq ${SUB_DEPTH} ]
then
SUBPATH_UNDERSCORE=`echo ${SUBDIR_PATH
echo " - @subpage ${SUBPATH_UNDERSCORE
fi
fi;
done
if [ -z "${IS_MAINPAGE}" ]; then
echo ""
cat $SOURCE
fi
echo "*/
//----"