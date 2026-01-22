#!/bin/bash
if [ $
then 
  echo "Usage: ./text-process.sh <mode> <language>"
  exit 0
fi
case $1 in
   pairs)
      directory=beta-pages
      ;;
   mst)
      directory=gamma-pages
      ;;
   *)
      echo "Usage: ./text-process.sh <mode> <language>"
      echo "<mode> must be either pairs or mst"
      exit 0
esac
source ./config/det-port-num.sh $1 $2
time find $directory -type f -exec ./process-one.sh $1 $2 {} localhost $PORT \;