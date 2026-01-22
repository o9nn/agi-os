#! /bin/bash
echo "Shutting down the word-pair text feeder ..."
ps aux |grep pair-submit |grep $(id -nu) | grep -v grep | cut -b10-15 | xargs kill -SIGHUP
sleep 2
echo "Killing guile ..."
ps aux |grep guile |grep $(id -nu) | grep -v grep | cut -b10-15 | xargs kill -SIGHUP