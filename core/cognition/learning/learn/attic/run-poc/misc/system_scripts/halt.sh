#! /bin/bash
echo "Shutting down the text feeder ..."
ps aux |grep wiki-ss |grep $(id -nu) | grep -v grep | cut -b10-15 | xargs kill -SIGHUP
echo "Killing guile ..."
ps aux |grep guile |grep $(id -nu) | grep -v grep | cut -b10-15 | xargs kill -SIGHUP