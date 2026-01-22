#!/bin/sh
timeout=10
trap "exit 0" QUIT
{
sleep $timeout
kill -3 $$ 2>/dev/null
} &
read -r input
exit_code=$?
cleaned_input=$(echo ${input} | sed "s/[^a-zA-Z0-9]//g")
if [ ${exit_code} -eq 0 ] && [ "${cleaned_input}" = "PING" ];then
echo "PONG"
fi
exit 0