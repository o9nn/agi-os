#!/bin/bash -noprofile
prompt='login: '
user=''
test "$_LOGIN_RETRY" && echo ''
unset _LOGIN_RETRY
until [ "$user" ]; do
echo -n "$prompt"
read user args || exit 0
done
exec login "$@" -p --paranoid -R-aSHELL="$0" -R-aMOTD -R-p -R-e_LOGIN_RETRY=yes "$user" $args