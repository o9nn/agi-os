#!/usr/bin/env bash
printf "
printf "
git log --format='%an <%ae>' --reverse --date=short master | awk '!seen[$0]++' | sort >> AUTHORS
sed -i '' 's/^jdoe/John Doe/g' AUTHORS