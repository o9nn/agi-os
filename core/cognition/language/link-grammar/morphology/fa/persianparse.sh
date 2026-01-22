#!/bin/sh
echo -e "!width=160\n$*\n\n" | ./stemmer.pl -u | link-parser fa