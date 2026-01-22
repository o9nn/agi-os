#!/bin/bash
m=200000
r=1
k=3
E=2
B=2
P=20
a=hc
r=11
moses-exec -H pa -r "$r" -m "$m" -k "$k" -E "$E" -B "$B" -P "$P" -a "$a" -L > /dev/null &
r=22
moses-exec -H pa -r "$r" -m "$m" -k "$k" -E "$E" -B "$B" -P "$P" -a "$a" -L > /dev/null &
E=3
r=11
moses-exec -H pa -r "$r" -m "$m" -k "$k" -E "$E" -B "$B" -P "$P" -a "$a" -L > /dev/null &
r=22
moses-exec -H pa -r "$r" -m "$m" -k "$k" -E "$E" -B "$B" -P "$P" -a "$a" -L > /dev/null &