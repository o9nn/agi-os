import sys
import re
usage = 'Usage: ' + sys.argv[0] + ' LOG_TRACE_FILE'
timestamp_re = '\\[\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}:\\d{3}\\]'
debug_re = '\\[DEBUG\\]'
ure_re = '\\[URE\\]'
select_andbit_re = 'Selected and-BIT for expansion:'
from_re = '({} )?{} {} {}'.format(timestamp_re, debug_re, ure_re, select_andbit_re)
from_cre = re.compile(from_re)
expand_andbit_re = 'Expanded forward chainer strategy:'
select_bn_re = 'Selected BIT-node for expansion:'
inter_re = '({} )?{} {} {}'.format(timestamp_re, debug_re, ure_re, select_bn_re)
inter_cre = re.compile(inter_re)
handle_re = '\\) ; \\[(\\d+)\\]\\[\\d+\\]'
handle_cre = re.compile(handle_re)
if len(sys.argv) != 2:
    print(usage)
    exit(1)
logtracefile = sys.argv[1]
src = ''
for l in open(logtracefile):
    ls = l.rstrip()
    m = from_cre.match(ls)
    if m:
        src = 'from'
        continue
    m = inter_cre.match(ls)
    if m:
        src = ''
        continue
    m = handle_cre.match(ls)
    if m:
        if src == 'from':
            print(m.group(1))
            continue