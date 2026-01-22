import sys
import re
usage = 'Usage: ' + sys.argv[0] + ' FCSHANDLE LOGFILE'
timestamp_re = '\\[\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}:\\d{3}\\]'
debug_re = '\\[DEBUG\\]'
ure_re = '\\[URE\\]'
iter_re = 'Iteration (\\d+)'
iteration_re = '({} )?{} {} {}'.format(timestamp_re, debug_re, ure_re, iter_re)
iteration_cre = re.compile(iteration_re)
select_andbit_re = 'Selected and-BIT for expansion:'
from_re = '({} )?{} {} {}'.format(timestamp_re, debug_re, ure_re, select_andbit_re)
from_cre = re.compile(from_re)
expand_andbit_re = 'Expanded forward chainer strategy:'
to_re = '({} )?{} {} {}'.format(timestamp_re, debug_re, ure_re, expand_andbit_re)
to_cre = re.compile(to_re)
select_bn_re = 'Selected BIT-node for expansion:'
inter_re = '({} )?{} {} {}'.format(timestamp_re, debug_re, ure_re, select_bn_re)
inter_cre = re.compile(inter_re)
handle_re = '\\) ; (\\[\\d+\\]\\[\\d+\\])'
handle_cre = re.compile(handle_re)
if len(sys.argv) != 3:
    print(usage)
    exit(1)
fcs_handle = sys.argv[1]
logfile = sys.argv[2]
i2ft = dict()
src = ''
for l in open(logfile):
    ls = l.rstrip()
    m = iteration_cre.match(ls)
    if m:
        iteration = int(m.group(2))
        continue
    m = from_cre.match(ls)
    if m:
        src = 'from'
        continue
    m = to_cre.match(ls)
    if m:
        src = 'to'
        continue
    m = inter_cre.match(ls)
    if m:
        src = ''
        continue
    m = handle_cre.match(ls)
    if m:
        if src == 'from':
            from_handle = m.group(1)
            continue
        if src == 'to':
            to_handle = m.group(1)
            i2ft[iteration] = (from_handle, to_handle)
            if fcs_handle in to_handle:
                fcs_handle_iteration = iteration
                break
            src = ''
            continue
i2ft_trace = dict()
iteration = fcs_handle_iteration
while iteration != 0:
    i2ft_trace[iteration] = i2ft[iteration]
    for i in range(iteration):
        if i in i2ft and i2ft[i][1] == i2ft[iteration][0]:
            iteration = i
            break
    if iteration != i:
        iteration = 0
iteration = -1
for l in open(logfile):
    ls = l.rstrip()
    m = iteration_cre.match(ls)
    if m:
        iteration = int(m.group(2))
    if iteration == 0 or iteration in i2ft_trace:
        print(ls)