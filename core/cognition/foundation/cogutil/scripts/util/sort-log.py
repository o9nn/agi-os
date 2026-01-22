import re
import sys
import datetime
import argparse
import functools
def datetime_from_str(time_str):
    fmt = '%Y-%m-%d %H:%M:%S:%f'
    return datetime.datetime.strptime(time_str, fmt)
def compare(x, y):
    if x < y:
        return -1
    elif x > y:
        return 1
    else:
        return 0
def chrono_compare(x, y):
    xtln, xdt, xln = x
    ytln, ydt, yln = y
    return compare((xdt, xln), (ydt, yln))
def thread_compare(x, y):
    xtln, xdt, xln = x
    ytln, ydt, yln = y
    if xtln and ytln and (xtln != ytln):
        return compare((xtln, xln), (ytln, yln))
    else:
        return compare(xln, yln)
def ln_compare(x, y):
    xtln, xdt, xln = x
    ytln, ydt, yln = y
    return compare(xln, yln)
def thread_chrono_compare(x, y):
    xtln, xdt, xln = x
    ytln, ydt, yln = y
    if xtln and ytln and (xtln != ytln):
        return compare(x, y)
    else:
        return chrono_compare(x, y)
if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Sort the given log according to a given order, such as chronological, thread cohesive, etc.')
    parser.add_argument('logfile', help='Log file to sort')
    parser.add_argument('-c', '--chrono', action='store_true', default=True, help='Sort chronologically. Indeed, such order can be broken if the logger is asynchronous.')
    parser.add_argument('-t', '--thread', action='store_true', default=False, help='Sort such that messages from the same thread are clumped together.')
    parser.add_argument('-o', '--output', help='Output file. If unused stdout is used instead')
    args = parser.parse_args()
    timestamp_re = '\\[(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}:\\d{3})\\]'
    timestamp_prog = re.compile(timestamp_re)
    thread_re = '.*\\[(thread-\\d+)\\]'
    thread_prog = re.compile(thread_re)
    of = open(args.output, 'w') if args.output else sys.stdout
    key2txt = {}
    tln, dt, ln = (None, None, None)
    line_num = 0
    t2ln = {}
    for l in open(args.logfile):
        timestamp_m = timestamp_prog.match(l)
        thread_m = thread_prog.match(l)
        if timestamp_m:
            ln = line_num
            if thread_m:
                thread = thread_m.group(1)
                if thread not in t2ln:
                    t2ln[thread] = ln
                tln = t2ln[thread]
            else:
                tln = None
            dt = datetime_from_str(timestamp_m.group(1))
            key2txt[tln, dt, ln] = l
        elif dt:
            key2txt[tln, dt, ln] += l
        else:
            of.write(l)
        line_num += 1
    if args.chrono and args.thread:
        cmp = thread_chrono_compare
    elif args.chrono:
        cmp = chrono_compare
    elif args.thread:
        cmp = thread_compare
    else:
        cmp = ln_compare
    for tln, dt, ln in sorted(key2txt.keys(), key=functools.cmp_to_key(cmp)):
        of.write(key2txt[tln, dt, ln])