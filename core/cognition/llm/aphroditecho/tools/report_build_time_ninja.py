import argparse
import errno
import fnmatch
import os
import sys
from collections import defaultdict
long_count = 10
long_ext_count = 10
class Target:
    def __init__(self, start, end):
        self.start = start
        self.end = end
        self.targets = []
        self.weighted_duration = 0.0
    def Duration(self):
        return self.end - self.start
    def SetWeightedDuration(self, weighted_duration):
        self.weighted_duration = weighted_duration
    def WeightedDuration(self):
        epsilon = 2e-06
        if self.weighted_duration > self.Duration() + epsilon:
            print('%s > %s?' % (self.weighted_duration, self.Duration()))
        assert self.weighted_duration <= self.Duration() + epsilon
        return self.weighted_duration
    def DescribeTargets(self):
        result = ', '.join(self.targets)
        max_length = 65
        if len(result) > max_length:
            result = result[:max_length] + '...'
        return result
def ReadTargets(log, show_all):
    header = log.readline()
    assert header == '# ninja log v5\n', 'unrecognized ninja log version %r' % header
    targets_dict = {}
    last_end_seen = 0.0
    for line in log:
        parts = line.strip().split('\t')
        if len(parts) != 5:
            continue
        start, end, _, name, cmdhash = parts
        start = int(start) / 1000.0
        end = int(end) / 1000.0
        if not show_all and end < last_end_seen:
            targets_dict = {}
        target = None
        if cmdhash in targets_dict:
            target = targets_dict[cmdhash]
            if not show_all and (target.start != start or target.end != end):
                targets_dict = {}
                target = None
        if not target:
            targets_dict[cmdhash] = target = Target(start, end)
        last_end_seen = end
        target.targets.append(name)
    return list(targets_dict.values())
def GetExtension(target, extra_patterns):
    for output in target.targets:
        if extra_patterns:
            for fn_pattern in extra_patterns.split(';'):
                if fnmatch.fnmatch(output, '*' + fn_pattern + '*'):
                    return fn_pattern
        if output.endswith('type_mappings'):
            extension = 'type_mappings'
            break
        root, ext1 = os.path.splitext(output)
        _, ext2 = os.path.splitext(root)
        extension = ext2 + ext1
        if len(extension) == 0:
            extension = '(no extension found)'
        if ext1 in ['.pdb', '.dll', '.exe']:
            extension = 'PEFile (linking)'
            break
        if ext1 in ['.so', '.TOC']:
            extension = '.so (linking)'
            break
        if ext1 in ['.obj', '.o']:
            break
        if ext1 == '.jar':
            break
        if output.count('.mojom') > 0:
            extension = 'mojo'
            break
    return extension
def SummarizeEntries(entries, extra_step_types):
    task_start_stop_times = []
    earliest = -1
    latest = 0
    total_cpu_time = 0
    for target in entries:
        if earliest < 0 or target.start < earliest:
            earliest = target.start
        if target.end > latest:
            latest = target.end
        total_cpu_time += target.Duration()
        task_start_stop_times.append((target.start, 'start', target))
        task_start_stop_times.append((target.end, 'stop', target))
    length = latest - earliest
    weighted_total = 0.0
    task_start_stop_times.sort(key=lambda times: times[:2])
    running_tasks = {}
    last_time = task_start_stop_times[0][0]
    last_weighted_time = 0.0
    for event in task_start_stop_times:
        time, action_name, target = event
        num_running = len(running_tasks)
        if num_running > 0:
            last_weighted_time += (time - last_time) / float(num_running)
        if action_name == 'start':
            running_tasks[target] = last_weighted_time
        if action_name == 'stop':
            weighted_duration = last_weighted_time - running_tasks[target]
            target.SetWeightedDuration(weighted_duration)
            weighted_total += weighted_duration
            del running_tasks[target]
        last_time = time
    assert len(running_tasks) == 0
    if abs(length - weighted_total) > 500:
        print('Warning: Possible corrupt ninja log, results may be untrustworthy. Length = %.3f, weighted total = %.3f' % (length, weighted_total))
    entries_by_ext = defaultdict(list)
    for target in entries:
        extension = GetExtension(target, extra_step_types)
        entries_by_ext[extension].append(target)
    for key, values in entries_by_ext.items():
        print('    Longest build steps for %s:' % key)
        values.sort(key=lambda x: x.WeightedDuration())
        for target in values[-long_count:]:
            print('      %8.1f weighted s to build %s (%.1f s elapsed time)' % (target.WeightedDuration(), target.DescribeTargets(), target.Duration()))
    print('    %.1f s weighted time (%.1f s elapsed time sum, %1.1fx parallelism)' % (length, total_cpu_time, total_cpu_time * 1.0 / length))
    print('    %d build steps completed, average of %1.2f/s' % (len(entries), len(entries) / length))
def main():
    log_file = '.ninja_log'
    parser = argparse.ArgumentParser()
    parser.add_argument('-C', dest='build_directory', help='Build directory.')
    parser.add_argument('-s', '--step-types', help='semicolon separated fnmatch patterns for build-step grouping')
    parser.add_argument('--log-file', help='specific ninja log file to analyze.')
    args, _extra_args = parser.parse_known_args()
    if args.build_directory:
        log_file = os.path.join(args.build_directory, log_file)
    if args.log_file:
        log_file = args.log_file
    if args.step_types:
        global long_ext_count
        long_ext_count += len(args.step_types.split(';'))
    try:
        with open(log_file, 'r') as log:
            entries = ReadTargets(log, False)
            SummarizeEntries(entries, args.step_types)
    except IOError:
        print('Log file %r not found, no build summary created.' % log_file)
        return errno.ENOENT
if __name__ == '__main__':
    sys.exit(main())