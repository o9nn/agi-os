import sys
import json
import argparse
from datetime import datetime, timedelta
def create_ascii_timeline(events, width=80):
    if not events:
        return 'No events to display'
    sorted_events = sorted(events, key=lambda x: x.get('timestamp', 0))
    if len(sorted_events) < 2:
        return 'Need at least 2 events for timeline'
    start_time = sorted_events[0]['timestamp']
    end_time = sorted_events[-1]['timestamp']
    time_range = end_time - start_time
    if time_range == 0:
        return 'All events have the same timestamp'
    timeline = [' '] * width
    labels = []
    for event in sorted_events:
        relative_time = event['timestamp'] - start_time
        position = int(relative_time / time_range * (width - 1))
        probe_id = event.get('probe_id', 0)
        if probe_id == 1:
            char = 'T'
        elif probe_id == 2:
            char = 'I'
        elif probe_id == 3:
            char = 'V'
        else:
            char = '*'
        timeline[position] = char
        if position not in [label[0] for label in labels]:
            labels.append((position, char, probe_id))
    result = []
    result.append('Timeline (T=thread_switch, I=ipc_send, V=vm_fault, *=other):')
    result.append(''.join(timeline))
    result.append(f'Time range: {time_range / 1000000:.1f} ms')
    return '\n'.join(result)
def analyze_frequency(events):
    if not events:
        return 'No events to analyze'
    type_counts = {}
    probe_names = {1: 'thread_switch', 2: 'ipc_send', 3: 'vm_fault'}
    for event in events:
        probe_id = event.get('probe_id', 0)
        probe_name = probe_names.get(probe_id, f'probe_{probe_id}')
        type_counts[probe_name] = type_counts.get(probe_name, 0) + 1
    result = []
    result.append('Event Frequency Analysis:')
    result.append('=' * 40)
    max_count = max(type_counts.values()) if type_counts else 0
    for probe_name, count in sorted(type_counts.items()):
        bar_length = int(count / max_count * 30) if max_count > 0 else 0
        bar = '*' * bar_length
        result.append(f'{probe_name:15} {count:4d} {bar}')
    return '\n'.join(result)
def create_performance_summary(events):
    if not events:
        return 'No events to analyze'
    sorted_events = sorted(events, key=lambda x: x.get('timestamp', 0))
    total_events = len(events)
    if total_events < 2:
        return f'Performance Summary: {total_events} event(s) - need more data'
    start_time = sorted_events[0]['timestamp']
    end_time = sorted_events[-1]['timestamp']
    duration_ms = (end_time - start_time) / 1000000.0
    if duration_ms > 0:
        events_per_second = total_events / (duration_ms / 1000.0)
    else:
        events_per_second = 0
    unique_threads = len(set((event.get('thread_id', 0) for event in events)))
    unique_tasks = len(set((event.get('task_id', 0) for event in events)))
    unique_cpus = len(set((event.get('cpu_id', 0) for event in events)))
    result = []
    result.append('Performance Summary:')
    result.append('=' * 30)
    result.append(f'Total events:     {total_events}')
    result.append(f'Duration:         {duration_ms:.2f} ms')
    result.append(f'Events/second:    {events_per_second:.1f}')
    result.append(f'Unique threads:   {unique_threads}')
    result.append(f'Unique tasks:     {unique_tasks}')
    result.append(f'CPUs involved:    {unique_cpus}')
    return '\n'.join(result)
def generate_sample_data():
    events = []
    base_time = 1000000000
    for i in range(20):
        event = {'probe_id': i % 3 + 1, 'timestamp': base_time + i * 100000, 'cpu_id': i % 2, 'thread_id': 4096 + i % 5, 'task_id': 8192 + i % 3, 'args': [3735928559 + i, 3405691582 + i, 0, 0, 0, 0]}
        events.append(event)
    return events
def main():
    parser = argparse.ArgumentParser(description='DTrace Visualization Tool for GNU Mach')
    parser.add_argument('-f', '--file', help='JSON file containing DTrace events')
    parser.add_argument('-t', '--timeline', action='store_true', help='Show timeline visualization')
    parser.add_argument('-q', '--frequency', action='store_true', help='Show frequency analysis')
    parser.add_argument('-p', '--performance', action='store_true', help='Show performance summary')
    parser.add_argument('-a', '--all', action='store_true', help='Show all visualizations')
    parser.add_argument('--demo', action='store_true', help='Use sample data for demonstration')
    parser.add_argument('-w', '--width', type=int, default=80, help='Width of timeline display (default: 80)')
    args = parser.parse_args()
    show_timeline = args.timeline or args.all
    show_frequency = args.frequency or args.all
    show_performance = args.performance or args.all
    if not (show_timeline or show_frequency or show_performance):
        show_timeline = show_frequency = show_performance = True
    if args.demo or not args.file:
        print('Using sample data for demonstration')
        print('In a real implementation, this would read data from the kernel or a log file\n')
        events = generate_sample_data()
    else:
        try:
            with open(args.file, 'r') as f:
                data = json.load(f)
                events = data.get('events', [])
        except FileNotFoundError:
            print(f"Error: File '{args.file}' not found")
            return 1
        except json.JSONDecodeError:
            print(f"Error: Invalid JSON in file '{args.file}'")
            return 1
    print('GNU Mach DTrace Visualization Tool')
    print('=' * 50)
    print()
    if show_performance:
        print(create_performance_summary(events))
        print()
    if show_frequency:
        print(analyze_frequency(events))
        print()
    if show_timeline:
        print(create_ascii_timeline(events, args.width))
        print()
    print('Visualization complete.')
    return 0
if __name__ == '__main__':
    sys.exit(main())