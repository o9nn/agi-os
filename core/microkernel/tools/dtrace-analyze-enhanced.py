import sys
import json
import struct
import argparse
import matplotlib.pyplot as plt
import numpy as np
from datetime import datetime, timedelta
from collections import defaultdict, namedtuple
Event = namedtuple('Event', ['probe_id', 'timestamp', 'cpu_id', 'thread_id', 'task_id', 'args'])
ProbeInfo = namedtuple('ProbeInfo', ['id', 'type', 'name', 'function', 'enabled', 'fire_count'])
Metrics = namedtuple('Metrics', ['total_events', 'active_probes', 'buffer_overflows', 'probe_overhead'])
class DTRaceAnalyzer:
    def __init__(self):
        self.events = []
        self.probes = {}
        self.metrics = None
        self.start_time = None
        self.end_time = None
    def load_data(self, filename):
        try:
            with open(filename, 'rb') as f:
                data = f.read()
            if self.parse_binary_data(data):
                print(f'Loaded {len(self.events)} events from binary format')
                return True
            return self.parse_text_data(filename)
        except Exception as e:
            print(f'Error loading data: {e}')
            return False
    def parse_binary_data(self, data):
        offset = 0
        event_size = 8 * 8
        while offset + event_size <= len(data):
            try:
                fields = struct.unpack('<8Q', data[offset:offset + event_size])
                event = Event(probe_id=fields[0], timestamp=fields[1], cpu_id=fields[2], thread_id=fields[3], task_id=fields[4], args=fields[5:])
                self.events.append(event)
                offset += event_size
            except:
                break
        return len(self.events) > 0
    def parse_text_data(self, filename):
        try:
            with open(filename, 'r') as f:
                for line in f:
                    if line.startswith('#') or not line.strip():
                        continue
                    parts = line.strip().split()
                    if len(parts) >= 6:
                        event = Event(probe_id=int(parts[0]), timestamp=int(parts[1]), cpu_id=int(parts[2]), thread_id=int(parts[3]), task_id=int(parts[4]), args=tuple((int(x) for x in parts[5:])))
                        self.events.append(event)
            return True
        except:
            return False
    def analyze_timeline(self):
        if not self.events:
            return
        self.events.sort(key=lambda e: e.timestamp)
        self.start_time = self.events[0].timestamp
        self.end_time = self.events[-1].timestamp
        duration = (self.end_time - self.start_time) / 1000000.0
        print(f'\n=== Timeline Analysis ===')
        print(f'Total events: {len(self.events)}')
        print(f'Duration: {duration:.3f} seconds')
        print(f'Event rate: {len(self.events) / duration:.1f} events/sec')
        probe_counts = defaultdict(int)
        for event in self.events:
            probe_counts[event.probe_id] += 1
        print(f'\nEvents by probe:')
        for probe_id, count in sorted(probe_counts.items()):
            percentage = count / len(self.events) * 100
            print(f'  Probe {probe_id}: {count} events ({percentage:.1f}%)')
    def analyze_performance(self):
        if not self.events:
            return
        print(f'\n=== Performance Analysis ===')
        intervals = []
        for i in range(1, len(self.events)):
            interval = self.events[i].timestamp - self.events[i - 1].timestamp
            intervals.append(interval)
        if intervals:
            avg_interval = np.mean(intervals)
            std_interval = np.std(intervals)
            print(f'Average inter-event interval: {avg_interval / 1000:.2f} ms')
            print(f'Interval std deviation: {std_interval / 1000:.2f} ms')
            threshold = avg_interval + 3 * std_interval
            bottlenecks = [(i, interval) for i, interval in enumerate(intervals) if interval > threshold]
            if bottlenecks:
                print(f'Potential bottlenecks detected: {len(bottlenecks)}')
                for i, (event_idx, interval) in enumerate(bottlenecks[:5]):
                    print(f'  #{i + 1}: {interval / 1000:.2f}ms gap at event {event_idx}')
        cpu_counts = defaultdict(int)
        for event in self.events:
            cpu_counts[event.cpu_id] += 1
        print(f'\nCPU distribution:')
        for cpu_id, count in sorted(cpu_counts.items()):
            percentage = count / len(self.events) * 100
            print(f'  CPU {cpu_id}: {count} events ({percentage:.1f}%)')
    def detect_anomalies(self):
        if not self.events:
            return
        print(f'\n=== Anomaly Detection ===')
        probe_events = defaultdict(list)
        for event in self.events:
            probe_events[event.probe_id].append(event)
        anomalies_found = False
        for probe_id, events in probe_events.items():
            if len(events) < 10:
                continue
            timestamps = [e.timestamp for e in events]
            intervals = np.diff(timestamps)
            if len(intervals) > 0:
                mean_interval = np.mean(intervals)
                std_interval = np.std(intervals)
                clusters = []
                cluster_start = 0
                for i, interval in enumerate(intervals):
                    if interval < mean_interval * 0.1:
                        if cluster_start == 0:
                            cluster_start = i
                    elif cluster_start > 0:
                        cluster_size = i - cluster_start + 1
                        if cluster_size >= 5:
                            clusters.append((cluster_start, cluster_size))
                        cluster_start = 0
                if clusters:
                    print(f'Probe {probe_id}: Event clustering detected')
                    for start, size in clusters[:3]:
                        print(f'  Cluster: {size} events starting at event {start}')
                    anomalies_found = True
        if not anomalies_found:
            print('No significant anomalies detected')
    def generate_visualizations(self, output_dir='./'):
        if not self.events:
            print('No data to visualize')
            return
        print(f'\n=== Generating Visualizations ===')
        try:
            self.plot_timeline(f'{output_dir}/dtrace_timeline.png')
            print('Generated timeline plot')
            self.plot_probe_heatmap(f'{output_dir}/dtrace_heatmap.png')
            print('Generated probe activity heatmap')
            self.plot_performance_distribution(f'{output_dir}/dtrace_performance.png')
            print('Generated performance distribution')
        except ImportError:
            print('Matplotlib not available - skipping visualizations')
        except Exception as e:
            print(f'Error generating visualizations: {e}')
    def plot_timeline(self, filename):
        plt.figure(figsize=(12, 6))
        times = [(e.timestamp - self.start_time) / 1000000.0 for e in self.events]
        probe_ids = [e.probe_id for e in self.events]
        plt.scatter(times, probe_ids, alpha=0.6, s=1)
        plt.xlabel('Time (seconds)')
        plt.ylabel('Probe ID')
        plt.title('DTrace Event Timeline')
        plt.grid(True, alpha=0.3)
        plt.tight_layout()
        plt.savefig(filename, dpi=150)
        plt.close()
    def plot_probe_heatmap(self, filename):
        if len(self.events) == 0:
            return
        plt.figure(figsize=(10, 6))
        duration = self.end_time - self.start_time
        bin_size = duration / 100
        probe_ids = sorted(set((e.probe_id for e in self.events)))
        heatmap_data = np.zeros((len(probe_ids), 100))
        for event in self.events:
            bin_idx = min(99, int((event.timestamp - self.start_time) / bin_size))
            probe_idx = probe_ids.index(event.probe_id)
            heatmap_data[probe_idx, bin_idx] += 1
        plt.imshow(heatmap_data, aspect='auto', origin='lower', cmap='hot')
        plt.colorbar(label='Event Count')
        plt.xlabel('Time Bins')
        plt.ylabel('Probe ID')
        plt.title('Probe Activity Heatmap')
        plt.yticks(range(len(probe_ids)), probe_ids)
        plt.tight_layout()
        plt.savefig(filename, dpi=150)
        plt.close()
    def plot_performance_distribution(self, filename):
        plt.figure(figsize=(15, 10))
        plt.subplot(2, 2, 1)
        intervals = []
        for i in range(1, len(self.events)):
            interval = (self.events[i].timestamp - self.events[i - 1].timestamp) / 1000
            intervals.append(min(interval, 100))
        plt.hist(intervals, bins=50, alpha=0.7, edgecolor='black')
        plt.xlabel('Inter-event Interval (ms)')
        plt.ylabel('Frequency')
        plt.title('Inter-event Interval Distribution')
        plt.subplot(2, 2, 2)
        cpu_ids = [e.cpu_id for e in self.events]
        unique_cpus = sorted(set(cpu_ids))
        cpu_counts = [cpu_ids.count(cpu) for cpu in unique_cpus]
        plt.bar(unique_cpus, cpu_counts)
        plt.xlabel('CPU ID')
        plt.ylabel('Event Count')
        plt.title('Events by CPU')
        plt.subplot(2, 2, 3)
        probe_ids = [e.probe_id for e in self.events]
        unique_probes = sorted(set(probe_ids))
        probe_counts = [probe_ids.count(probe) for probe in unique_probes]
        plt.bar(unique_probes, probe_counts)
        plt.xlabel('Probe ID')
        plt.ylabel('Event Count')
        plt.title('Events by Probe')
        plt.subplot(2, 2, 4)
        duration_sec = (self.end_time - self.start_time) // 1000000
        if duration_sec > 0:
            rates = []
            times = []
            for sec in range(int(duration_sec)):
                window_start = self.start_time + sec * 1000000
                window_end = window_start + 1000000
                count = sum((1 for e in self.events if window_start <= e.timestamp < window_end))
                rates.append(count)
                times.append(sec)
            plt.plot(times, rates)
            plt.xlabel('Time (seconds)')
            plt.ylabel('Events per Second')
            plt.title('Event Rate Over Time')
        plt.tight_layout()
        plt.savefig(filename, dpi=150)
        plt.close()
    def generate_report(self, output_file='dtrace_report.txt'):
        with open(output_file, 'w') as f:
            f.write('GNU Mach DTrace Analysis Report\n')
            f.write('=' * 40 + '\n')
            f.write(f'Generated: {datetime.now()}\n\n')
            if self.events:
                duration = (self.end_time - self.start_time) / 1000000.0
                f.write(f'Total Events: {len(self.events)}\n')
                f.write(f'Duration: {duration:.3f} seconds\n')
                f.write(f'Event Rate: {len(self.events) / duration:.1f} events/sec\n\n')
                probe_counts = defaultdict(int)
                for event in self.events:
                    probe_counts[event.probe_id] += 1
                f.write('Probe Activity:\n')
                for probe_id, count in sorted(probe_counts.items()):
                    percentage = count / len(self.events) * 100
                    f.write(f'  Probe {probe_id}: {count} events ({percentage:.1f}%)\n')
        print(f'Generated comprehensive report: {output_file}')
def main():
    parser = argparse.ArgumentParser(description='Enhanced DTrace Analysis Tool')
    parser.add_argument('input_file', help='DTrace data file to analyze')
    parser.add_argument('--output-dir', default='./', help='Output directory for visualizations')
    parser.add_argument('--report', default='dtrace_report.txt', help='Output report filename')
    parser.add_argument('--visualize', action='store_true', help='Generate visualization plots')
    parser.add_argument('--detect-anomalies', action='store_true', help='Run anomaly detection')
    args = parser.parse_args()
    analyzer = DTRaceAnalyzer()
    if not analyzer.load_data(args.input_file):
        print('Failed to load data file')
        return 1
    analyzer.analyze_timeline()
    analyzer.analyze_performance()
    if args.detect_anomalies:
        analyzer.detect_anomalies()
    if args.visualize:
        analyzer.generate_visualizations(args.output_dir)
    analyzer.generate_report(args.report)
    return 0
if __name__ == '__main__':
    sys.exit(main())