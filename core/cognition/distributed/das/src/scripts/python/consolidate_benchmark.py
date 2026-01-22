import argparse
import os
import re
import sqlite3
import sys
from collections import defaultdict
from contextlib import contextmanager
from dataclasses import dataclass
from typing import Optional, DefaultDict, TextIO, Generator
BenchmarkEntry = dict[str, str]
BenchmarkData = list[BenchmarkEntry]
ConsolidatedResults = DefaultDict[str, dict[str, list[BenchmarkEntry] | str]]
class FileManager:
    DEFAULT_COL_ORDER = ['Backend', 'Operation', 'MED', 'MIN', 'MAX', 'P50', 'P90', 'P99', 'TT', 'TPA', 'TP']
    @staticmethod
    def _get_active_columns(entries: list[BenchmarkEntry]) -> list[str]:
        present_columns = set()
        for entry in entries:
            present_columns.update(entry.keys())
        return [col for col in FileManager.DEFAULT_COL_ORDER if col in present_columns]
    @staticmethod
    def _calculate_column_widths(entries: list[BenchmarkEntry], columns: list[str]) -> dict[str, int]:
        widths = {col: len(col) for col in columns}
        for entry in entries:
            for col in columns:
                widths[col] = max(widths[col], len(str(entry.get(col, ''))))
        return widths
    @staticmethod
    def write_report(results: ConsolidatedResults, output_file: TextIO, header_text: str=''):
        if header_text:
            output_file.write(f'{header_text}\n\n')
        for op_type, data in sorted(results.items()):
            batch_size = data['batch_size']
            entries = data['entries']
            output_file.write(f'[{op_type}] - Batch Size: {batch_size}\n\n')
            columns = FileManager._get_active_columns(entries)
            col_widths = FileManager._calculate_column_widths(entries, columns)
            header_row = '  '.join((col.ljust(col_widths[col]) for col in columns))
            output_file.write(header_row + '\n')
            separator = '  '.join(('-' * col_widths[col] for col in columns))
            output_file.write(separator + '\n')
            for entry in entries:
                row = '  '.join((str(entry.get(col, '')).ljust(col_widths[col]) for col in columns))
                output_file.write(row + '\n')
            output_file.write('\n')
@dataclass
class Scenario:
    name: str
    database_size: str
    relationships: str
    concurrency: int
    cache_enabled: bool
    iterations: int
@dataclass
class ResultRow:
    backend: str
    operation: str
    batch_size: int
    median_ms: float
    min_ms: float
    max_ms: float
    p50_ms: float
    p90_ms: float
    p99_ms: float
    total_time_ms: float
    time_per_atom_ms: float
    throughput: float
    def to_tuple(self) -> tuple:
        return (self.backend, self.operation, self.batch_size, self.median_ms, self.min_ms, self.max_ms, self.p50_ms, self.p90_ms, self.p99_ms, self.total_time_ms, self.time_per_atom_ms, self.throughput)
class DatabaseManager:
    def __init__(self, db_name: str):
        self.db_name = db_name
    @contextmanager
    def cursor(self) -> Generator[sqlite3.Cursor, None, None]:
        conn = sqlite3.connect(self.db_name)
        try:
            cursor = conn.cursor()
            yield cursor
            conn.commit()
        except sqlite3.Error as e:
            print(f'SQLite error: {e}')
            conn.rollback()
            raise
        finally:
            conn.close()
    def create_tables(self):
        with self.cursor() as cursor:
            cursor.execute('\n                CREATE TABLE IF NOT EXISTS benchmark_type (\n                    id INTEGER PRIMARY KEY AUTOINCREMENT,\n                    name TEXT NOT NULL UNIQUE\n                );\n            ')
            cursor.execute('\n                CREATE TABLE IF NOT EXISTS benchmark_scenario (\n                    id INTEGER PRIMARY KEY AUTOINCREMENT,\n                    scenario_name TEXT UNIQUE,\n                    database_size TEXT,\n                    atoms_relationships TEXT,\n                    concurrent_access INTEGER,\n                    cache_enabled BOOLEAN,\n                    iterations INTEGER\n                );\n            ')
            cursor.execute("\n                CREATE TABLE IF NOT EXISTS benchmark_execution (\n                    id INTEGER PRIMARY KEY AUTOINCREMENT,\n                    benchmark_type_id INTEGER NOT NULL,\n                    benchmark_scenario_id INTEGER NOT NULL,\n                    execution_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,\n                    status TEXT NOT NULL CHECK (status IN ('IN_PROGRESS', 'COMPLETED', 'FAILED')),\n                    pr_execution_type BOOLEAN NOT NULL DEFAULT 0,\n                    pr_link TEXT,\n                    pr_title TEXT,\n                    completed_at TEXT,\n                    FOREIGN KEY (benchmark_type_id) REFERENCES benchmark_type(id),\n                    FOREIGN KEY (benchmark_scenario_id) REFERENCES benchmark_scenario(id)\n                );\n            ")
            cursor.execute('\n                CREATE TABLE IF NOT EXISTS benchmark_result (\n                    id INTEGER PRIMARY KEY AUTOINCREMENT,\n                    benchmark_execution_id INTEGER NOT NULL,\n                    backend TEXT NOT NULL,\n                    operation TEXT NOT NULL,\n                    batch_size INTEGER NOT NULL,\n                    median_operation_time_ms REAL NOT NULL,\n                    min_operation_time_ms REAL NOT NULL,\n                    max_operation_time_ms REAL NOT NULL,\n                    p50_operation_time_ms REAL NOT NULL,\n                    p90_operation_time_ms REAL NOT NULL,\n                    p99_operation_time_ms REAL NOT NULL,\n                    total_time_ms REAL NOT NULL,\n                    time_per_atom_ms REAL NOT NULL,\n                    throughput REAL NOT NULL,\n                    FOREIGN KEY (benchmark_execution_id) REFERENCES benchmark_execution(id)\n                );\n            ')
    def get_or_create_ids(self, benchmark_type: str, scenario: Scenario) -> tuple[int, int]:
        with self.cursor() as cursor:
            cursor.execute('SELECT id FROM benchmark_type WHERE name = ?', (benchmark_type,))
            row = cursor.fetchone()
            if row:
                type_id = row[0]
            else:
                cursor.execute('INSERT INTO benchmark_type (name) VALUES (?)', (benchmark_type,))
                type_id = cursor.lastrowid
            cursor.execute('SELECT id FROM benchmark_scenario WHERE scenario_name = ?', (scenario.name,))
            row = cursor.fetchone()
            if row:
                scenario_id = row[0]
            else:
                cursor.execute('\n                    INSERT INTO benchmark_scenario (scenario_name, database_size, atoms_relationships, concurrent_access, cache_enabled, iterations)\n                    VALUES (?, ?, ?, ?, ?, ?)\n                ', (scenario.name, scenario.database_size, scenario.relationships, scenario.concurrency, scenario.cache_enabled, scenario.iterations))
                scenario_id = cursor.lastrowid
        return (type_id, scenario_id)
    def insert_results(self, type_id: int, scenario_id: int, results_data: list[ResultRow]) -> int:
        with self.cursor() as cursor:
            cursor.execute("\n                INSERT INTO benchmark_execution (benchmark_type_id, benchmark_scenario_id, status)\n                VALUES (?, ?, 'IN_PROGRESS')\n            ", (type_id, scenario_id))
            execution_id = cursor.lastrowid
            records_to_insert = [(execution_id, *result.to_tuple()) for result in results_data]
            cursor.executemany('\n                INSERT INTO benchmark_result (\n                    benchmark_execution_id, backend, operation, batch_size,\n                    median_operation_time_ms, min_operation_time_ms, max_operation_time_ms,\n                    p50_operation_time_ms, p90_operation_time_ms, p99_operation_time_ms,\n                    total_time_ms, time_per_atom_ms, throughput\n                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)\n            ', records_to_insert)
        return execution_id
def parse_benchmark_file(filepath: str) -> BenchmarkData:
    with open(filepath, 'r') as f:
        lines = [line.strip() for line in f if line.strip()]
    header: Optional[list[str]] = None
    for i, line in enumerate(lines):
        if '|' in line and 'Operation' in line:
            header = [h.strip() for h in line.split('|') if h.strip()]
            data_lines = lines[i + 1:]
            break
    else:
        return []
    results: BenchmarkData = []
    for line in data_lines:
        is_separator = all((c in '-| ' for c in line))
        if '|' in line and (not is_separator):
            values = [v.strip() for v in line.split('|') if v.strip()]
            if len(values) == len(header):
                entry = dict(zip(header, values))
                results.append(entry)
    return results
def extract_metadata_from_filename(filename: str) -> Optional[tuple[str, str, str, str, str]]:
    pattern = '^(.*?)_(morkdb|redismongodb)_([A-Za-z0-9]+)_([A-Za-z0-9_]+)_([0-9]+)\\.txt$'
    match = re.match(pattern, filename)
    if match:
        benchmark_type, backend, op_type, method, batch_size = match.groups()
        return (benchmark_type, backend, op_type, method, batch_size)
    return None
def consolidate_results_from_directory(benchmark_type: str, directory: str) -> ConsolidatedResults:
    results: ConsolidatedResults = defaultdict(lambda: {'entries': [], 'batch_size': 'N/A'})
    for filename in sorted(os.listdir(directory)):
        if not (filename.startswith(f'{benchmark_type}_') and filename.endswith('.txt')):
            continue
        metadata = extract_metadata_from_filename(filename)
        if not metadata:
            print(f'Warning: Could not parse metadata from filename: {filename}', file=sys.stderr)
            continue
        _, backend, op_type, _, batch_size = metadata
        filepath = os.path.join(directory, filename)
        parsed_data = parse_benchmark_file(filepath)
        if not parsed_data:
            print(f'Warning: No data parsed from file: {filepath}', file=sys.stderr)
            continue
        results[op_type]['batch_size'] = batch_size
        for entry in parsed_data:
            entry['Backend'] = backend
            results[op_type]['entries'].append(entry)
    return results
def parse_scenario_string(scenario_str: str) -> Scenario:
    parts = scenario_str.split()
    if len(parts) != 6:
        raise ValueError('Scenario string must contain exactly 6 parts: name, database, relationships, concurrency, cache, iterations.')
    name, db_size, rels, conc, cache, iters = parts
    return Scenario(name=name, database_size=db_size, relationships=rels, concurrency=int(conc), cache_enabled=cache.lower() == 'enabled', iterations=int(iters))
def prepare_results_for_db(results: ConsolidatedResults) -> list[ResultRow]:
    db_rows = []
    for _, data in results.items():
        batch_size = int(data['batch_size'])
        for entry in data['entries']:
            try:
                row = ResultRow(backend=entry['Backend'], operation=entry['Operation'], batch_size=batch_size, median_ms=float(entry['MED']), min_ms=float(entry['MIN']), max_ms=float(entry['MAX']), p50_ms=float(entry['P50']), p90_ms=float(entry['P90']), p99_ms=float(entry['P99']), total_time_ms=float(entry['TT']), time_per_atom_ms=float(entry['TPA']), throughput=float(entry['TP']))
                db_rows.append(row)
            except (KeyError, ValueError) as e:
                print(f'Warning: Skipping row due to missing/invalid data: {entry}. Error: {e}', file=sys.stderr)
    return db_rows
def main():
    parser = argparse.ArgumentParser(description='Consolidate and store benchmark results.')
    parser.add_argument('directory', help='Directory with benchmark files.')
    parser.add_argument('--scenario', required=True, help='Test scenario data string.')
    parser.add_argument('--type', required=True, help="Benchmark type (e.g., 'atomdb').")
    parser.add_argument('--db-path', required=False, default='./benchmark.db', help='Path where the benchmark database will be saved.')
    parser.add_argument('--output-file', required=False, default=None, help='Path to save the consolidated report (if not specified, no report will be generated)')
    parser.add_argument('--header-text', required=False, default='', help='Header text for the report.')
    args = parser.parse_args()
    print(f'Consolidating results from: {args.directory}')
    results = consolidate_results_from_directory(args.type, args.directory)
    if not results:
        print('No valid benchmark data found. Exiting.')
        return
    if args.output_file:
        with open(args.output_file, 'w') as report_file:
            FileManager.write_report(results, report_file, header_text=args.header_text)
        print(f'Consolidated report saved to: {args.output_file}')
    db_name = os.path.abspath(args.db_path)
    dir_name = os.path.dirname(db_name)
    if not os.path.exists(dir_name):
        print(f'Creating output directory: {dir_name}')
        os.makedirs(dir_name, exist_ok=True)
    print(f'Using database: {db_name}')
    db = DatabaseManager(db_name)
    db.create_tables()
    try:
        scenario = parse_scenario_string(args.scenario)
    except ValueError as e:
        print(f'Error: Invalid scenario string. {e}', file=sys.stderr)
        sys.exit(1)
    type_id, scenario_id = db.get_or_create_ids(args.type, scenario)
    results_for_db = prepare_results_for_db(results)
    if not results_for_db:
        print('No valid results to insert into the database. Exiting.')
        return
    try:
        execution_id = db.insert_results(type_id, scenario_id, results_for_db)
        print(f'Successfully loaded {len(results_for_db)} results into the database.')
        print(f'New execution ID: {execution_id}')
    except sqlite3.Error as e:
        print(f'Failed to insert data into the database. Error: {e}', file=sys.stderr)
        sys.exit(1)
if __name__ == '__main__':
    main()