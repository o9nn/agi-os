#!/usr/bin/env python3
"""
Universal Batch Minification Script for Smol Protocol
Processes all code files in a directory, applying language-specific minification
"""

import os
import sys
import ast
import re
import subprocess
from pathlib import Path
from concurrent.futures import ProcessPoolExecutor, as_completed
import json

# Configuration
EXCLUDED_DIRS = {'.git', 'node_modules', 'external', 'archive', '__pycache__', '.venv', 'venv'}
EXCLUDED_FILES = {'package-lock.json', 'yarn.lock'}

# Results tracking
results = {
    'processed': 0,
    'optimized': 0,
    'failed': 0,
    'total_original': 0,
    'total_optimized': 0,
    'by_extension': {}
}


def minify_python(content: str) -> str:
    """Minify Python source code by removing docstrings and comments"""
    try:
        tree = ast.parse(content)
        
        # Remove docstrings
        for node in ast.walk(tree):
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef, ast.Module)):
                if (node.body and isinstance(node.body[0], ast.Expr) and 
                    isinstance(node.body[0].value, (ast.Str, ast.Constant))):
                    if isinstance(node.body[0].value, ast.Constant) and isinstance(node.body[0].value.value, str):
                        node.body = node.body[1:]
                    elif isinstance(node.body[0].value, ast.Str):
                        node.body = node.body[1:]
        
        minified = ast.unparse(tree)
        
        # Remove inline comments and empty lines
        lines = []
        for line in minified.split('\n'):
            # Remove inline comments (but preserve # in strings)
            if '#' in line:
                in_string = False
                quote_char = None
                new_line = []
                for i, c in enumerate(line):
                    if c in '"\'':
                        if not in_string:
                            in_string = True
                            quote_char = c
                        elif c == quote_char and (i == 0 or line[i-1] != '\\'):
                            in_string = False
                        new_line.append(c)
                    elif c == '#' and not in_string:
                        break
                    else:
                        new_line.append(c)
                line = ''.join(new_line).rstrip()
            
            if line.strip():
                lines.append(line)
        
        return '\n'.join(lines)
    except:
        return content  # Return original if parsing fails


def minify_c_cpp(content: str) -> str:
    """Minify C/C++ source code by removing comments"""
    # Remove multi-line comments
    content = re.sub(r'/\*.*?\*/', '', content, flags=re.DOTALL)
    # Remove single-line comments
    content = re.sub(r'//.*$', '', content, flags=re.MULTILINE)
    # Remove empty lines and compress whitespace
    lines = [line.strip() for line in content.split('\n') if line.strip()]
    return '\n'.join(lines)


def minify_scheme(content: str) -> str:
    """Minify Scheme/Lisp source code by removing comments"""
    lines = []
    for line in content.split('\n'):
        # Remove comment lines (starting with ;)
        if line.strip().startswith(';'):
            continue
        # Remove inline comments
        if ';' in line:
            # Be careful with strings
            in_string = False
            new_line = []
            for c in line:
                if c == '"':
                    in_string = not in_string
                if c == ';' and not in_string:
                    break
                new_line.append(c)
            line = ''.join(new_line).rstrip()
        if line.strip():
            lines.append(line)
    return '\n'.join(lines)


def minify_shell(content: str) -> str:
    """Minify shell scripts by removing comments"""
    lines = []
    for i, line in enumerate(content.split('\n')):
        # Keep shebang
        if i == 0 and line.startswith('#!'):
            lines.append(line)
            continue
        # Remove comment lines
        if line.strip().startswith('#'):
            continue
        # Remove inline comments (but be careful with strings and special chars)
        if '#' in line and not line.strip().startswith('#'):
            # Simple approach: only remove if # is preceded by whitespace
            parts = line.split('#')
            if len(parts) > 1:
                line = parts[0].rstrip()
        if line.strip():
            lines.append(line)
    return '\n'.join(lines)


def minify_go(content: str) -> str:
    """Minify Go source code by removing comments"""
    # Remove multi-line comments
    content = re.sub(r'/\*.*?\*/', '', content, flags=re.DOTALL)
    # Remove single-line comments
    content = re.sub(r'//.*$', '', content, flags=re.MULTILINE)
    # Remove empty lines
    lines = [line for line in content.split('\n') if line.strip()]
    return '\n'.join(lines)


def minify_javascript(content: str) -> str:
    """Minify JavaScript/TypeScript by removing comments"""
    # Remove multi-line comments
    content = re.sub(r'/\*.*?\*/', '', content, flags=re.DOTALL)
    # Remove single-line comments (but not URLs)
    content = re.sub(r'(?<!:)//.*$', '', content, flags=re.MULTILINE)
    # Remove empty lines
    lines = [line for line in content.split('\n') if line.strip()]
    return '\n'.join(lines)


def minify_ruby(content: str) -> str:
    """Minify Ruby source code by removing comments"""
    lines = []
    in_heredoc = False
    for i, line in enumerate(content.split('\n')):
        # Keep shebang
        if i == 0 and line.startswith('#!'):
            lines.append(line)
            continue
        # Skip comment lines
        if line.strip().startswith('#'):
            continue
        # Remove inline comments
        if '#' in line:
            # Simple approach
            in_string = False
            new_line = []
            for c in line:
                if c in '"\'':
                    in_string = not in_string
                if c == '#' and not in_string:
                    break
                new_line.append(c)
            line = ''.join(new_line).rstrip()
        if line.strip():
            lines.append(line)
    return '\n'.join(lines)


def get_minifier(ext: str):
    """Get the appropriate minifier for a file extension"""
    minifiers = {
        '.py': minify_python,
        '.c': minify_c_cpp,
        '.cpp': minify_c_cpp,
        '.cc': minify_c_cpp,
        '.h': minify_c_cpp,
        '.hpp': minify_c_cpp,
        '.scm': minify_scheme,
        '.lisp': minify_scheme,
        '.el': minify_scheme,
        '.rkt': minify_scheme,
        '.sh': minify_shell,
        '.bash': minify_shell,
        '.go': minify_go,
        '.js': minify_javascript,
        '.ts': minify_javascript,
        '.jsx': minify_javascript,
        '.tsx': minify_javascript,
        '.rb': minify_ruby,
        '.jl': minify_c_cpp,  # Julia uses similar comment style
        '.b': minify_c_cpp,   # Limbo uses similar comment style
        '.zig': minify_c_cpp, # Zig uses similar comment style
    }
    return minifiers.get(ext)


def process_file(filepath: str) -> dict:
    """Process a single file and return results"""
    result = {
        'path': filepath,
        'original_size': 0,
        'optimized_size': 0,
        'saved': 0,
        'status': 'skipped',
        'error': None
    }
    
    try:
        path = Path(filepath)
        ext = path.suffix.lower()
        
        minifier = get_minifier(ext)
        if not minifier:
            result['status'] = 'no_minifier'
            return result
        
        # Read original content
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            original = f.read()
        
        result['original_size'] = len(original.encode('utf-8'))
        
        # Skip very small files (< 100 bytes)
        if result['original_size'] < 100:
            result['status'] = 'too_small'
            return result
        
        # Apply minification
        minified = minifier(original)
        result['optimized_size'] = len(minified.encode('utf-8'))
        result['saved'] = result['original_size'] - result['optimized_size']
        
        # Only write if we saved bytes
        if result['saved'] > 0:
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(minified)
            result['status'] = 'optimized'
        else:
            result['status'] = 'no_savings'
        
        return result
        
    except Exception as e:
        result['status'] = 'error'
        result['error'] = str(e)
        return result


def find_files(root_dir: str, extensions: set) -> list:
    """Find all files with given extensions, excluding certain directories"""
    files = []
    for dirpath, dirnames, filenames in os.walk(root_dir):
        # Skip excluded directories
        dirnames[:] = [d for d in dirnames if d not in EXCLUDED_DIRS]
        
        for filename in filenames:
            if filename in EXCLUDED_FILES:
                continue
            ext = Path(filename).suffix.lower()
            if ext in extensions:
                files.append(os.path.join(dirpath, filename))
    
    return files


def main():
    if len(sys.argv) < 2:
        print("Usage: batch-minify.py <directory> [--dry-run] [--ext=.py,.js,...]")
        sys.exit(1)
    
    root_dir = sys.argv[1]
    dry_run = '--dry-run' in sys.argv
    
    # Parse extensions
    extensions = {'.py', '.c', '.cpp', '.h', '.js', '.ts', '.scm', '.go', '.sh', '.rb', '.jl', '.b'}
    for arg in sys.argv:
        if arg.startswith('--ext='):
            extensions = set(arg[6:].split(','))
    
    print(f"Scanning {root_dir} for files with extensions: {extensions}")
    files = find_files(root_dir, extensions)
    print(f"Found {len(files)} files to process")
    
    if dry_run:
        print("Dry run mode - no files will be modified")
        return
    
    # Process files
    total_original = 0
    total_optimized = 0
    optimized_count = 0
    by_ext = {}
    
    for i, filepath in enumerate(files):
        if i % 100 == 0:
            print(f"Processing {i}/{len(files)}...")
        
        result = process_file(filepath)
        
        if result['status'] == 'optimized':
            total_original += result['original_size']
            total_optimized += result['optimized_size']
            optimized_count += 1
            
            ext = Path(filepath).suffix
            if ext not in by_ext:
                by_ext[ext] = {'original': 0, 'optimized': 0, 'count': 0}
            by_ext[ext]['original'] += result['original_size']
            by_ext[ext]['optimized'] += result['optimized_size']
            by_ext[ext]['count'] += 1
    
    # Print summary
    print("\n" + "="*60)
    print("OPTIMIZATION SUMMARY")
    print("="*60)
    print(f"Files processed: {len(files)}")
    print(f"Files optimized: {optimized_count}")
    print(f"Total original: {total_original:,} bytes")
    print(f"Total optimized: {total_optimized:,} bytes")
    print(f"Total saved: {total_original - total_optimized:,} bytes")
    if total_original > 0:
        print(f"Reduction: {((total_original - total_optimized) / total_original) * 100:.1f}%")
    
    print("\nBy Extension:")
    for ext, data in sorted(by_ext.items()):
        saved = data['original'] - data['optimized']
        pct = (saved / data['original']) * 100 if data['original'] > 0 else 0
        print(f"  {ext}: {data['original']:,} → {data['optimized']:,} ({saved:,} saved, {pct:.1f}%) [{data['count']} files]")


if __name__ == '__main__':
    main()
